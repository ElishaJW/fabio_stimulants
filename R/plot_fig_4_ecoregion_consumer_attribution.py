"""
top20_ecoregion_consumer_attribution.py
───────────────────────────────────────────────────────────────────────────────
For each of the top-20 ecoregions by combined BD impact from stimulant
consumption, attribute the impact to consuming countries (top 10 per ecoregion)
and break it down by stimulant crop.

Method
──────
Consuming-country shares are derived from bd_fp_total_2020.csv, the BD
footprint matrix with dimensions:

    rows : production sectors  (comm_code_area_code, ~26 k rows)
    cols : consuming sectors   (area_code_use_type,  ~1 344 cols)
             where use_type ∈ {food, losses, other, ...}

Columns are first summed over use types so that each consuming country has a
single aggregated value.  The share for consumer C given production row i is:

    share(i, C) = bd_fp[i, C] / Σ_c bd_fp[i, c]

This is CF-independent: the CF depends on the production country (row),
so it cancels when computing the ratio between consuming countries.

Production-country weights per ecoregion are derived by intersecting the
top-20 TEOW ecoregion polygons with country boundaries and extracting zonal
sums from the stimulant-specific production rasters.  The weight of production
country P in ecoregion E for stimulant S is then scaled by P's world-total BD
footprint for S (from bd_fp_total) so that high-output countries count more:

    adjusted_weight(E, P, S) = raster_frac(E, P, S) × world_fp(P, S)
    weight_norm(E, P, S)     = adjusted_weight / Σ_P adjusted_weight

Final attribution:
    consumer_attr(E, consumer) =
        Σ_{S, P} weight_norm(E,P,S) × stim_impact(E,S) × share(P,S,consumer)

Output
──────
top20_ecoregion_consumer_attribution.xlsx  (same directory as this script)
  • Sheet "Consumer_Attribution" — long-format table: one row per
    (ecoregion × consumer_rank), with attributed BD and % share
  • Sheet "Stimulant_Breakdown" — per ecoregion × stimulant × consumer_rank
  • Sheet "Top20_Summary" — totals for the 20 ecoregions
───────────────────────────────────────────────────────────────────────────────
"""

import os
import warnings
import geopandas as gpd
import pandas as pd
import numpy as np
import rasterio
import json
from rasterio.features import geometry_mask

# ── Config ────────────────────────────────────────────────────────────────────
PROJECT_DIR = (
    "C:/Users/elishaw/OneDrive - NTNU/BAMBOO-personal/"
    "WP3 - FABIO Development/fabio_stimulants"
)
DATA_DIR = (
    "X:/Eli/projects/fabio_stimulants/results_data/" "bd_footprint_combined/World"
)
TEOW_SHP = "X:/Eli/DATA/shapefiles/TEOW/wwf_terr_ecos.shp"
COUNTRY_SHP = "X:/Eli/DATA/shapefiles/TM_WorldBorders/TM_WORLD_BORDERS-0.3.shp"
PROD_RASTERS = {
    "coffee": "X:/Eli/projects/fabio_stimulants/results_data/rel_production_by_country_rasters/rel_prod_normalized_COFF.tif",
    "cocoa": "X:/Eli/projects/fabio_stimulants/results_data/rel_production_by_country_rasters/rel_prod_normalized_COCO.tif",
    "tea": "X:/Eli/projects/fabio_stimulants/results_data/rel_production_by_country_rasters/rel_prod_normalized_TEA.tif",
    "tobacco": "X:/Eli/projects/fabio_stimulants/results_data/rel_production_by_country_rasters/rel_prod_normalized_TOBAC.tif",
}
BD_FP_CSV = os.path.join(
    PROJECT_DIR, "data/biodiversity footprints/bd_fp_total_2020.csv"
)

STIMS = ["coffee", "cocoa", "tea", "tobacco"]
TOP_N_ECOREGIONS = 20
TOP_N_CONSUMERS = 10
YEAR = 2020

SCRIPT_DIR = os.path.dirname(os.path.abspath(__file__))
OUT = os.path.join(SCRIPT_DIR, "top20_ecoregion_consumer_attribution.xlsx")

# ── 1. Lookup tables ──────────────────────────────────────────────────────────
print("Loading lookup tables...")
items = pd.read_csv(os.path.join(PROJECT_DIR, "inst/items_full.csv"))
regions = pd.read_csv(
    os.path.join(PROJECT_DIR, "inst/regions_fabio.csv"), encoding="latin-1"
)

# Stimulant commodity codes
stim_codes = items[
    (items["comm_group"] == "Coffee, tea, cocoa")
    | ((items["comm_group"] == "Tobacco, rubber") & (items["comm_code"] != "c060"))
]["comm_code"].tolist()

# Map each commodity code to its stimulant crop name
STIM_KEYWORDS = {
    "coffee": ["coffee"],
    "cocoa": ["cocoa", "chocolate"],
    "tea": ["tea", "mate"],
    "tobacco": ["tobacco", "cigar", "cigarette"],
}


def _map_stim(item_name):
    nl = str(item_name).lower()
    for stim, kws in STIM_KEYWORDS.items():
        if any(k in nl for k in kws):
            return stim
    return "other"


stim_code_to_group = (
    items[items["comm_code"].isin(stim_codes)]
    .set_index("comm_code")["item"]
    .apply(_map_stim)
    .to_dict()
)

# Lookup dicts
area_code_to_name = regions.set_index("area_code")["area"].to_dict()
area_code_to_iso3 = regions.set_index("area_code")["iso3c"].to_dict()
iso3_to_area_code = {v: k for k, v in area_code_to_iso3.items()}

# ── 2. Top-20 ecoregions ──────────────────────────────────────────────────────
print("Identifying top-20 ecoregions...")
dfs = []
for stim in STIMS:
    path = f"{DATA_DIR}/world_bd_footprint_combined_{YEAR}_{stim}.gpkg"
    try:
        gdf = gpd.read_file(path)[["ECO_ID", "ECO_NAME", "combined_bd"]]
        gdf = pd.DataFrame(gdf.drop(columns="geometry", errors="ignore"))
        gdf = gdf.rename(columns={"combined_bd": f"{stim}_bd"})
        dfs.append(gdf)
    except Exception as e:
        print(f"  WARNING: {stim}: {e}")

merged = dfs[0]
for df in dfs[1:]:
    merged = merged.merge(df, on=["ECO_ID", "ECO_NAME"], how="outer")

bd_cols = [f"{s}_bd" for s in STIMS if f"{s}_bd" in merged.columns]
merged[bd_cols] = merged[bd_cols].fillna(0)
merged["total_bd"] = merged[bd_cols].sum(axis=1)

top20 = merged.nlargest(TOP_N_ECOREGIONS, "total_bd").reset_index(drop=True)
top20.insert(0, "Rank", range(1, TOP_N_ECOREGIONS + 1))
top20_eco_ids = set(top20["ECO_ID"].tolist())
print(f"  {TOP_N_ECOREGIONS} ecoregions identified.")

# ── 3. Production-country weights per (ecoregion × stimulant) ─────────────────
# weight(E, P, S) = sum(prod_raster_S in E ∩ P) / sum(prod_raster_S in P)
# This captures what fraction of country P's production of S is in ecoregion E.

import shapely as shp

print("Loading spatial data (TEOW + country boundaries)...")
teow = gpd.read_file(TEOW_SHP)[["ECO_ID", "ECO_NAME", "geometry"]]
teow["ECO_ID"] = teow["ECO_ID"].astype(int)
countries_shp = gpd.read_file(COUNTRY_SHP)[["ISO3", "NAME", "geometry"]]

if teow.crs != countries_shp.crs:
    countries_shp = countries_shp.to_crs(teow.crs)

POLY_TYPES = {"Polygon", "MultiPolygon"}


def _to_shp(geoseries):
    """Return a plain Python list of native Shapely geometry objects.
    Converts via WKB to avoid geopandas returning old-style wrappers."""
    wkbs = geoseries.to_wkb()
    return [shp.from_wkb(b) if b is not None else None for b in wkbs]


def _validate_geoms(gdf):
    """make_valid + drop nulls/empties.  Keeps MultiPolygons intact (no explode).
    Used for country totals where we want one geometry per country."""
    gdf = gdf.copy()
    geoms = _to_shp(gdf.geometry)
    geoms = [shp.make_valid(g) if g is not None else None for g in geoms]
    mask = [g is not None and not shp.is_empty(g) for g in geoms]
    gdf = gdf[mask].copy()
    geoms = [g for g, m in zip(geoms, mask) if m]
    gdf["geometry"] = gpd.GeoSeries(geoms, index=gdf.index, crs=gdf.crs)
    return gdf


def _clean_polys(gdf):
    """Repair + explode to individual Polygon pieces (no MultiPolygons).
    Used for intersection so geopandas handles each piece separately."""
    gdf = _validate_geoms(gdf)
    gdf = gdf.explode(index_parts=False).reset_index(drop=True)
    gdf = gdf[gdf.geometry.geom_type.isin(POLY_TYPES)].copy()
    return gdf


teow = _clean_polys(teow)

# Two versions of countries:
#  - countries_shp : exploded Polygons — used for pairwise intersection with TEOW
#  - countries_tot : one row per country (MultiPolygons intact) — used for raster totals
countries_shp = _clean_polys(countries_shp)
countries_tot = _validate_geoms(gpd.read_file(COUNTRY_SHP)[["ISO3", "geometry"]])
if countries_tot.crs != teow.crs:
    countries_tot = countries_tot.to_crs(teow.crs)
print(
    f"  {len(countries_shp)} exploded country polygons, {len(countries_tot)} unique country geometries."
)

# No dissolve needed — keep individual TEOW polygon pieces for the top-20
# ecoregions and aggregate zonal stats by ECO_ID after extraction.
teow_top20 = teow[teow["ECO_ID"].isin(top20_eco_ids)].copy().reset_index(drop=True)

# ── Pairwise intersection using shapely directly ──────────────────────────────
# Bypasses gpd.overlay entirely so we never touch create_collection / union_all.
print("Computing ecoregion × country intersections (pairwise shapely)...")
eco_geoms = _to_shp(teow_top20.geometry)
eco_ids = teow_top20["ECO_ID"].tolist()
ctry_geoms = _to_shp(countries_shp.geometry)
ctry_iso3s = countries_shp["ISO3"].tolist()

inter_rows = []
for i, (eco_g, eco_id) in enumerate(zip(eco_geoms, eco_ids)):
    if eco_g is None or shp.is_empty(eco_g):
        continue
    for j, (ctry_g, iso3) in enumerate(zip(ctry_geoms, ctry_iso3s)):
        if ctry_g is None or shp.is_empty(ctry_g):
            continue
        if not shp.intersects(eco_g, ctry_g):
            continue
        inter = shp.intersection(eco_g, ctry_g)
        if inter is None or shp.is_empty(inter):
            continue
        # Discard degenerate results (lines, points)
        type_id = shp.get_type_id(inter)
        if type_id not in (3, 6):  # 3=Polygon, 6=MultiPolygon
            continue
        if shp.area(inter) <= 0:
            continue
        inter_rows.append({"ECO_ID": eco_id, "ISO3": iso3, "geometry": inter})

intersected = gpd.GeoDataFrame(inter_rows, crs=teow.crs)
print(f"  {len(intersected)} non-empty intersection polygons.")

# Build ECO_ID → sorted list of ISO3 codes for Y-axis labels
eco_to_countries = (
    intersected.groupby("ECO_ID")["ISO3"]
    .apply(lambda s: sorted(s.unique().tolist()))
    .to_dict()
)

# ── Zonal sum helper (rasterio-native, no rasterstats) ───────────────────────
# geometry_mask takes plain GeoJSON dicts — never calls shapely ufuncs.


def _geom_to_geojson(geom):
    """Return GeoJSON dict for a single shapely geometry."""
    return json.loads(shp.to_geojson(geom))


def _batch_zonal_sum(geoms, arr, transform):
    """Sum raster values inside each geometry.  Uses rasterio.features
    geometry_mask on the full loaded array — avoids rasterstats and shapely
    collection ufuncs entirely."""
    height, width = arr.shape
    results = []
    for geom in geoms:
        if geom is None or shp.is_empty(geom):
            results.append(0.0)
            continue
        try:
            geoj = _geom_to_geojson(geom)
            mask = geometry_mask(
                [geoj],
                out_shape=(height, width),
                transform=transform,
                invert=True,  # True = inside geometry
            )
            vals = arr[mask]
            valid = vals[~np.isnan(vals)]
            results.append(float(valid.sum()) if len(valid) > 0 else 0.0)
        except Exception:
            results.append(0.0)
    return results


# Pre-extract geometry lists (native shapely objects from WKB round-trip)
inter_geoms = _to_shp(intersected.geometry)
ctry_geoms_tot = _to_shp(countries_tot.geometry)  # one geometry per country (~190)

# ── Zonal stats per stimulant ─────────────────────────────────────────────────
print("Extracting production raster sums per (ecoregion × country)...")
prod_weights = {}  # {stim: DataFrame[ECO_ID, ISO3, raster_frac]}

for stim, raster_path in PROD_RASTERS.items():
    print(f"  {stim}...", flush=True)
    try:
        with rasterio.open(raster_path) as src:
            affine = src.transform
            nodata = src.nodata
            arr = src.read(1).astype(float)
            print(f"    raster shape: {arr.shape}", flush=True)
            if nodata is not None:
                arr[arr == nodata] = np.nan

        print(
            f"    computing intersection sums ({len(inter_geoms)} polygons)...",
            flush=True,
        )
        intersected[f"{stim}_sum"] = _batch_zonal_sum(inter_geoms, arr, affine)

        # Aggregate intersection sums by (ECO_ID, ISO3).
        # No country-total normalisation needed: the raw raster sum is used as a
        # spatial weight and is later scaled by world_fp (total BD footprint of
        # that production country) in the attribution step.  Since rasters are
        # normalised per-country, dividing by the country total would cancel
        # with the world_fp scaling, leaving the same normalised result.
        inter_agg = (
            intersected.groupby(["ECO_ID", "ISO3"])[f"{stim}_sum"]
            .sum()
            .reset_index()
            .rename(columns={f"{stim}_sum": "raster_sum"})
        )

        prod_weights[stim] = inter_agg[inter_agg["raster_sum"] > 0].reset_index(
            drop=True
        )
        print(
            f"    {len(prod_weights[stim])} pairs with non-zero raster sum.", flush=True
        )

    except BaseException as e:
        import traceback

        print(f"WARNING [{type(e).__name__}]: {e}", flush=True)
        traceback.print_exc()
        prod_weights[stim] = pd.DataFrame(columns=["ECO_ID", "ISO3", "raster_frac"])

# ── 4. Consuming-country shares from bd_fp_total ──────────────────────────────
# Columns in the CSV are "area_code_use_type" (e.g. "10_food", "10_losses").
# Sum over use types so each consuming country has one aggregated value.

print("Loading BD footprint matrix (this may take a moment)...")
bd_fp = pd.read_csv(BD_FP_CSV, index_col=0)
print(f"  Loaded: {bd_fp.shape[0]} rows × {bd_fp.shape[1]} cols")

# Parse row index → comm_code, prod_area_code  (format: "c045_10")
idx_split = bd_fp.index.to_series().str.rsplit("_", n=1, expand=True)
idx_split.columns = ["comm_code", "prod_area_code"]
bd_fp = bd_fp.assign(**idx_split)

# Filter to stimulant rows only
bd_fp_stim = bd_fp[bd_fp["comm_code"].isin(stim_codes)].copy()
bd_fp_stim["stim_group"] = bd_fp_stim["comm_code"].map(stim_code_to_group)
bd_fp_stim["prod_iso3"] = pd.to_numeric(
    bd_fp_stim["prod_area_code"], errors="coerce"
).map(area_code_to_iso3)
print(f"  Stimulant rows: {len(bd_fp_stim)}")

# Data columns: everything except the metadata columns appended above
meta_cols = ["comm_code", "prod_area_code", "stim_group", "prod_iso3"]
data_cols = [c for c in bd_fp_stim.columns if c not in meta_cols]


# Sum data columns by consuming area_code (strip use-type suffix)
# col format: "area_code_use_type"  →  area_code = everything before last "_"
def _col_to_area(c):
    return c.rsplit("_", 1)[0]


col_area_map = {c: _col_to_area(c) for c in data_cols}
unique_cons_areas = sorted(
    set(col_area_map.values()), key=lambda x: int(x) if x.isdigit() else 0
)

print("  Aggregating by consuming country...")
# Build a mapping from consuming-area-code to its columns
area_to_cols = {}
for c, area in col_area_map.items():
    area_to_cols.setdefault(area, []).append(c)

# Sum across use types per consuming country — build all columns at once
# with pd.concat to avoid DataFrame fragmentation warnings
area_series = {
    area: bd_fp_stim[cols].sum(axis=1) for area, cols in area_to_cols.items()
}
cons_summed = pd.concat(area_series, axis=1)
cons_summed.index = bd_fp_stim.index

# Attach metadata
cons_summed["stim_group"] = bd_fp_stim["stim_group"].values
cons_summed["prod_iso3"] = bd_fp_stim["prod_iso3"].values
cons_summed["comm_code"] = bd_fp_stim["comm_code"].values
cons_summed["prod_area_code"] = bd_fp_stim["prod_area_code"].values

# Melt to long format and filter zeros
cons_long = cons_summed.melt(
    id_vars=["stim_group", "prod_iso3", "comm_code"],
    value_vars=unique_cons_areas,
    var_name="cons_area_code",
    value_name="bd_value",
)
cons_long = cons_long[cons_long["bd_value"] > 0].copy()

# Add consuming country name
cons_long["cons_area_int"] = pd.to_numeric(cons_long["cons_area_code"], errors="coerce")
cons_long["cons_country"] = cons_long["cons_area_int"].map(area_code_to_name)

# World-total BD footprint per (prod_iso3, stim_group) — used to scale raster_frac
world_fp = (
    cons_long.groupby(["prod_iso3", "stim_group"])["bd_value"]
    .sum()
    .reset_index(name="world_fp")
)

# Consuming-country shares per (prod_iso3, stim_group)
cons_agg = (
    cons_long.groupby(["prod_iso3", "stim_group", "cons_country"])["bd_value"]
    .sum()
    .reset_index(name="bd_cons")
)
cons_agg = cons_agg.merge(world_fp, on=["prod_iso3", "stim_group"], how="left")
cons_agg["share"] = np.where(
    cons_agg["world_fp"] > 0,
    cons_agg["bd_cons"] / cons_agg["world_fp"],
    0.0,
)
print(f"  Consumer-share rows: {len(cons_agg)}")

# ── 5. Combine: attribution per ecoregion ────────────────────────────────────
print("Computing consuming-country attribution for each ecoregion...")

all_results = []  # overall consumer attribution
stim_results = []  # per-stimulant consumer attribution

for _, eco_row in top20.iterrows():
    eco_id = int(eco_row["ECO_ID"])
    eco_name = eco_row["ECO_NAME"]
    rank = int(eco_row["Rank"])
    total_bd = eco_row["total_bd"]

    # Accumulate attributed BD per (stimulant, consumer) pair
    stim_consumer_bd = {}  # {(stim, cons_country): attributed_bd}

    for stim in STIMS:
        stim_impact = eco_row.get(f"{stim}_bd", 0.0)
        if stim_impact <= 0 or stim not in prod_weights:
            continue

        # Production weights for this ecoregion and stimulant
        w_df = prod_weights[stim]
        w_eco = w_df[w_df["ECO_ID"] == eco_id].copy()
        if len(w_eco) == 0:
            continue

        # Scale raster_sum by world BD footprint of each production country.
        # adj_weight = raster_sum(E ∩ P) × world_fp(P, S)
        # This weights each production country by (a) how much of its crop is
        # spatially in this ecoregion and (b) how large its total global BD
        # footprint is, giving a production-impact proportional weight.
        wfp = world_fp[world_fp["stim_group"] == stim]
        w_eco = w_eco.merge(
            wfp[["prod_iso3", "world_fp"]],
            left_on="ISO3",
            right_on="prod_iso3",
            how="left",
        )
        w_eco["world_fp"] = w_eco["world_fp"].fillna(0)
        w_eco["adj_weight"] = w_eco["raster_sum"] * w_eco["world_fp"]

        total_adj = w_eco["adj_weight"].sum()
        if total_adj <= 0:
            # Fall back to raw raster sum as weight
            rsum_total = w_eco["raster_sum"].sum()
            w_eco["weight_norm"] = (
                w_eco["raster_sum"] / rsum_total if rsum_total > 0 else 1.0 / len(w_eco)
            )
        else:
            w_eco["weight_norm"] = w_eco["adj_weight"] / total_adj

        # For each production country, apply consuming-country shares
        stim_cons_shares = cons_agg[cons_agg["stim_group"] == stim]

        for _, w_row in w_eco.iterrows():
            prod_iso3 = w_row["ISO3"]
            wt = w_row["weight_norm"]
            shares = stim_cons_shares[stim_cons_shares["prod_iso3"] == prod_iso3]

            for _, s_row in shares.iterrows():
                cons = s_row["cons_country"]
                if pd.isna(cons):
                    continue
                contribution = wt * stim_impact * s_row["share"]
                key = (stim, cons)
                stim_consumer_bd[key] = stim_consumer_bd.get(key, 0.0) + contribution

    # Aggregate across stimulants for overall consumer ranking
    overall = {}
    for (stim, cons), val in stim_consumer_bd.items():
        overall[cons] = overall.get(cons, 0.0) + val

    sorted_overall = sorted(overall.items(), key=lambda x: x[1], reverse=True)
    for c_rank, (cons, attributed) in enumerate(sorted_overall[:TOP_N_CONSUMERS], 1):
        all_results.append(
            {
                "Eco_Rank": rank,
                "ECO_ID": eco_id,
                "ECO_NAME": eco_name,
                "Total_BD": total_bd,
                "Consumer_Rank": c_rank,
                "Consuming_Country": cons,
                "Attributed_BD": attributed,
                "Pct_of_Eco_Total": attributed / total_bd * 100 if total_bd > 0 else 0,
            }
        )

    # Per-stimulant ranking
    for stim in STIMS:
        stim_impact = eco_row.get(f"{stim}_bd", 0.0)
        stim_dict = {
            cons: val for (s, cons), val in stim_consumer_bd.items() if s == stim
        }
        sorted_stim = sorted(stim_dict.items(), key=lambda x: x[1], reverse=True)
        for c_rank, (cons, attributed) in enumerate(sorted_stim[:TOP_N_CONSUMERS], 1):
            stim_results.append(
                {
                    "Eco_Rank": rank,
                    "ECO_ID": eco_id,
                    "ECO_NAME": eco_name,
                    "Stimulant": stim,
                    "Stim_BD": stim_impact,
                    "Consumer_Rank": c_rank,
                    "Consuming_Country": cons,
                    "Attributed_BD": attributed,
                    "Pct_of_Stim_BD": (
                        attributed / stim_impact * 100 if stim_impact > 0 else 0
                    ),
                }
            )

results_df = pd.DataFrame(all_results)
stim_results_df = pd.DataFrame(stim_results)

# ── 6. Top-20 summary with commodity shares ───────────────────────────────────
summary = top20[["Rank", "ECO_ID", "ECO_NAME"] + bd_cols + ["total_bd"]].copy()
for stim in STIMS:
    col = f"{stim}_bd"
    if col in summary.columns:
        summary[f"{stim}_pct"] = summary[col] / summary["total_bd"] * 100

# ── 7. Write Excel ────────────────────────────────────────────────────────────
print(f"Writing output: {OUT}", flush=True)
print(f"  Consumer_Attribution: {len(results_df)} rows", flush=True)
print(f"  Stimulant_Breakdown:  {len(stim_results_df)} rows", flush=True)
print(f"  Top20_Summary:        {len(summary)} rows", flush=True)

# Dump CSVs first as a safe fallback regardless of Excel outcome
csv_base = OUT.replace(".xlsx", "")
results_df.to_csv(csv_base + "_consumer_attribution.csv", index=False)
stim_results_df.to_csv(csv_base + "_stimulant_breakdown.csv", index=False)
summary.to_csv(csv_base + "_top20_summary.csv", index=False)
print("  CSVs written successfully.", flush=True)

try:
    # Remove any stale 0-byte file left by a previous failed run
    if os.path.exists(OUT) and os.path.getsize(OUT) == 0:
        os.remove(OUT)
        print("  Removed stale 0-byte output file.", flush=True)

    print("  Writing Excel sheets (xlsxwriter)...", flush=True)
    with pd.ExcelWriter(OUT, engine="xlsxwriter") as writer:
        wb_xls = writer.book
        hdr_fmt = wb_xls.add_format(
            {
                "bold": True,
                "font_color": "FFFFFF",
                "bg_color": "2F4F8F",
                "align": "center",
            }
        )
        sci_fmt = wb_xls.add_format({"num_format": "0.000E+00"})
        pct_fmt = wb_xls.add_format({"num_format": "0.00"})

        for df, sheet_name in [
            (results_df, "Consumer_Attribution"),
            (stim_results_df, "Stimulant_Breakdown"),
            (summary, "Top20_Summary"),
        ]:
            print(f"    writing {sheet_name}...", flush=True)
            df.to_excel(writer, sheet_name=sheet_name, index=False, startrow=0)
            ws = writer.sheets[sheet_name]
            ws.freeze_panes(1, 0)
            # Header formatting
            for col_idx, col_name in enumerate(df.columns):
                ws.write(0, col_idx, col_name, hdr_fmt)
            # Column widths
            for col_idx, col_name in enumerate(df.columns):
                max_len = max(
                    len(str(col_name)),
                    df[col_name].astype(str).str.len().max() if len(df) > 0 else 0,
                )
                ws.set_column(col_idx, col_idx, min(int(max_len) + 3, 45))

    print(f"\nDone.  Output: {OUT}", flush=True)

except BaseException as e:
    import traceback

    print(f"\nERROR during Excel write: {type(e).__name__}: {e}", flush=True)
    traceback.print_exc()

# ══════════════════════════════════════════════════════════════════════════════
# ── 8. Combined multi-panel figure ───────────────────────────────────────────
# Panels a, b, c share the same Y-axis (top-20 ecoregions):
#   a  — horizontal stacked bars: absolute BD impact subdivided by stimulant
#   b  — heatmap: share of total BD impact by pathway (land use / freshwater)
#   c  — horizontal stacked bars: % of total BD impact by consuming country
# ══════════════════════════════════════════════════════════════════════════════
print("\nGenerating combined figure...", flush=True)

import matplotlib.pyplot as plt
import matplotlib.patches as mpatches
import matplotlib.ticker as ticker
import matplotlib.gridspec as gridspec
from matplotlib.colors import LinearSegmentedColormap

FIG_DIR = os.path.join(PROJECT_DIR, "figures")
os.makedirs(FIG_DIR, exist_ok=True)
OUT_FIG_BASE = os.path.join(FIG_DIR, "top20_ecoregion_consumer_attribution_figure_2020")

# ── Re-build the top-20 data with pathway shares ──────────────────────────────
# (mirrors top20_ecoregion_stressor_attribution.py)
print("  Loading stressor-level gpkg files for pathway shares...", flush=True)

landuse_cols_plot, fw_cols_plot = [], []
top20_plot = top20.copy()  # already has Rank, ECO_ID, ECO_NAME, stim_bd cols, total_bd

for stim in STIMS:
    path = f"{DATA_DIR}/world_bd_footprint_combined_2020_{stim}.gpkg"
    try:
        gdf = gpd.read_file(path)
        gdf = pd.DataFrame(gdf.drop(columns="geometry", errors="ignore"))
        for src_col, dst_col in [
            ("landuse_bd", f"{stim}_landuse"),
            ("fw_total_eco", f"{stim}_fw"),
        ]:
            if src_col in gdf.columns:
                top20_plot = top20_plot.merge(
                    gdf[["ECO_ID", src_col]].rename(columns={src_col: dst_col}),
                    on="ECO_ID",
                    how="left",
                )
                top20_plot[dst_col] = top20_plot[dst_col].fillna(0)
                if src_col == "landuse_bd":
                    landuse_cols_plot.append(dst_col)
                else:
                    fw_cols_plot.append(dst_col)
    except Exception as e:
        print(f"    WARNING: {stim} gpkg: {e}", flush=True)

top20_plot["total_landuse"] = top20_plot[
    [c for c in landuse_cols_plot if c in top20_plot.columns]
].sum(axis=1)
top20_plot["total_fw"] = top20_plot[
    [c for c in fw_cols_plot if c in top20_plot.columns]
].sum(axis=1)

# ── Extract N and P BD rasters to top-20 ecoregions to split fw into
#    water use vs. eutrophication (N + P combined).
#    Uses the same _batch_zonal_sum helper already defined above.
print(
    "  Extracting N/P rasters to top-20 ecoregions for stressor breakdown...",
    flush=True,
)
NP_DIR = "X:/Eli/projects/fabio_stimulants/results_data/bd_footprint_by_np_scale"

from shapely.ops import unary_union

# Dissolve exploded teow polygons to one geometry per ECO_ID for extraction
dissolved = teow_top20.groupby("ECO_ID")["geometry"].apply(unary_union).reset_index()
teow_top20_diss = gpd.GeoDataFrame(dissolved, geometry="geometry", crs=teow_top20.crs)
eco_id_order_np = top20_plot["ECO_ID"].tolist()
teow_top20_diss = (
    teow_top20_diss.set_index("ECO_ID").reindex(eco_id_order_np).reset_index()
)
eco_geoms_np = _to_shp(teow_top20_diss.geometry)

np_eco_totals = {eco_id: 0.0 for eco_id in eco_id_order_np}
for stim in STIMS:
    for stype in ["n", "p"]:
        rpath = f"{NP_DIR}/world_bd_footprint_{stype}_{YEAR}_{stim}.tif"
        if not os.path.exists(rpath):
            print(f"    missing: {os.path.basename(rpath)}", flush=True)
            continue
        try:
            with rasterio.open(rpath) as src:
                arr = src.read(1).astype(float)
                nodata = src.nodata
                if nodata is not None:
                    arr[arr == nodata] = np.nan
                aff = src.transform
            sums = _batch_zonal_sum(eco_geoms_np, arr, aff)
            for eco_id, val in zip(eco_id_order_np, sums):
                np_eco_totals[eco_id] += val
        except Exception as e:
            print(f"    WARNING [{stim}/{stype}]: {e}", flush=True)

top20_plot["total_eutroph"] = [np_eco_totals[eid] for eid in top20_plot["ECO_ID"]]
top20_plot["total_water"] = (top20_plot["total_fw"] - top20_plot["total_eutroph"]).clip(
    lower=0
)

denom = top20_plot["total_bd"].replace(0, np.nan)
top20_plot["lu_share"] = (top20_plot["total_landuse"] / 2) / denom
top20_plot["water_share"] = (top20_plot["total_water"] / 2) / denom
top20_plot["eutroph_share"] = (top20_plot["total_eutroph"] / 2) / denom
top20_plot[["lu_share", "water_share", "eutroph_share"]] = top20_plot[
    ["lu_share", "water_share", "eutroph_share"]
].fillna(0)
heatmap_data = top20_plot[["lu_share", "water_share", "eutroph_share"]].values

# ── Consumer attribution matrix ───────────────────────────────────────────────
# Identify top-12 consumers globally (by sum of attributed BD across all ecoregions)
N_NAMED = 12
global_rank = (
    results_df.groupby("Consuming_Country")["Attributed_BD"]
    .sum()
    .sort_values(ascending=False)
)
top_global = global_rank.index[:N_NAMED].tolist()

# Desaturated, low-contrast palette for consuming-country bars
_PALETTE_12 = [
    "#7B9BB8",  # slate blue
    "#82AD90",  # sage green
    "#D4A47A",  # soft ochre-orange
    "#B89AB3",  # dusty mauve
    "#C99090",  # muted rose-red
    "#96C0BC",  # soft teal
    "#AFA8C3",  # pale lavender
    "#B0998C",  # warm taupe
    "#8AB49A",  # soft forest green
    "#C8BB7A",  # pale straw-gold
    "#A8C0CC",  # light slate
    "#C9AAAC",  # rose beige
]
CONSUMER_COLORS = {country: _PALETTE_12[i] for i, country in enumerate(top_global)}
CONSUMER_COLORS["Other"] = "#BDBDBD"  # neutral grey for remainder

# Build consumer matrix: rows = ecoregions (top-20 order), cols = named + Other
# "Other" captures ALL consumers not in the named top-12, i.e. 100% − named_sum,
# so the bars always extend to the full attributed percentage.
eco_id_order = top20_plot["ECO_ID"].tolist()

cons_mat = {}  # {consumer_label: list of pct_of_total per ecoregion}
for label in top_global + ["Other"]:
    cons_mat[label] = []

for eco_id in eco_id_order:
    eco_rows = results_df[results_df["ECO_ID"] == eco_id]
    named_sum = 0.0
    for label in top_global:
        row = eco_rows[eco_rows["Consuming_Country"] == label]
        val = float(row["Pct_of_Eco_Total"].iloc[0]) if len(row) > 0 else 0.0
        cons_mat[label].append(val)
        named_sum += val
    # "Other" = everything not individually named, up to 100% total attribution.
    # This correctly captures all consumers beyond the top-12, not just those
    # stored in results_df (which is limited to TOP_N_CONSUMERS per ecoregion).
    cons_mat["Other"].append(max(0.0, 100.0 - named_sum))

# ── Style constants (mirrors v2 script) ───────────────────────────────────────
STIM_COLORS = {
    "coffee": "#4B2115",
    "cocoa": "#B5541D",
    "tea": "#4A7C59",
    "tobacco": "#C9963B",
}
PATHWAY_LABELS = ["Land use", "Water use", "Eutrophication"]
HEAT_CMAP = LinearSegmentedColormap.from_list("pathway", ["#F5F0E8", "#1A5C6B"])

# ── Figure layout ─────────────────────────────────────────────────────────────
# cols: [a: stacked bars] [b: heatmap] [b_cb: colorbar] [gap] [c: consumer bars]
fig = plt.figure(figsize=(22, 9.5), facecolor="white")

gs = gridspec.GridSpec(
    1,
    5,
    width_ratios=[5.5, 2.8, 0.18, 0.45, 4.2],
    left=0.22,
    right=0.98,
    bottom=0.08,
    top=0.93,
    wspace=0.04,
)
ax_bar = fig.add_subplot(gs[0])  # panel a
ax_heat = fig.add_subplot(gs[1])  # panel b
ax_cb = fig.add_subplot(gs[2])  # colorbar for b
ax_cons = fig.add_subplot(gs[4])  # panel c

n = len(top20_plot)
ypos = np.arange(n - 1, -1, -1)  # rank 1 → ypos 19 (top)

# ── Alternating row shading (shared across all panels) ───────────────────────
for ax in [ax_bar, ax_heat, ax_cons]:
    for i, yp in enumerate(ypos):
        if i % 2 == 0:
            ax.axhspan(yp - 0.34, yp + 0.34, color="#F8F6F2", zorder=0)

# ── Panel a: stimulant stacked bars ──────────────────────────────────────────
lefts_a = np.zeros(n)
for stim in STIMS:
    col = f"{stim}_bd"
    if col not in top20_plot.columns:
        continue
    vals = top20_plot[col].values
    ax_bar.barh(
        ypos,
        vals,
        left=lefts_a,
        height=0.68,
        color=STIM_COLORS[stim],
        linewidth=0,
        label=stim.capitalize(),
    )
    lefts_a += vals


def short_name(name, max_len=30):
    return name if len(name) <= max_len else name[: max_len - 1] + "…"


def _country_tag(eco_id):
    codes = eco_to_countries.get(int(eco_id), [])
    if not codes:
        return ""
    return " [" + ", ".join(codes) + "]"


top20_plot["eco_label"] = top20_plot.apply(
    lambda r: f"#{int(r['Rank']):2d}  {short_name(r['ECO_NAME'])}{_country_tag(r['ECO_ID'])}",
    axis=1,
)

ax_bar.set_yticks(ypos)
ax_bar.set_yticklabels(
    top20_plot["eco_label"].tolist(), fontsize=7.8, fontfamily="monospace"
)
ax_bar.set_xlabel("Biodiversity impact (PDF·yr)", fontsize=9.5, labelpad=6)
ax_bar.xaxis.set_major_formatter(ticker.ScalarFormatter(useMathText=True))
ax_bar.ticklabel_format(style="sci", axis="x", scilimits=(0, 0))
ax_bar.set_xlim(left=0)
ax_bar.set_ylim(-0.5, n - 0.5)
ax_bar.spines[["top", "right", "left"]].set_visible(False)
ax_bar.tick_params(axis="y", length=0, pad=4)
ax_bar.tick_params(axis="x", labelsize=8)
ax_bar.grid(axis="x", color="grey", alpha=0.2, linewidth=0.5, zorder=0)
ax_bar.set_axisbelow(True)

legend_a = [mpatches.Patch(color=STIM_COLORS[s], label=s.capitalize()) for s in STIMS]
ax_bar.legend(
    handles=legend_a,
    title="Stimulant crop",
    title_fontsize=10,
    fontsize=9.5,
    loc="lower right",
    framealpha=0.92,
    edgecolor="#CCCCCC",
    frameon=True,
)

# ── Panel b: pathway share heatmap ───────────────────────────────────────────
n_stressors = heatmap_data.shape[1]  # 3
im = ax_heat.imshow(
    heatmap_data,
    aspect="auto",
    cmap=HEAT_CMAP,
    vmin=0,
    vmax=1,
    interpolation="nearest",
    extent=[-0.5, n_stressors - 0.5, n - 0.5, -0.5],
)
for i in range(n):
    for j in range(n_stressors):
        val = heatmap_data[i, j]
        txt_col = "white" if val > 0.60 else ("#333333" if val > 0.05 else "#AAAAAA")
        ax_heat.text(
            j,
            i,
            f"{val:.0%}",
            ha="center",
            va="center",
            fontsize=7,
            color=txt_col,
            fontweight="bold",
        )

ax_heat.set_xticks(range(n_stressors))
ax_heat.set_xticklabels(PATHWAY_LABELS, fontsize=8, rotation=35, ha="right")
ax_heat.tick_params(axis="x", length=0, pad=2)
ax_heat.set_yticks(np.arange(n))
ax_heat.set_yticklabels([""] * n)
ax_heat.tick_params(axis="y", length=0)
for yp in np.arange(n) - 0.5:
    ax_heat.axhline(yp, color="white", linewidth=0.9)
for xp in np.arange(n_stressors - 1) + 0.5:
    ax_heat.axvline(xp, color="white", linewidth=1.4)
ax_heat.set_ylim(n - 0.5, -0.5)
ax_heat.set_xlim(-0.5, n_stressors - 0.5)
ax_heat.set_title("Stressor\nshare", fontsize=8.5, pad=5, color="#444444")

cb = fig.colorbar(im, cax=ax_cb, orientation="vertical")
cb.set_label("Share of BD impact", fontsize=7.5, labelpad=5)
cb.ax.yaxis.set_major_formatter(ticker.FuncFormatter(lambda x, _: f"{x:.0%}"))
cb.ax.tick_params(labelsize=7)
cb.outline.set_linewidth(0.5)

# ── Panel c: consumer attribution stacked bars ────────────────────────────────
lefts_c = np.zeros(n)
for label in top_global + ["Other"]:
    vals_c = np.array(cons_mat[label])
    ax_cons.barh(
        ypos,
        vals_c,
        left=lefts_c,
        height=0.68,
        color=CONSUMER_COLORS[label],
        linewidth=0,
    )
    lefts_c += vals_c

# X-axis: percentage of total ecoregion BD impact
max_attributed = max(
    sum(cons_mat[lab][i] for lab in top_global + ["Other"]) for i in range(n)
)
ax_cons.set_xlim(0, min(max_attributed * 1.12, 100))
ax_cons.set_xlabel("% of ecoregion BD impact", fontsize=9.5, labelpad=6)
ax_cons.set_yticks(ypos)
ax_cons.set_yticklabels([""] * n)
ax_cons.tick_params(axis="y", length=0)
ax_cons.tick_params(axis="x", labelsize=8)
ax_cons.spines[["top", "right", "left"]].set_visible(False)
ax_cons.grid(axis="x", color="grey", alpha=0.2, linewidth=0.5, zorder=0)
ax_cons.set_axisbelow(True)
ax_cons.set_ylim(-0.5, n - 0.5)
ax_cons.xaxis.set_major_formatter(ticker.FuncFormatter(lambda x, _: f"{x:.0f}%"))

# Divider between panels b and c (subtle vertical rule on ax_cons left spine)
ax_cons.spines["left"].set_visible(True)
ax_cons.spines["left"].set_color("#DDDDDD")
ax_cons.spines["left"].set_linewidth(0.8)

# Legend for panel c — anchored outside the right edge of the panel so it
# never overlaps bar labels or data.  bbox_inches='tight' in savefig ensures
# it is fully captured in the saved file.
legend_c_handles = [
    mpatches.Patch(
        color=CONSUMER_COLORS[lab], label=lab if len(lab) <= 24 else lab[:23] + "…"
    )
    for lab in top_global + ["Other"]
]
ax_cons.legend(
    handles=legend_c_handles,
    title="Consuming country",
    title_fontsize=8,
    fontsize=7.5,
    loc="upper left",
    bbox_to_anchor=(1.02, 1.0),
    framealpha=0.96,
    edgecolor="#CCCCCC",
    frameon=True,
    ncol=1,
    borderpad=0.8,
    labelspacing=0.45,
)

# Panel c title
ax_cons.set_title("Consumer\nattribution", fontsize=8.5, pad=5, color="#444444")

# ── Figure titles & panel labels ─────────────────────────────────────────────
fig.text(
    0.60,
    0.985,
    "Top 20 ecoregions by biodiversity impact from global stimulant consumption (2020)",
    ha="center",
    va="top",
    fontsize=12,
    fontweight="bold",
    color="#1A1A1A",
)

for ax, label in [(ax_bar, "a"), (ax_heat, "b"), (ax_cons, "c")]:
    ax.text(
        -0.02,
        1.01,
        label,
        transform=ax.transAxes,
        fontsize=11,
        fontweight="bold",
        va="bottom",
        ha="right",
        color="#333333",
    )

# ── Save ─────────────────────────────────────────────────────────────────────
for ext in ["png"]:
    fp = f"{OUT_FIG_BASE}.{ext}"
    plt.savefig(fp, dpi=300, bbox_inches="tight", facecolor="white")
    print(f"  Saved: {fp}", flush=True)

plt.close()
print("Figure done.", flush=True)
