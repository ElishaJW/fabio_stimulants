"""
sankeys.py
==========
Publication-quality 3-stage Sankey diagrams: Biodiversity footprint flows
in the global supply chains for cocoa, coffee, tea, and tobacco.

Stages
------
  1. Producing country  – where the primary crop is grown (BD impact source)
  2. Processing hub     – country that converts raw commodity to processed
                          products (Z-matrix trade flows; or producing country
                          itself when no interregional processing is observed)
  3. Consuming country  – country of final demand (Y-matrix)

Method
------
  * S1→S2 : Z-matrix rows for primary commodity, columns for processed-
    commodity sectors, impact-weighted by producer's total BD footprint.
    If Z flows are zero (no interregional processing in FABIO — as for tea
    and tobacco), the producing country itself acts as the processing hub
    and BD impact is assigned directly.
  * S2→S3 : Y-matrix rows for processed commodities, aggregated per
    processing country and consuming country, impact-weighted.
    Demand category used: "food" for cocoa/coffee/tea; "unspecified"+"other"
    for tobacco (tobacco is not classified as food in FABIO).
  * Both Z and Y matrices are scanned ONCE across all commodity groups.

Data  : FABIO MRIO model 2020  |  bd_fp_total_2020.rds  |  IOT_2020_csv.zip
Unit  : PDF·year (Potentially Disappeared Fraction × year)
"""

import os, io, zipfile, sys, pickle

if sys.stdout.encoding and sys.stdout.encoding.lower() != "utf-8":
    sys.stdout.reconfigure(encoding="utf-8", errors="replace")

import numpy as np
import pandas as pd
import pyreadr
import plotly.graph_objects as go

# ── paths ─────────────────────────────────────────────────────────────────────
BASE = (
    r"C:\Users\elishaw\OneDrive - NTNU\BAMBOO-personal"
    r"\WP3 - FABIO Development\fabio_stimulants"
)
os.chdir(BASE)

YEAR = 2020
ZIP = os.path.join("data", "IOT_2020_csv.zip")
BD_FP = os.path.join("data", "biodiversity footprints", f"bd_fp_total_{YEAR}.rds")

# ── commodity group definitions ───────────────────────────────────────────────
# demand_cats: Y-matrix demand categories to use for final consumption
GROUPS = {
    "cocoa": {
        "title": "Cocoa",
        "bean_codes": ["c046"],
        "proc_codes": ["c126", "c127", "c128", "c129", "c130"],
        "demand_cats": ["food"],
        "bd_cat": "food",  # column suffix in bd_fp_total to use
    },
    "coffee": {
        "title": "Coffee",
        "bean_codes": ["c045"],
        "proc_codes": ["c131", "c132", "c135"],
        "demand_cats": ["food"],
        "bd_cat": "food",
    },
    "tea": {
        "title": "Tea",
        "bean_codes": ["c047"],
        "proc_codes": ["c047", "c133"],   # c047 traded interregionally before final use
        "demand_cats": ["food"],
        "bd_cat": "food",
    },
    "tobacco": {
        "title": "Tobacco",
        "bean_codes": ["c059"],
        "proc_codes": ["c136", "c137", "c138"],
        "demand_cats": ["unspecified", "other"],
        "bd_cat": "unspecified_other",  # use unspecified+other cols
    },
}

# =============================================================================
# 1.  LOAD REFERENCE TABLES
# =============================================================================
print("Loading reference tables …")
items = pd.read_csv("inst/items_full.csv")
regions = pd.read_csv("inst/regions_fabio.csv", encoding="latin-1")
regions["continent2"] = regions["continent"].map(
    lambda c: {"LAM": "SAM", "EU": "EUR"}.get(str(c), str(c))
)

item_list = items["comm_code"].tolist()
region_list = regions["area_code"].tolist()
n_items = len(item_list)  # 136
n_regions = len(region_list)  # 192
assert n_items * n_regions == 26112
print(f"  items: {n_items},  regions: {n_regions}")

item_idx = {c: i for i, c in enumerate(item_list)}
reg_idx = {r: i for i, r in enumerate(region_list)}

area_to_iso = dict(zip(regions["area_code"], regions["iso3c"]))
area_to_cont = dict(zip(regions["area_code"], regions["continent2"]))
iso_to_cont = dict(zip(regions["iso3c"], regions["continent2"]))

# =============================================================================
# 2.  LOAD BIODIVERSITY FOOTPRINT
# =============================================================================
print("Loading bd_fp_total via pyreadr …")
bd = pyreadr.read_r(BD_FP)[None]
print(f"  bd_fp shape: {bd.shape}")


def parse_row(name):
    p = str(name).rsplit("_", 1)
    return (p[0], int(p[1])) if len(p) == 2 else (name, np.nan)


bd_meta = pd.DataFrame(
    [parse_row(r) for r in bd.index], columns=["comm_code", "area_code"]
)
bd_meta["area_code"] = pd.to_numeric(bd_meta["area_code"], errors="coerce")
bd_meta = bd_meta.merge(
    regions[["area_code", "iso3c", "continent2"]], on="area_code", how="left"
)

food_cols = [c for c in bd.columns if c.endswith("_food")]
unspec_other_cols = [
    c for c in bd.columns if c.endswith("_unspecified") or c.endswith("_other")
]
print(
    f"  food columns: {len(food_cols)},  unspecified+other columns: {len(unspec_other_cols)}"
)

# =============================================================================
# 3.  Z MATRIX SCAN  (single pass for all groups)
# =============================================================================
CACHE_Z = os.path.join("data", "cache_z_stimulants_v2_flows.pkl")   # v2: tea c047 proc col added
CACHE_Y = os.path.join("data", "cache_y_stimulants_v3_flows.pkl")   # v3: tea c047 Y rows added

if os.path.exists(CACHE_Z):
    print("\nLoading Z flows from cache …")
    with open(CACHE_Z, "rb") as f:
        z_flows_by_group = pickle.load(f)
    for g, df in z_flows_by_group.items():
        print(f"  {g}: {len(df)} flows")
else:
    print("\nScanning Z matrix (single pass, all groups) …")

    target_z_rows = {}
    for g, grp in GROUPS.items():
        for code in grp["bean_codes"]:
            if code not in item_idx:
                continue
            cidx = item_idx[code]
            for ri in range(n_regions):
                target_z_rows[ri * n_items + cidx] = (g, region_list[ri])

    target_z_cols = {}
    for g, grp in GROUPS.items():
        for proc_area in region_list:
            ri = reg_idx[proc_area]
            cols = [
                ri * n_items + item_idx[c] for c in grp["proc_codes"] if c in item_idx
            ]
            if cols:
                target_z_cols[(g, proc_area)] = cols

    z_rows_acc = {g: [] for g in GROUPS}

    with zipfile.ZipFile(ZIP, "r") as zf:
        with zf.open("IOT_2020_csv/2020_Z_mass.csv") as raw:
            reader = io.TextIOWrapper(raw, encoding="utf-8")
            next(reader)
            for line_no, line in enumerate(reader):
                if line_no not in target_z_rows:
                    continue
                g, area_here = target_z_rows[line_no]
                fields = line.strip().split(",")
                for proc_area in region_list:
                    col_idxs = target_z_cols.get((g, proc_area))
                    if not col_idxs:
                        continue
                    mass = 0.0
                    for ci in col_idxs:
                        try:
                            mass += float(fields[ci + 1])
                        except (ValueError, IndexError):
                            pass
                    if mass > 1e-10:
                        z_rows_acc[g].append(
                            {
                                "producer_area": area_here,
                                "processor_area": proc_area,
                                "mass_flow": mass,
                            }
                        )

    z_flows_by_group = {
        g: (
            pd.DataFrame(rows)
            if rows
            else pd.DataFrame(columns=["producer_area", "processor_area", "mass_flow"])
        )
        for g, rows in z_rows_acc.items()
    }
    with open(CACHE_Z, "wb") as f:
        pickle.dump(z_flows_by_group, f)
    for g, df in z_flows_by_group.items():
        print(f"  {g}: {len(df)} flows (cached)")

# =============================================================================
# 4.  Y MATRIX SCAN  (single pass, all groups, all relevant demand categories)
# =============================================================================
if os.path.exists(CACHE_Y):
    print("\nLoading Y flows from cache …")
    with open(CACHE_Y, "rb") as f:
        y_flows_by_group = pickle.load(f)
    for g, df in y_flows_by_group.items():
        print(f"  {g}: {len(df)} flows")
else:
    print("\nScanning Y matrix (single pass, all groups) …")

    # Collect all demand_cats needed across all groups
    all_demand_cats = set()
    for grp in GROUPS.values():
        all_demand_cats.update(grp["demand_cats"])

    # row_idx → (group_key, area_code, comm_code)
    target_y_rows = {}
    for g, grp in GROUPS.items():
        for code in grp["proc_codes"]:
            if code not in item_idx:
                continue
            cidx = item_idx[code]
            for ri in range(n_regions):
                row_no = ri * n_items + cidx
                target_y_rows[row_no] = (g, region_list[ri], code)

    y_rows_acc = {g: [] for g in GROUPS}

    with zipfile.ZipFile(ZIP, "r") as zf:
        with zf.open("IOT_2020_csv/2020_Y_mass.csv") as raw:
            reader2 = io.TextIOWrapper(raw, encoding="utf-8")
            y_header = next(reader2).strip().split(",")
            # col positions per demand category
            cat_col_pos = {}
            cat_col_area = {}
            for i, h in enumerate(y_header[1:]):
                clean = h.strip('"')
                if "_" not in clean:
                    continue
                area_str, cat = clean.split("_", 1)
                if cat not in all_demand_cats:
                    continue
                cat_col_pos.setdefault(cat, []).append(i)
                cat_col_area.setdefault(cat, []).append(int(area_str))

            for line_no, line in enumerate(reader2):
                if line_no not in target_y_rows:
                    continue
                g, area_here, comm_here = target_y_rows[line_no]
                fields = line.strip().split(",")
                for cat in GROUPS[g]["demand_cats"]:
                    for fi, cons_area in zip(
                        cat_col_pos.get(cat, []), cat_col_area.get(cat, [])
                    ):
                        try:
                            mass = float(fields[fi + 1])
                        except (ValueError, IndexError):
                            mass = 0.0
                        if mass > 1e-10:
                            y_rows_acc[g].append(
                                {
                                    "processor_area": area_here,
                                    "commodity": comm_here,
                                    "cons_area": cons_area,
                                    "demand_cat": cat,
                                    "mass_flow": mass,
                                }
                            )

    y_flows_by_group = {
        g: (
            pd.DataFrame(rows)
            if rows
            else pd.DataFrame(
                columns=[
                    "processor_area",
                    "commodity",
                    "cons_area",
                    "demand_cat",
                    "mass_flow",
                ]
            )
        )
        for g, rows in y_rows_acc.items()
    }
    with open(CACHE_Y, "wb") as f:
        pickle.dump(y_flows_by_group, f)
    for g, df in y_flows_by_group.items():
        print(f"  {g}: {len(df)} flows (cached)")

# =============================================================================
# 5.  COLOUR PALETTE
# =============================================================================
CONT_COLOR = {
    "AFR": (60, 151, 112),
    "SAM": (222, 179, 64),
    "ASI": (68, 112, 157),
    "OCE": (217, 120, 40),
    "EUR": (131, 153, 42),
    "NAM": (162, 60, 51),
}
CONT_LABEL = {
    "AFR": "Africa",
    "SAM": "S. America",
    "ASI": "Asia",
    "EUR": "Europe",
    "NAM": "N. America",
    "OCE": "Oceania",
}


def rgba(cont, alpha=1.0):
    r, g, b = CONT_COLOR.get(cont, (153, 153, 153))
    return f"rgba({r},{g},{b},{alpha:.2f})"


def get_cont(label):
    clean = label.strip()
    if clean.startswith("Other-"):
        return clean.replace("Other-", "")
    if clean in CONT_COLOR:
        return clean
    return iso_to_cont.get(clean, "")


# =============================================================================
# 6.  SANKEY BUILDER
# =============================================================================
def build_sankey(
    s12: pd.DataFrame,
    s23: pd.DataFrame,
    title: str,
    year: int,
    demand_label: str = "food consumption",
    height: int = 900,
) -> go.Figure:

    stage1_labels = sorted(set(s12["stage1"].dropna()))
    stage2_labels = sorted(
        set(list(s12["stage2"].dropna()) + list(s23["stage2"].dropna()))
    )
    stage3_labels = sorted(set(s23["stage3"].dropna()))

    s1 = [f"{l}  " for l in stage1_labels]
    s2 = [f"{l}" for l in stage2_labels]
    s3 = [f"  {l}" for l in stage3_labels]

    all_nodes = s1 + s2 + s3
    node_idx = {n: i for i, n in enumerate(all_nodes)}

    def s1n(l):
        return f"{l}  "

    def s2n(l):
        return f"{l}"

    def s3n(l):
        return f"  {l}"

    def node_color(label, is_stage2=False):
        clean = label.strip()
        cont = get_cont(clean)
        alpha = 0.50 if clean.startswith("Other-") else (0.80 if is_stage2 else 1.0)
        return rgba(cont, alpha)

    node_colors = [
        node_color(n.strip(), is_stage2=(n.strip() in stage2_labels)) for n in all_nodes
    ]

    sources, targets, values, link_colors = [], [], [], []

    for _, row in s12.iterrows():
        src, tgt = s1n(row["stage1"]), s2n(row["stage2"])
        if src not in node_idx or tgt not in node_idx:
            continue
        sources.append(node_idx[src])
        targets.append(node_idx[tgt])
        values.append(float(row["flow"]))
        link_colors.append(rgba(get_cont(row["stage1"]), 0.35))

    for _, row in s23.iterrows():
        src, tgt = s2n(row["stage2"]), s3n(row["stage3"])
        if src not in node_idx or tgt not in node_idx:
            continue
        sources.append(node_idx[src])
        targets.append(node_idx[tgt])
        values.append(float(row["flow"]))
        link_colors.append(rgba(get_cont(row["stage2"]), 0.35))

    display_labels = [CONT_LABEL.get(n.strip(), n.strip()) for n in all_nodes]
    node_x = [0.01] * len(s1) + [0.45] * len(s2) + [0.99] * len(s3)
    node_y = (
        np.linspace(0.04, 0.80, max(len(s1), 1)).tolist()
        + np.linspace(0.04, 0.80, max(len(s2), 1)).tolist()
        + np.linspace(0.04, 0.80, max(len(s3), 1)).tolist()
    )

    total_impact = sum(values) / 2

    fig = go.Figure(
        go.Sankey(
            arrangement="snap",
            node=dict(
                pad=18,
                thickness=24,
                line=dict(color="white", width=0.8),
                label=display_labels,
                color=node_colors,
                x=node_x,
                y=node_y,
            ),
            link=dict(
                source=sources,
                target=targets,
                value=values,
                color=link_colors,
                hovertemplate=(
                    "<b>%{source.label}</b> → <b>%{target.label}</b><br>"
                    "BD Impact: %{value:.2e} PDF·year<extra></extra>"
                ),
            ),
        )
    )

    fig.update_layout(
        title=dict(
            text=f"<b>{title} Products   |   {year}   |   Total Impact: {total_impact:.2e} PDF·year</b>",
            x=0.5,
            xanchor="center",
            font=dict(size=26, color="#1a1a1a"),
        ),
        annotations=[
            dict(
                x=0.01,
                y=1.07,
                xref="paper",
                yref="paper",
                text="<b>PRODUCING<br>COUNTRY</b>",
                showarrow=False,
                font=dict(size=16, color="#333333"),
                align="center",
            ),
            dict(
                x=0.45,
                y=1.07,
                xref="paper",
                yref="paper",
                text="<b>PROCESSING<br>HUB</b>",
                showarrow=False,
                font=dict(size=16, color="#333333"),
                align="center",
            ),
            dict(
                x=0.99,
                y=1.07,
                xref="paper",
                yref="paper",
                text="<b>CONSUMING<br>COUNTRY</b>",
                showarrow=False,
                font=dict(size=16, color="#333333"),
                align="center",
            ),
        ],
        font=dict(size=15, family="Arial"),
        paper_bgcolor="white",
        plot_bgcolor="white",
        margin=dict(l=30, r=30, t=170, b=100),
        width=1400,
        height=height,
    )
    return fig


# =============================================================================
# 7.  PER-COMMODITY FLOW COMPUTATION AND FIGURE GENERATION
# =============================================================================
MIN_SHARE = 0.003
TOP_N = 12
TOP_PROC = 12
TOP_CONS = 15


def filter_flows(df, min_share):
    total = df["flow"].sum()
    return df[df["flow"] >= total * min_share].copy() if total > 0 else df.copy()


def safe_nlargest(series, n):
    """nlargest that handles empty or object-dtype Series."""
    s = pd.to_numeric(series, errors="coerce").dropna()
    return s.nlargest(n) if len(s) > 0 else s


for group_key, grp in GROUPS.items():
    print(f"\n{'='*60}")
    print(f"Building Sankey: {grp['title']}")

    # ── BD impact per producing country ───────────────────────────────────────
    bd_use_cols = (
        unspec_other_cols if grp["bd_cat"] == "unspecified_other" else food_cols
    )
    bean_mask = bd_meta["comm_code"].isin(grp["bean_codes"])
    bd_beans = bd.loc[bean_mask.values, bd_use_cols]
    bean_meta = bd_meta.loc[bean_mask.values].reset_index(drop=True).copy()
    bean_meta["total_bd"] = bd_beans.values.sum(axis=1)

    prod_bd = (
        bean_meta.groupby(["area_code", "iso3c", "continent2"])["total_bd"]
        .sum()
        .reset_index()
        .sort_values("total_bd", ascending=False)
    )
    prod_bd = prod_bd[
        prod_bd["continent2"].notna() & ~prod_bd["continent2"].isin(["ROW", "nan"])
    ]

    print(
        "  Top 5 producers: "
        + ", ".join(
            f"{r.iso3c}({r.total_bd:.2e})" for _, r in prod_bd.head(5).iterrows()
        )
    )

    top_producers = set(prod_bd.head(TOP_N)["area_code"].tolist())
    prod_bd_map = dict(zip(prod_bd["area_code"], prod_bd["total_bd"]))

    def label_stage1(area, _top=top_producers):
        if area in _top:
            return area_to_iso.get(area, str(area))
        cont = area_to_cont.get(area, "?")
        return None if cont in ("ROW", "nan", None, "?") else f"Other-{cont}"

    # ── S1→S2: Z-matrix flows (or self-processing proxy) ─────────────────────
    z = z_flows_by_group[group_key].copy()
    if not z.empty:
        z["prod_bd"] = z["producer_area"].map(prod_bd_map).fillna(0)
        z["prod_iso"] = z["producer_area"].apply(label_stage1)
        z["proc_iso"] = z["processor_area"].map(area_to_iso)
        z["proc_cont"] = z["processor_area"].map(area_to_cont)
        z["prod_total_mass"] = z.groupby("producer_area")["mass_flow"].transform("sum")
        z["bd_flow"] = (
            z["prod_bd"] * z["mass_flow"] / z["prod_total_mass"].replace(0, np.nan)
        ).fillna(0)

        s12 = (
            z[z["bd_flow"] > 0]
            .groupby(["prod_iso", "proc_iso", "proc_cont"])["bd_flow"]
            .sum()
            .reset_index()
            .rename(
                columns={
                    "prod_iso": "stage1",
                    "proc_iso": "stage2",
                    "proc_cont": "stage2_cont",
                    "bd_flow": "flow",
                }
            )
        )
        s12 = s12[s12["stage1"].notna() & s12["stage2"].notna()]
        print("  Z flows found – using actual processing routes")
    else:
        # No interregional processing in FABIO: producer country = processor
        print("  No Z flows – using producer-as-processor proxy")
        rows = []
        for _, prow in prod_bd.iterrows():
            lbl = label_stage1(prow["area_code"])
            if lbl is None:
                continue
            rows.append(
                {
                    "stage1": lbl,
                    "stage2": prow["iso3c"],
                    "stage2_cont": prow["continent2"],
                    "flow": prow["total_bd"],
                }
            )
        s12 = pd.DataFrame(rows).dropna(subset=["stage1", "stage2"])

    # ── S2→S3: Y-matrix flows ─────────────────────────────────────────────────
    y = y_flows_by_group[group_key].copy()
    if not y.empty:
        y["proc_iso"] = y["processor_area"].map(area_to_iso)
        y["cons_iso"] = y["cons_area"].map(area_to_iso)
        y["cons_cont"] = y["cons_area"].map(area_to_cont)

        # BD impact per processing country
        if not s12.empty and s12["flow"].sum() > 0:
            proc_bd_map = s12.groupby("stage2")["flow"].sum().to_dict()
        else:
            # fallback: distribute total BD proportional to Y output
            y_mass = y.groupby("proc_iso")["mass_flow"].sum()
            total_mass = y_mass.sum()
            total_bd = prod_bd["total_bd"].sum()
            proc_bd_map = (
                (y_mass / total_mass * total_bd).to_dict() if total_mass > 0 else {}
            )

        y["proc_bd"] = y["proc_iso"].map(proc_bd_map).fillna(0)
        y["proc_total_mass"] = y.groupby("proc_iso")["mass_flow"].transform("sum")
        y["bd_flow"] = (
            y["proc_bd"] * y["mass_flow"] / y["proc_total_mass"].replace(0, np.nan)
        ).fillna(0)

        s23 = (
            y[
                (y["bd_flow"] > 0)
                & y["cons_iso"].notna()
                & ~y["cons_cont"].isin(["ROW", "nan"])
            ]
            .groupby(["proc_iso", "cons_iso"])["bd_flow"]
            .sum()
            .reset_index()
            .rename(
                columns={"proc_iso": "stage2", "cons_iso": "stage3", "bd_flow": "flow"}
            )
        )
        s23 = s23[s23["stage2"].notna()]
    else:
        print("  WARNING: No Y flows found for this commodity")
        s23 = pd.DataFrame(columns=["stage2", "stage3", "flow"])

    # ── filter & collapse minor nodes ─────────────────────────────────────────
    s12 = filter_flows(s12, MIN_SHARE)
    s23 = filter_flows(s23, MIN_SHARE)

    # collapse minor processing countries
    s12_sum = (
        s12.groupby("stage2")["flow"].sum() if not s12.empty else pd.Series(dtype=float)
    )
    s23_sum = (
        s23.groupby("stage2")["flow"].sum() if not s23.empty else pd.Series(dtype=float)
    )
    combined = pd.concat([s12_sum, s23_sum]).groupby(level=0).sum()
    proc_importance = safe_nlargest(combined, TOP_PROC).index.tolist()

    def collapse_proc(iso, _imp=proc_importance):
        if iso in _imp:
            return iso
        cont = iso_to_cont.get(iso, "?")
        return None if cont in ("ROW", "nan", None, "?") else f"Other-{cont}"

    if not s12.empty:
        s12["stage2"] = s12["stage2"].apply(collapse_proc)
        s12 = s12.dropna(subset=["stage1", "stage2"])
        s12 = s12.groupby(["stage1", "stage2"])["flow"].sum().reset_index()

    if not s23.empty:
        s23["stage2"] = s23["stage2"].apply(collapse_proc)
        s23 = s23.dropna(subset=["stage2", "stage3"])
        s23 = s23.groupby(["stage2", "stage3"])["flow"].sum().reset_index()

    # collapse minor consuming countries
    s23_cons = (
        s23.groupby("stage3")["flow"].sum() if not s23.empty else pd.Series(dtype=float)
    )
    cons_importance = safe_nlargest(s23_cons, TOP_CONS).index.tolist()

    def collapse_cons(iso, _imp=cons_importance):
        if iso in _imp:
            return iso
        cont = iso_to_cont.get(iso, "?")
        return None if cont in ("ROW", "nan", None, "?") else f"Other-{cont}"

    if not s23.empty:
        s23["stage3"] = s23["stage3"].apply(collapse_cons)
        s23 = s23.dropna(subset=["stage2", "stage3"])
        s23 = s23.groupby(["stage2", "stage3"])["flow"].sum().reset_index()

    # remove ROW
    s12 = s12[~s12["stage1"].str.startswith("Other-ROW", na=True)]
    if not s23.empty:
        s23 = s23[~s23["stage3"].str.startswith("Other-ROW", na=True)]

    # final filter
    s12 = filter_flows(s12, MIN_SHARE)
    s23 = filter_flows(s23, MIN_SHARE)

    print(f"  S1→S2 flows: {len(s12)},  S2→S3 flows: {len(s23)}")

    # ── build demand label for subtitle ───────────────────────────────────────
    cats = grp["demand_cats"]
    demand_label = (
        "food consumption" if cats == ["food"] else f"{'/'.join(cats)} consumption"
    )

    # ── compute canvas height from node count ─────────────────────────────────
    n_max = max(
        len(set(s12["stage1"].dropna())),
        len(set(list(s12["stage2"].dropna()) + list(s23["stage2"].dropna()))),
        len(set(s23["stage3"].dropna())),
        1,
    )
    fig_height = max(900, n_max * 40 + 300)

    # ── build and save figure ─────────────────────────────────────────────────
    fig = build_sankey(
        s12, s23, title=grp["title"], year=YEAR,
        demand_label=demand_label, height=fig_height,
    )
    png_out = f"{group_key}_biodiversity_sankey.png"
    try:
        fig.write_image(png_out, width=1400, height=fig_height, scale=2)
        print(f"  Saved {png_out}")
    except Exception as e:
        print(f"  PNG export failed: {e}")

print("\n=== All done ===")
