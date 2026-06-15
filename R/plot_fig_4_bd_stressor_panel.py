"""
plot_fig_4_bd_stressor_panel.py

Python equivalent of plot_fig_4_bd_stressor_panel.R

4 × 4 panel map figure:
  Columns = commodities  (coffee, cocoa, tea, tobacco)
  Rows    = stressors    (land use, water, N deposition, P deposition)

Each row shares a log10 colour scale across all 4 commodities.
Palette: "turbo" (blue → cyan → yellow → orange → red), heatmap-style.

Vector layers (land use, water): read with geopandas, projected to Robinson.
Raster layers (N, P): reprojected to Robinson with rasterio, plotted with imshow.
Missing files render as a labelled empty panel rather than raising an error.

Output: {ckey}_bd_stressor_panel_{year}_py.png  (30 × 22 cm, 300 dpi)
"""

import os
import warnings
import numpy as np
import matplotlib
matplotlib.use("Agg")                          # non-interactive backend
import matplotlib.pyplot as plt
import matplotlib.colors as mcolors
import matplotlib.ticker as mticker
import matplotlib.cm as mcm
from matplotlib.patches import Polygon as MplPolygon
import geopandas as gpd
import rasterio
from rasterio.warp import (reproject, Resampling,
                            calculate_default_transform)
import pyproj


# ── CONFIG ────────────────────────────────────────────────────────────────────
CKEY     = "world"
YEAR     = 2020
SAVE_FIG = True

BASE_RESULTS = "X:/Eli/projects/fabio_stimulants/results_data"

# Robinson projection — PROJ string for maximum compatibility
ROBIN_CRS = "+proj=robin +lon_0=0 +x_0=0 +y_0=0 +datum=WGS84 +units=m +no_defs"

COMMODITIES      = ["coffee", "cocoa", "tea",    "tobacco"]
COMMODITY_LABELS = ["Coffee", "Cocoa", "Tea",    "Tobacco"]

STRESSORS = {
    "landuse": {
        "label":   "Land use",
        "type":    "vector",
        "col":     "landuse_bd",
        "path_fn": lambda stim: os.path.join(
            BASE_RESULTS, "bd_footprint_by_ecoregion", CKEY,
            f"{CKEY}_bd_footprint_{YEAR}_{stim}.gpkg"),
    },
    "water": {
        "label":   "Water",
        "type":    "vector",
        "col":     "water_bd",
        "path_fn": lambda stim: os.path.join(
            BASE_RESULTS, "bd_footprint_by_basin", CKEY,
            f"{CKEY}_bd_footprint_{YEAR}_{stim}.gpkg"),
    },
    "n": {
        "label":   "N deposition",
        "type":    "raster",
        "col":     None,
        "path_fn": lambda stim: os.path.join(
            BASE_RESULTS, "bd_footprint_by_np_scale",
            f"{CKEY}_bd_footprint_n_{YEAR}_{stim}.tif"),
    },
    "p": {
        "label":   "P deposition",
        "type":    "raster",
        "col":     None,
        "path_fn": lambda stim: os.path.join(
            BASE_RESULTS, "bd_footprint_by_np_scale",
            f"{CKEY}_bd_footprint_p_{YEAR}_{stim}.tif"),
    },
}


# ── BASE LAYERS (loaded once, shared by all panels) ───────────────────────────
print("Loading base layers...")

world_gdf = gpd.read_file(
    "X:/Eli/DATA/shapefiles/TM_WorldBorders/TM_WORLD_BORDERS-0.3.shp"
).to_crs(ROBIN_CRS)

# Densified globe border: ~1° segments along each edge, projected to Robinson.
# Using manual point arrays avoids shapely version dependencies.
_n_side = 720          # ~0.5° per segment (plenty of curvature points)
_n_pole = 360
_lons = np.concatenate([
    np.linspace(-179.9,  179.9, _n_side),   # S edge left→right
    np.full(_n_pole,  179.9),               # E edge bottom→top
    np.linspace( 179.9, -179.9, _n_side),   # N edge right→left
    np.full(_n_pole, -179.9),               # W edge top→bottom
])
_lats = np.concatenate([
    np.full(_n_side, -89.9),                # S edge
    np.linspace(-89.9,  89.9, _n_pole),     # E edge
    np.full(_n_side,  89.9),                # N edge
    np.linspace( 89.9, -89.9, _n_pole),     # W edge
])
_tr = pyproj.Transformer.from_crs("EPSG:4326", ROBIN_CRS, always_xy=True)
_rx, _ry = _tr.transform(_lons, _lats)
GLOBE_COORDS = np.column_stack([_rx, _ry])
GLOBE_XLIM   = (GLOBE_COORDS[:, 0].min(), GLOBE_COORDS[:, 0].max())
GLOBE_YLIM   = (GLOBE_COORDS[:, 1].min(), GLOBE_COORDS[:, 1].max())


# ── FIRST PASS: per-stressor colour ranges ────────────────────────────────────
print("Computing per-stressor colour ranges...")
stressor_ranges = {}

for str_key, si in STRESSORS.items():
    all_vals = []
    for stim in COMMODITIES:
        path = si["path_fn"](stim)
        if not os.path.exists(path):
            continue
        if si["type"] == "vector":
            gdf  = gpd.read_file(path)
            vals = gdf[si["col"]].dropna().values
        else:
            with rasterio.open(path) as src:
                arr = src.read(1, masked=True)
                vals = arr.compressed()          # excludes nodata mask
        vals = vals[np.isfinite(vals) & (vals > 0)]
        all_vals.extend(vals.tolist())

    if all_vals:
        vmin, vmax = float(np.min(all_vals)), float(np.max(all_vals))
        stressor_ranges[str_key] = (vmin, vmax)
        print(f"  {str_key:<12}  {vmin:.2e} – {vmax:.2e}")
    else:
        stressor_ranges[str_key] = None
        print(f"  {str_key:<12}  no data")


# ── RASTER HELPER ─────────────────────────────────────────────────────────────
def load_raster_robin(path):
    """
    Read a GeoTIFF and reproject to Robinson.
    Returns (array, extent) where extent = (left, right, bottom, top) in
    Robinson metres — suitable for matplotlib's imshow(extent=...).
    """
    with rasterio.open(path) as src:
        src_crs = src.crs if src.crs else rasterio.CRS.from_epsg(4326)
        dst_transform, dst_w, dst_h = calculate_default_transform(
            src_crs, ROBIN_CRS, src.width, src.height, *src.bounds)
        out = np.full((dst_h, dst_w), np.nan, dtype=np.float32)
        reproject(
            source      = rasterio.band(src, 1),
            destination = out,
            src_transform = src.transform,
            src_crs       = src_crs,
            dst_transform = dst_transform,
            dst_crs       = ROBIN_CRS,
            resampling    = Resampling.bilinear,
            dst_nodata    = np.nan,
        )
    out[out <= 0] = np.nan
    left   = dst_transform.c
    top    = dst_transform.f
    right  = left + dst_transform.a * dst_w
    bottom = top  + dst_transform.e * dst_h   # e is negative
    return out, (left, right, bottom, top)


# ── COLOURMAP ─────────────────────────────────────────────────────────────────
try:
    TURBO = plt.get_cmap("turbo")
except (ValueError, KeyError):
    warnings.warn("'turbo' colormap not found — falling back to 'jet'.")
    TURBO = plt.get_cmap("jet")


# ── FIGURE ────────────────────────────────────────────────────────────────────
print("Building 4×4 panel...")

fig, axes = plt.subplots(
    nrows = 4, ncols = 4,
    figsize = (30 / 2.54, 22 / 2.54),   # cm → inches
    gridspec_kw = {"wspace": 0.03, "hspace": 0.05},
)
fig.patch.set_facecolor("white")

stressor_keys = list(STRESSORS.keys())

for row_i, str_key in enumerate(stressor_keys):
    si       = STRESSORS[str_key]
    bd_range = stressor_ranges.get(str_key)
    norm     = (mcolors.LogNorm(vmin=bd_range[0], vmax=bd_range[1])
                if bd_range else None)

    for col_i, stim in enumerate(COMMODITIES):
        ax   = axes[row_i, col_i]
        path = si["path_fn"](stim)
        print(f"  [{row_i * 4 + col_i + 1:2d}] {str_key:<12} / {stim}")

        # ── World basemap ──────────────────────────────────────────────────
        world_gdf.plot(ax=ax, color="#EBEBEB", edgecolor="white",
                       linewidth=0.08, zorder=1)

        # ── Globe border ───────────────────────────────────────────────────
        ax.add_patch(MplPolygon(
            GLOBE_COORDS, closed=True,
            fill=False, edgecolor="#C0C0C0", linewidth=0.3, zorder=4))

        # ── Data layer ─────────────────────────────────────────────────────
        has_data = os.path.exists(path) and norm is not None

        if not has_data:
            ax.text(0, 0, "No data", ha="center", va="center",
                    color="grey", fontsize=4, style="italic",
                    transform=ax.transData, zorder=5)

        elif si["type"] == "vector":
            with warnings.catch_warnings():
                warnings.simplefilter("ignore")
                gdf = gpd.read_file(path).to_crs(ROBIN_CRS)
            gdf = gdf[gdf[si["col"]].notna() & (gdf[si["col"]] > 0)]
            if not gdf.empty:
                gdf.plot(ax=ax, column=si["col"], cmap=TURBO, norm=norm,
                         linewidth=0, legend=False, zorder=2)

        else:
            data, extent = load_raster_robin(path)
            ax.imshow(data, extent=extent, cmap=TURBO, norm=norm,
                      origin="upper", interpolation="nearest",
                      aspect="auto", zorder=2)

        # ── Axis limits + style ────────────────────────────────────────────
        ax.set_xlim(*GLOBE_XLIM)
        ax.set_ylim(*GLOBE_YLIM)
        ax.set_aspect("equal", adjustable="box")
        ax.axis("off")

        # ── Column title (top row only) ────────────────────────────────────
        if row_i == 0:
            ax.set_title(COMMODITY_LABELS[col_i],
                         fontsize=7, fontweight="bold", pad=3)

        # ── Row label (left column only, rotated) ─────────────────────────
        if col_i == 0:
            ax.text(-0.09, 0.5, si["label"],
                    transform=ax.transAxes,
                    rotation=90, ha="center", va="center",
                    fontsize=6, fontweight="bold", color="#222222")

        # ── Colourbar (rightmost column only, one per row) ─────────────────
        if col_i == len(COMMODITIES) - 1 and bd_range is not None:
            sm = mcm.ScalarMappable(cmap=TURBO, norm=norm)
            sm.set_array([])
            cbar = fig.colorbar(sm, ax=ax, orientation="vertical",
                                fraction=0.055, pad=0.04, shrink=0.85)
            cbar.ax.tick_params(labelsize=3.5)
            cbar.set_label("BD Impact (PDF·yr)", fontsize=4.5, labelpad=3)
            cbar.ax.yaxis.set_major_formatter(
                mticker.FuncFormatter(lambda x, _: f"{x:.1e}"))


# ── Subtitle ──────────────────────────────────────────────────────────────────
fig.text(0.5, 0.997,
         f"Consumer: {CKEY}  \u2022  Year: {YEAR}"
         f"  \u2022  Stressors at native spatial scale",
         ha="center", va="top", fontsize=6, color="grey")


# ── SAVE ──────────────────────────────────────────────────────────────────────
if SAVE_FIG:
    out_dir  = ("X:/Eli/projects/fabio_stimulants"
                "/results_figures/combined_impacts")
    os.makedirs(out_dir, exist_ok=True)
    fig_path = os.path.join(out_dir,
                            f"{CKEY}_bd_stressor_panel_{YEAR}_py.png")
    fig.savefig(fig_path, dpi=300, bbox_inches="tight",
                facecolor="white")
    print(f"\nFigure saved: {fig_path}")
else:
    print("\n(save_fig=False — figure not written to disk)")
