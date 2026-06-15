"""
BIODIVERSITY FOOTPRINT BAR CHARTS BY CONSUMING COUNTRY — STIMULANT COMMODITIES
Python port of plot_fig_2_footprints_by_country.R

Uses:
  - E_bd_2020.rds  (pyreadr)
  - Y_2020_sparse.csv + Y_rownames.csv + Y_colnames.csv  (pre-exported from R)
  - L_2020_sparse.csv  (pre-exported from R)
  - items_full.csv, regions_fabio.csv

Eli Wilson — elisha.wilson@ntnu.no
"""

import os
import time
import warnings

import numpy as np
import pandas as pd
import matplotlib
import matplotlib.pyplot as plt
import matplotlib.patches as mpatches
import matplotlib.colors as mcolors
from scipy.sparse import csr_matrix
import pyreadr

warnings.filterwarnings("ignore")
matplotlib.rcParams["font.family"] = "DejaVu Sans"

# ── Paths ─────────────────────────────────────────────────────────────────────
BASE = r"C:\Users\elishaw\OneDrive - NTNU\BAMBOO-personal\WP3 - FABIO Development\fabio_stimulants"
IO   = os.path.join(BASE, "data", "IO tables")
OUT  = r"X:\Eli\PROJECTS\fabio_stimulants\results_figures\country_impacts"
os.makedirs(OUT, exist_ok=True)

# ── Load reference tables ─────────────────────────────────────────────────────
print("Loading reference tables...")
items   = pd.read_csv(os.path.join(BASE, "inst", "items_full.csv"),   encoding="latin-1")
regions = pd.read_csv(os.path.join(BASE, "inst", "regions_fabio.csv"), encoding="latin-1")

# ── Stimulant commodity filter ────────────────────────────────────────────────
# Mirrors R: items[comm_group == "Coffee, tea, cocoa" |
#                   comm_group == "Tobacco, rubber" & comm_code != 'c060']
mask_stim = (
    (items["comm_group"] == "Coffee, tea, cocoa") |
    ((items["comm_group"] == "Tobacco, rubber") & (items["comm_code"] != "c060"))
)
items_stim = items.loc[mask_stim, "comm_code"].tolist()
print(f"Stimulant commodity codes ({len(items_stim)}):", items_stim)

# Commodity name groups (for legend coloring)
items_coff  = ["Coffee, green", "Coffee, decaffeinated or roasted",
               "Coffee extracts", "Coffee substitutes"]
items_tea   = ["Tea leaves",
               "Extracts, essences and concentrates of tea or mate, and preparations with a basis thereof or with a basis of tea or mate",
               "Mate leaves"]
items_coco  = ["Cocoa beans", "Cocoa butter, fat and oil", "Cocoa husks and shells",
               "Cocoa paste not defatted", "Cocoa powder and cake", "Chocolate products nes"]
items_tobac = ["Unmanufactured tobacco", "Cigars and cheroots",
               'Other manufactured tobacco and manufactured tobacco substitutes; homogenized"" or ""reconstituted"" tobacco; tobacco extracts and essences"',
               "Cigarettes"]

# ── Load E_bd ─────────────────────────────────────────────────────────────────
print("Loading E_bd...")
result_e = pyreadr.read_r(os.path.join(BASE, "data", "E", "E_bd_2020.rds"))
eb_raw = result_e[None]  # shape (22, 26112); index has duplicates

# E_bd has duplicated index labels — use iloc to extract by position
# Rows 8-14 are the first set of _bd rows (these are the ones R uses)
ROW_LABELS = ["landuse_bd", "water_bd", "P_bd", "N_bd", "CH4_bd", "CO2_bd", "N2O_bd"]
# Use positions 8-14 (first occurrence of _bd rows)
eb = eb_raw.iloc[8:15, :].copy()
eb.index = ROW_LABELS
# eb columns: 'area_comm' format ('1_c001', '1_c002', ...)
# Values are numpy float arrays
print(f"  eb shape: {eb.shape}")

# ── Load IO tables ────────────────────────────────────────────────────────────
print("Loading Y sparse matrix...")
t0 = time.time()
y_sp   = pd.read_csv(os.path.join(IO, "Y_2020_sparse.csv"))
y_rows = pd.read_csv(os.path.join(IO, "Y_rownames.csv"))   # 26112 × 1 (comm codes)
y_cols = pd.read_csv(os.path.join(IO, "Y_colnames.csv"))   # 1344 × 1 ('area_category')
Y_rownames = y_rows["rowname"].tolist()   # c001, c001, ... (repeats 192 times)
Y_colnames = y_cols["colname"].tolist()   # 1_balancing, 1_food, ...
Y = csr_matrix(
    (y_sp["x"].values, (y_sp["i"].values - 1, y_sp["j"].values - 1)),
    shape=(26112, 1344)
)
print(f"  Y loaded ({time.time()-t0:.1f}s): {Y.shape}")

print("Loading L sparse matrix...")
t0 = time.time()
l_sp = pd.read_csv(os.path.join(IO, "L_2020_sparse.csv"))
L = csr_matrix(
    (l_sp["x"].values, (l_sp["i"].values - 1, l_sp["j"].values - 1)),
    shape=(26112, 26112)
)
print(f"  L loaded ({time.time()-t0:.1f}s): {L.shape}")

# ── Build row names for the 26112-row footprint matrix ────────────────────────
# R: expand.grid(items$comm_code, regions$area_code)
# In R's expand.grid, Var1 (comm_code) varies FAST and Var2 (area_code) varies SLOW:
#   row 0:   (c001, area1) → 'c001_1'
#   row 1:   (c002, area1) → 'c002_1'
#   ...
#   row 135: (c138, area1) → 'c138_1'
#   row 136: (c001, area2) → 'c001_2'
# This matches Y/L row ordering confirmed by Y_rownames.
# IMPORTANT: Python's itertools.product(comm, area) gives the OPPOSITE striding
# (comm slow, area fast) — use a nested comprehension to replicate R's expand.grid.
import itertools
row_names = [f"{c}_{a}"
             for a in regions["area_code"]
             for c in items["comm_code"]]   # area varies slow, comm varies fast
assert len(row_names) == 26112, f"Expected 26112, got {len(row_names)}"

# E_bd column ordering: '1_c001', '1_c002', ..., '1_c138', '2_c001', ...
# = area varies slow, comm varies fast — same striding as row_names and L rows.
eb_values = eb.values  # shape (7, 26112); columns aligned with row_names and L rows

# ── Compute L×Y ───────────────────────────────────────────────────────────────
print("Computing L × Y...")
t0 = time.time()
Ly = L @ Y   # sparse result: (26112, 1344)
print(f"  Done ({time.time()-t0:.1f}s), nnz={Ly.nnz}")

# Convert to dense for element-wise operations (26112 × 1344 ≈ 280M floats ≈ 2.2 GB)
# Use float32 to halve memory
print("Converting Ly to dense float32...")
Ly_mat = np.array(Ly.todense(), dtype=np.float32)   # (26112, 1344)
print(f"  Ly_mat shape: {Ly_mat.shape}, memory: {Ly_mat.nbytes/1e9:.2f} GB")

# ── Compute per-stressor biodiversity footprints ───────────────────────────────
# R: sweep(Ly_mat, 1, eb["landuse_bd",], "*")
# = multiply each ROW i of Ly_mat by eb["landuse_bd", i]
# eb_values[row, :] → length-26112 vector; broadcast over columns of Ly_mat
print("Computing stressor footprint matrices...")
# Each is (26112, 1344) float32

def compute_fp(Ly_mat, intensity_vec):
    """Multiply each row i of Ly_mat by intensity_vec[i]."""
    return Ly_mat * intensity_vec[:, np.newaxis]

landuse_idx = ROW_LABELS.index("landuse_bd")
water_idx   = ROW_LABELS.index("water_bd")
P_idx       = ROW_LABELS.index("P_bd")
N_idx       = ROW_LABELS.index("N_bd")
CH4_idx     = ROW_LABELS.index("CH4_bd")
CO2_idx     = ROW_LABELS.index("CO2_bd")
N2O_idx     = ROW_LABELS.index("N2O_bd")

landuse_vec = eb_values[landuse_idx, :].astype(np.float32)
water_vec   = eb_values[water_idx,   :].astype(np.float32)
P_vec       = eb_values[P_idx,       :].astype(np.float32)
N_vec       = eb_values[N_idx,       :].astype(np.float32)
CH4_vec     = eb_values[CH4_idx,     :].astype(np.float32)
CO2_vec     = eb_values[CO2_idx,     :].astype(np.float32)
N2O_vec     = eb_values[N2O_idx,     :].astype(np.float32)

bd_fp_landuse = compute_fp(Ly_mat, landuse_vec)
bd_fp_water   = compute_fp(Ly_mat, water_vec)
bd_fp_N       = compute_fp(Ly_mat, N_vec)
bd_fp_P       = compute_fp(Ly_mat, P_vec)
ghg_intensity = CH4_vec + CO2_vec + N2O_vec
bd_fp_GHG     = compute_fp(Ly_mat, ghg_intensity)

# Replace NaN with 0
for arr in [bd_fp_landuse, bd_fp_water, bd_fp_N, bd_fp_P, bd_fp_GHG]:
    np.nan_to_num(arr, copy=False)

# Sum stressors
bd_fp_fw          = bd_fp_water + bd_fp_N + bd_fp_P
bd_fp_terrestrial = bd_fp_landuse + bd_fp_GHG
bd_fp_total       = (bd_fp_fw + bd_fp_terrestrial) / 2.0

# Free large intermediates
del bd_fp_fw, bd_fp_terrestrial, bd_fp_landuse, bd_fp_water, bd_fp_N, bd_fp_P, bd_fp_GHG
print("Stressor matrices done.")

# ── Extract food consumption columns ─────────────────────────────────────────
# Y cols format: 'area_category'; food cols match '_food'
food_col_mask = [c.endswith("_food") for c in Y_colnames]
food_col_idx  = [i for i, m in enumerate(food_col_mask) if m]
# Y_colnames for food cols: '1_food', '2_food', ... → areas in area_code order
food_areas = [int(Y_colnames[i].split("_")[0]) for i in food_col_idx]

# bd_fp_food_by_country: rows=row_names, cols=area_codes (unique consumer countries)
bd_fp_food = bd_fp_total[:, food_col_idx]   # (26112, 192)
region_area_codes = regions["area_code"].tolist()  # 192 area codes
# Explicitly reorder columns to match region_area_codes order rather than
# assuming Y's food-column order matches the regions CSV order.
food_area_to_col = {a: i for i, a in enumerate(food_areas)}
reorder_idx = [food_area_to_col[a] for a in region_area_codes if a in food_area_to_col]
bd_fp_food = bd_fp_food[:, reorder_idx]
# After reordering, region_area_codes and bd_fp_food columns are aligned
assert bd_fp_food.shape[1] == len(region_area_codes), \
    f"Column count mismatch: {bd_fp_food.shape[1]} vs {len(region_area_codes)}"

# ── Filter stimulant rows ─────────────────────────────────────────────────────
# row_names: 'comm_area' → comm_code = part before '_'
row_comm = [rn.split("_")[0] for rn in row_names]
stim_row_mask = np.array([c in items_stim for c in row_comm])
bd_fp_stim = bd_fp_food[stim_row_mask, :]   # (n_stim_rows, 192)
stim_row_names = [rn for rn, m in zip(row_names, stim_row_mask) if m]
print(f"Stimulant rows: {bd_fp_stim.shape}")

# ── Top 25 consumers ──────────────────────────────────────────────────────────
col_totals = bd_fp_stim.sum(axis=0)   # (192,)
top25_idx  = np.argsort(col_totals)[::-1][:25]
top25_area_codes = [region_area_codes[i] for i in top25_idx]
print(f"Top 25 area codes: {top25_area_codes[:5]}...")

# ── Build long-format dataframe ───────────────────────────────────────────────
print("Building long-format dataframe...")
# Extract subset matrix: rows=stim rows, cols=top25 consumer areas
top25_col_positions = [region_area_codes.index(a) for a in top25_area_codes]
sub_mat = bd_fp_stim[:, top25_col_positions]   # (n_stim, 25)

# Lookup dicts
area_to_iso3  = dict(zip(regions["area_code"], regions["iso3c"]))
area_to_name  = dict(zip(regions["area_code"], regions["area"]))
name_to_iso3  = dict(zip(regions["area"], regions["iso3c"]))
name_to_area  = dict(zip(regions["area"], regions["area_code"]))
code_to_item  = dict(zip(items["comm_code"], items["item"]))
# comm_group lookup
code_to_group = dict(zip(items["comm_code"], items["comm_group"]))

# Build records
records = []
for row_i, rn in enumerate(stim_row_names):
    parts = rn.split("_")
    comm_code = parts[0]
    prod_area_code = int(parts[1])
    prod_iso3 = area_to_iso3.get(prod_area_code, str(prod_area_code))
    commodity_name = code_to_item.get(comm_code, comm_code)
    for col_j, cons_area_code in enumerate(top25_area_codes):
        val = float(sub_mat[row_i, col_j])
        if val != 0.0:
            records.append({
                "commodity":         comm_code,
                "producing_area":    prod_area_code,
                "producer_iso3":     prod_iso3,
                "commodity_name":    commodity_name,
                "consumer_area":     cons_area_code,
                "consumer_iso3":     area_to_iso3.get(cons_area_code, str(cons_area_code)),
                "footprint":         val,
            })

df = pd.DataFrame(records)
print(f"  Long df shape: {df.shape}")

# ── Summarise by (consumer, commodity_name, producer_iso3) ───────────────────
df_grp = (
    df.groupby(["consumer_iso3", "commodity_name", "producer_iso3"], as_index=False)
    ["footprint"].sum()
)

# Rank within each consumer; top-2 keep label, rest → 'other'
df_grp["rank"] = df_grp.groupby("consumer_iso3")["footprint"].rank(
    ascending=False, method="first"
)
df_grp["prod_commodity_name"] = df_grp.apply(
    lambda r: f"{r['producer_iso3']} {r['commodity_name']}" if r["rank"] <= 2 else "other",
    axis=1
)
df_summary = (
    df_grp.groupby(["consumer_iso3", "prod_commodity_name"], as_index=False)
    ["footprint"].sum()
)

# ── Simplified commodity name mapping ────────────────────────────────────────
simplified_names = {
    "Unmanufactured tobacco":            "Tobacco",
    "Cocoa beans":                       "Cocoa",
    "Coffee, green":                     "Coffee",
    "Tea leaves":                        "Tea",
    "Coffee, decaffeinated or roasted":  "Coffee",
    "Coffee extracts":                   "Coffee",
    "Coffee substitutes":                "Coffee",
    "Extracts, essences and concentrates of tea or mate, and preparations with a basis thereof or with a basis of tea or mate": "Tea",
    "Mate leaves":                       "Tea",
    "Cocoa butter, fat and oil":         "Cocoa",
    "Cocoa husks and shells":            "Cocoa",
    "Cocoa paste not defatted":          "Cocoa",
    "Cocoa powder and cake":             "Cocoa",
    "Chocolate products nes":            "Cocoa",
    "Cigars and cheroots":               "Tobacco",
    'Other manufactured tobacco and manufactured tobacco substitutes; homogenized"" or ""reconstituted"" tobacco; tobacco extracts and essences"': "Tobacco",
    "Cigarettes":                        "Tobacco",
}

GROUP_ORDER = ["Coffee", "Cocoa", "Tea", "Tobacco", "other"]

def enrich_summary(df_s):
    """Add legend_group, simplified_name, legend_label to a summary df."""
    df_s = df_s.copy()

    def parse_row(row):
        pcn = row["prod_commodity_name"]
        if pcn == "other":
            return "other", "other", "Other"
        # Format: 'ISO3 Commodity name'
        parts = pcn.split(" ", 1)
        producer = parts[0]   # ISO3
        comm     = parts[1] if len(parts) > 1 else ""
        simplified = simplified_names.get(comm, comm)
        # If the simplified name isn't in known groups, mark as other
        if simplified not in GROUP_ORDER[:-1]:
            simplified = "other"
        label = f"{simplified} {producer}"
        return simplified, producer, label

    parsed = df_s.apply(parse_row, axis=1, result_type="expand")
    parsed.columns = ["simplified_name", "producer", "legend_label"]
    df_s = pd.concat([df_s, parsed], axis=1)

    # Order by legend group
    group_order_map = {g: i for i, g in enumerate(GROUP_ORDER)}
    df_s["legend_group"]     = df_s["simplified_name"]
    df_s["legend_group_ord"] = df_s["simplified_name"].map(group_order_map).fillna(99)
    df_s = df_s.sort_values("legend_group_ord")
    df_s["legend_label"] = df_s["legend_label"].replace("other other", "Other")

    # Assign unique legend_label ordering consistent with group order
    seen = []
    for ll in df_s["legend_label"].tolist():
        if ll not in seen:
            seen.append(ll)
    df_s["legend_label_cat"] = pd.Categorical(df_s["legend_label"], categories=seen, ordered=True)
    return df_s

df_summary = enrich_summary(df_summary)
print(f"Unique legend labels: {df_summary['legend_label'].unique()}")

# ── Color palette ─────────────────────────────────────────────────────────────
def make_color_palette(df_s):
    """Generate colors for each unique legend_label following commodity group palettes."""
    palette_fns = {
        "Coffee":  plt.cm.Reds,
        "Cocoa":   plt.cm.Greens,
        "Tea":     plt.cm.Blues,
        "Tobacco": plt.cm.Purples,
        "other":   None,
    }
    # color range: avoid too light or too dark
    CRANGE = {"Coffee": (0.3, 0.85), "Cocoa": (0.3, 0.85),
              "Tea": (0.3, 0.85), "Tobacco": (0.4, 0.85)}

    color_map = {}
    for grp in GROUP_ORDER:
        labels_in_grp = [ll for ll in df_s["legend_label_cat"].cat.categories
                         if df_s.loc[df_s["legend_label"] == ll, "legend_group"].iloc[0] == grp
                         if df_s["legend_label"].eq(ll).any()]
        n = len(labels_in_grp)
        if grp == "other":
            for ll in labels_in_grp:
                color_map[ll] = "#d3d3d3"
        else:
            cmap = palette_fns[grp]
            lo, hi = CRANGE.get(grp, (0.3, 0.85))
            vals = np.linspace(lo, hi, max(n, 1))
            for ll, v in zip(labels_in_grp, vals):
                color_map[ll] = mcolors.to_hex(cmap(v))
    return color_map


# ── Hatch pattern assignment ──────────────────────────────────────────────────
# Repeat each hatch character 3× so the pattern is dense enough to be
# readable in small legend patches and in the bars themselves.
HATCH_OPTS = ['///', '\\\\\\\\\\\\', '|||', '---', 'xxx', '+++']

def make_hatch_map(df_s):
    hatch_map = {}
    for grp in GROUP_ORDER:
        labels_in_grp = [ll for ll in df_s["legend_label_cat"].cat.categories
                         if df_s.loc[df_s["legend_label"] == ll, "legend_group"].iloc[0] == grp
                         if df_s["legend_label"].eq(ll).any()]
        for i, ll in enumerate(labels_in_grp):
            if grp == "other":
                hatch_map[ll] = None
            else:
                hatch_map[ll] = HATCH_OPTS[i % len(HATCH_OPTS)]
    return hatch_map


# ── Pivot for stacked bar ─────────────────────────────────────────────────────
def make_plot_df(df_s):
    """Return pivoted df (consumers as index, legend_labels as cols) sorted by total desc."""
    # consumer total order
    consumer_order = (
        df_s.groupby("consumer_iso3")["footprint"].sum()
        .sort_values(ascending=False).index.tolist()
    )
    legend_order = list(df_s["legend_label_cat"].cat.categories)

    pivot = df_s.pivot_table(
        index="consumer_iso3", columns="legend_label", values="footprint",
        aggfunc="sum", fill_value=0.0
    )
    pivot = pivot.reindex(index=consumer_order, columns=legend_order, fill_value=0.0)
    return pivot, consumer_order, legend_order


# ── Broken-axis stacked bar plot ──────────────────────────────────────────────
FIG_W = 180 / 25.4
FIG_H = 140 / 25.4

def draw_stacked_bar(df_s, ylabel, fname_stem):
    """Draw stacked bar chart; handles axis break if tallest > 2.5× second."""
    import math
    from matplotlib.ticker import FuncFormatter

    def _sci_tick(v, p):
        """Format tick as e.g. 1.0×10⁻³ using mathtext superscript."""
        if v == 0:
            return "0"
        e = int(math.floor(math.log10(abs(v))))
        c = v / 10**e
        return f"${c:.1f}\\!\\times\\!10^{{{e}}}$"

    sci_fmt = FuncFormatter(_sci_tick)

    color_map = make_color_palette(df_s)
    hatch_map = make_hatch_map(df_s)
    pivot, consumer_order, legend_order = make_plot_df(df_s)

    bar_totals = pivot.sum(axis=1).values   # length 25, sorted desc
    y1 = bar_totals[0]
    y2 = bar_totals[1] if len(bar_totals) > 1 else y1
    need_break = y1 > 2.5 * y2

    if not need_break:
        fig, ax = plt.subplots(figsize=(FIG_W, FIG_H))
        fig.patch.set_facecolor("white")

        bottom = np.zeros(len(pivot))
        bars_drawn = {}   # legend_label → patch
        # Draw "Other" first so it sits at the bottom of every bar
        draw_order = [col for col in legend_order if col == "Other"] + \
                     [col for col in legend_order if col != "Other"]
        for col in draw_order:
            if col not in pivot.columns:
                continue
            vals = pivot[col].values.astype(float)
            color = color_map.get(col, "#999999")
            hatch = hatch_map.get(col, None)
            patches = ax.bar(
                range(len(pivot)), vals, bottom=bottom,
                color=color, hatch=hatch, edgecolor="grey",
                linewidth=0.5, label=col
            )
            bars_drawn[col] = patches[0]
            bottom += vals

        ax.set_xticks(range(len(pivot)))
        ax.set_xticklabels(consumer_order, rotation=45, ha="right", fontsize=7)
        ax.set_ylabel(ylabel, fontsize=9)
        ax.set_xlabel("Consuming Country", fontsize=9)
        ax.yaxis.set_major_formatter(sci_fmt)
        ax.set_facecolor("white")
        ax.grid(False)
        for spine in ax.spines.values():
            spine.set_visible(True)

        # Legend inside upper-right of the axes
        handles = [mpatches.Patch(facecolor=color_map.get(ll, "#999"),
                                  hatch=hatch_map.get(ll, None),
                                  edgecolor="grey", linewidth=0.5,
                                  label=ll)
                   for ll in legend_order if ll in color_map]
        leg = ax.legend(handles=handles, title="Commodity & Producer",
                        loc="upper right",
                        ncol=3,
                        fontsize=6, title_fontsize=8,
                        handlelength=3.5, handleheight=2.2,
                        frameon=True, framealpha=1.0,
                        edgecolor="grey")
        leg.get_frame().set_facecolor("white")

        fig.tight_layout()

    else:
        # ── Broken axis ───────────────────────────────────────────────────────
        y_cap   = y2 * 1.25
        gap_lo  = y_cap * 0.910
        gap_hi  = y_cap * 0.975

        # Use a fixed height split so the lower comparison panel always gets
        # the majority of the figure regardless of how large the outlier is.
        # (Data-proportional ratios give the outlier stub most of the height.)
        BOT_FRAC = 0.78          # fraction of panel height for lower (comparison) panel
        height_ratio_top = 1 - BOT_FRAC
        height_ratio_bot = BOT_FRAC

        fig, (ax_top, ax_bot) = plt.subplots(
            2, 1,
            figsize=(FIG_W, FIG_H),
            gridspec_kw={"height_ratios": [height_ratio_top, height_ratio_bot],
                         "hspace": 0.05},
            sharex=True
        )
        fig.patch.set_facecolor("white")

        # Draw bars on BOTH axes
        bottom_t = np.zeros(len(pivot))
        bottom_b = np.zeros(len(pivot))
        handles_list = []

        # Draw "Other" first so it sits at the bottom of every bar
        draw_order = [col for col in legend_order if col == "Other"] + \
                     [col for col in legend_order if col != "Other"]
        for col in draw_order:
            if col not in pivot.columns:
                continue
            vals = pivot[col].values.astype(float)
            color = color_map.get(col, "#999999")
            hatch = hatch_map.get(col, None)
            kw = dict(color=color, hatch=hatch, edgecolor="grey",
                      linewidth=0.5)
            ax_top.bar(range(len(pivot)), vals, bottom=bottom_t, **kw)
            ax_bot.bar(range(len(pivot)), vals, bottom=bottom_b, **kw, label=col)
            bottom_t += vals
            bottom_b += vals
            handles_list.append(
                mpatches.Patch(facecolor=color, hatch=hatch,
                               edgecolor="grey", linewidth=0.5, label=col)
            )

        # Set y limits
        ax_top.set_ylim(gap_hi, y1 * 1.05)
        ax_bot.set_ylim(0,      gap_lo)

        # Hide inner spines
        ax_top.spines["bottom"].set_visible(False)
        ax_bot.spines["top"].set_visible(False)
        ax_top.tick_params(labelbottom=False, bottom=False)

        # Horizontal break marks — two short horizontal lines spanning the left
        # spine, drawn in figure-fraction space at the panel boundary.
        # left=0.22 gives enough room for the wide mathtext tick labels (e.g. 1.0×10⁻³)
        # without them running into the rotated y-axis title at x=0.04.
        fig.subplots_adjust(left=0.22, right=0.97, top=0.95, bottom=0.22)

        from matplotlib.lines import Line2D

        # y positions of the two inner edges in figure fraction
        y_top_bot = ax_top.get_position().y0   # bottom edge of upper panel
        y_bot_top = ax_bot.get_position().y1   # top edge of lower panel

        # Break marks on left spine only (where the y-axis labels are)
        x_spine_left = ax_bot.get_position().x0
        half_w = 0.018
        for y_pos in (y_top_bot, y_bot_top):
            fig.add_artist(Line2D(
                [x_spine_left - half_w, x_spine_left + half_w],
                [y_pos, y_pos],
                transform=fig.transFigure, color="grey",
                linewidth=1.0, clip_on=False
            ))

        # x-axis labels on bottom panel
        ax_bot.set_xticks(range(len(pivot)))
        ax_bot.set_xticklabels(consumer_order, rotation=45, ha="right", fontsize=7)
        ax_bot.set_xlabel("Consuming Country", fontsize=9)

        # y-axis label centred across both panels; x=0.04 sits clear of the
        # tick labels which now end around x=0.22 (the left margin)
        fig.text(0.04, 0.5, ylabel, va="center", ha="center",
                 rotation="vertical", fontsize=9)

        for ax in (ax_top, ax_bot):
            ax.set_facecolor("white")
            ax.grid(False)
            ax.yaxis.set_major_formatter(sci_fmt)

        # Legend anchored in figure space so it is never clipped by a panel edge.
        # bbox_to_anchor places the legend's top-right corner at (0.97, 0.95) in
        # figure fraction — inside the overall figure with room to expand downward.
        leg = fig.legend(
            handles=handles_list,
            title="Commodity & Producer",
            loc="upper right",
            bbox_to_anchor=(0.97, 0.95),
            bbox_transform=fig.transFigure,
            ncol=3,
            fontsize=6, title_fontsize=8,
            handlelength=3.5, handleheight=2.2,
            frameon=True, framealpha=1.0,
            edgecolor="grey",
        )
        leg.get_frame().set_facecolor("white")

    # Save
    png_path = os.path.join(OUT, f"{fname_stem}.png")
    svg_path = os.path.join(OUT, f"{fname_stem}.svg")
    fig.savefig(png_path, dpi=300, bbox_inches="tight", facecolor="white")
    fig.savefig(svg_path, bbox_inches="tight", facecolor="white")
    plt.close(fig)
    print(f"  Saved: {png_path}")
    print(f"  Saved: {svg_path}")


# ── Plot 1: Total footprint ───────────────────────────────────────────────────
print("\nPlotting total footprint by country...")
draw_stacked_bar(
    df_summary,
    ylabel="Biodiversity Footprint (PDF·yr)",
    fname_stem="cons_country_stacked_bar_py"
)

# ── Per-capita: load Natural Earth population estimates ───────────────────────
# The R script uses world_blank$pop_est from ne_countries(); we use the
# pre-exported inst/ne_pop_est.csv (columns: iso_a3, pop_est).
print("\nAttempting per-capita plot...")

NE_POP_PATH = os.path.join(BASE, "inst", "ne_pop_est.csv")
ne_pop = None
if os.path.exists(NE_POP_PATH):
    ne_pop = pd.read_csv(NE_POP_PATH)
    ne_pop["pop_est"] = pd.to_numeric(ne_pop["pop_est"], errors="coerce")
    # Normalise: Natural Earth ISO_A3 → match FABIO iso3c where possible
    ne_pop = ne_pop.rename(columns={"iso_a3": "iso3c"})
    ne_pop = ne_pop[ne_pop["iso3c"] != "-99"].dropna(subset=["pop_est"])
    print(f"  Loaded Natural Earth pop_est: {len(ne_pop)} countries")
else:
    print("  ne_pop_est.csv not found — skipping per-capita plot.")

pop2017 = ne_pop  # alias for the rest of the block (same structure: iso3c, pop_est)

if ne_pop is None:
    pass
else:

    # All country total footprints
    col_totals_all = pd.Series(
        bd_fp_stim.sum(axis=0), index=[str(a) for a in region_area_codes]
    )
    df_percap = pd.DataFrame({
        "area_code": region_area_codes,
        "total_footprint": bd_fp_stim.sum(axis=0)
    })
    df_percap["iso3c"] = df_percap["area_code"].map(area_to_iso3)
    df_percap = df_percap.merge(pop2017[["iso3c", "pop_est"]], on="iso3c", how="left")
    df_percap = df_percap.dropna(subset=["pop_est"])
    df_percap = df_percap[df_percap["pop_est"] >= 1_000_000]
    df_percap["fp_per_cap"] = df_percap.apply(
        lambda r: 0 if r["pop_est"] == 0 else r["total_footprint"] / r["pop_est"], axis=1
    )
    df_percap = df_percap.sort_values("fp_per_cap", ascending=False).head(25)
    top25_pc_iso3 = df_percap["iso3c"].tolist()
    top25_pc_areas = df_percap["area_code"].tolist()

    # Build long df for per-cap top 25
    top25_pc_positions = [region_area_codes.index(a) for a in top25_pc_areas]
    sub_mat_pc = bd_fp_stim[:, top25_pc_positions]
    records_pc = []
    for row_i, rn in enumerate(stim_row_names):
        parts = rn.split("_")
        comm_code = parts[0]
        prod_area_code = int(parts[1])
        prod_iso3 = area_to_iso3.get(prod_area_code, str(prod_area_code))
        commodity_name = code_to_item.get(comm_code, comm_code)
        for col_j, cons_area_code in enumerate(top25_pc_areas):
            val = float(sub_mat_pc[row_i, col_j])
            if val != 0.0:
                records_pc.append({
                    "commodity":      comm_code,
                    "producer_iso3":  prod_iso3,
                    "commodity_name": commodity_name,
                    "consumer_area":  cons_area_code,
                    "consumer_iso3":  area_to_iso3.get(cons_area_code, str(cons_area_code)),
                    "footprint":      val,
                })

    df_pc = pd.DataFrame(records_pc)
    df_grp_pc = (
        df_pc.groupby(["consumer_iso3", "commodity_name", "producer_iso3"], as_index=False)
        ["footprint"].sum()
    )
    df_grp_pc["rank"] = df_grp_pc.groupby("consumer_iso3")["footprint"].rank(
        ascending=False, method="first"
    )
    df_grp_pc["prod_commodity_name"] = df_grp_pc.apply(
        lambda r: f"{r['producer_iso3']} {r['commodity_name']}" if r["rank"] <= 2 else "other",
        axis=1
    )
    df_summary_pc = (
        df_grp_pc.groupby(["consumer_iso3", "prod_commodity_name"], as_index=False)
        ["footprint"].sum()
    )
    # Per-capita division
    pop_map = dict(zip(df_percap["iso3c"], df_percap["pop_est"]))
    df_summary_pc["footprint"] = df_summary_pc.apply(
        lambda r: r["footprint"] / pop_map.get(r["consumer_iso3"], 1), axis=1
    )

    df_summary_pc = enrich_summary(df_summary_pc)

    print("Plotting per-capita footprint by country...")
    draw_stacked_bar(
        df_summary_pc,
        ylabel="Biodiversity Footprint per Capita (PDF·yr / cap)",
        fname_stem="cons_country_stacked_bar_per_cap_py"
    )

# ── Per-GDP: biodiversity footprint per million USD GDP ──────────────────────
# Uses gdp_md (GDP in M USD) from the same ne_pop_est.csv as the per-capita plot.
print("\nAttempting per-GDP plot...")

if ne_pop is None:
    print("  ne_pop_est.csv not found — skipping per-GDP plot.")
else:
    ne_gdp = ne_pop[["iso3c", "gdp_md"]].copy()
    ne_gdp["gdp_md"] = pd.to_numeric(ne_gdp["gdp_md"], errors="coerce")
    ne_gdp = ne_gdp.dropna(subset=["gdp_md"])

    df_pergdp = pd.DataFrame({
        "area_code":       region_area_codes,
        "total_footprint": bd_fp_stim.sum(axis=0)
    })
    df_pergdp["iso3c"] = df_pergdp["area_code"].map(area_to_iso3)
    df_pergdp = df_pergdp.merge(ne_gdp, on="iso3c", how="left")
    df_pergdp = df_pergdp.dropna(subset=["gdp_md"])
    # Exclude micro-economies (GDP < $1 billion = 1000 M USD)
    df_pergdp = df_pergdp[df_pergdp["gdp_md"] >= 1_000]
    df_pergdp["fp_per_gdp"] = df_pergdp.apply(
        lambda r: 0 if r["gdp_md"] == 0 else r["total_footprint"] / r["gdp_md"], axis=1
    )
    df_pergdp = df_pergdp.sort_values("fp_per_gdp", ascending=False).head(25)
    top25_gdp_areas = df_pergdp["area_code"].tolist()

    # Build long df for the top-25 per-GDP countries
    top25_gdp_positions = [region_area_codes.index(a) for a in top25_gdp_areas]
    sub_mat_gdp = bd_fp_stim[:, top25_gdp_positions]
    records_gdp = []
    for row_i, rn in enumerate(stim_row_names):
        parts = rn.split("_")
        comm_code      = parts[0]
        prod_area_code = int(parts[1])
        prod_iso3      = area_to_iso3.get(prod_area_code, str(prod_area_code))
        commodity_name = code_to_item.get(comm_code, comm_code)
        for col_j, cons_area_code in enumerate(top25_gdp_areas):
            val = float(sub_mat_gdp[row_i, col_j])
            if val != 0.0:
                records_gdp.append({
                    "commodity":      comm_code,
                    "producer_iso3":  prod_iso3,
                    "commodity_name": commodity_name,
                    "consumer_area":  cons_area_code,
                    "consumer_iso3":  area_to_iso3.get(cons_area_code, str(cons_area_code)),
                    "footprint":      val,
                })

    df_gdp = pd.DataFrame(records_gdp)
    df_grp_gdp = (
        df_gdp.groupby(["consumer_iso3", "commodity_name", "producer_iso3"], as_index=False)
        ["footprint"].sum()
    )
    df_grp_gdp["rank"] = df_grp_gdp.groupby("consumer_iso3")["footprint"].rank(
        ascending=False, method="first"
    )
    df_grp_gdp["prod_commodity_name"] = df_grp_gdp.apply(
        lambda r: f"{r['producer_iso3']} {r['commodity_name']}" if r["rank"] <= 2 else "other",
        axis=1
    )
    df_summary_gdp = (
        df_grp_gdp.groupby(["consumer_iso3", "prod_commodity_name"], as_index=False)
        ["footprint"].sum()
    )
    # Divide each consumer's footprint by their GDP
    gdp_map = dict(zip(df_pergdp["iso3c"], df_pergdp["gdp_md"]))
    df_summary_gdp["footprint"] = df_summary_gdp.apply(
        lambda r: r["footprint"] / gdp_map.get(r["consumer_iso3"], 1), axis=1
    )

    df_summary_gdp = enrich_summary(df_summary_gdp)

    print("Plotting per-GDP footprint by country...")
    draw_stacked_bar(
        df_summary_gdp,
        ylabel="Biodiversity Footprint per GDP (PDF·yr / M USD)",
        fname_stem="cons_country_stacked_bar_per_gdp_py"
    )

print("\nAll done.")
