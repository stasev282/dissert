# ============================================================
# CFA (lavaan) — Second-order factor loadings included
# - 1st order: assortment (3), benefit (3), uniqueness (3)
# - 2nd order: perceived_value =~ assortment + benefit + uniqueness
# - Outputs: standardized loadings for BOTH levels + fit indices
# ============================================================

library(readxl)
library(janitor)
library(dplyr)
library(stringr)
library(lavaan)
library(writexl)
library(tibble)

# -------------------------
# 0) load data
# -------------------------
df <- read_excel("all_cfa_sem.xlsx") |> clean_names()

# -------------------------
# 1) variables (LIKERT items)
# -------------------------
items <- c(
  # assortment (3)
  "assortment_item_category_coverage_likert",
  "assortment_item_price_range_likert",
  "assortment_item_wide_choice_likert",
  # benefit (3)
  "benefit_item_choose_same_price_likert",
  "benefit_item_saves_money_likert",
  "benefit_item_value_money_likert",
  # uniqueness (3)
  "uniqueness_item_new_interest_likert",
  "uniqueness_item_unique_features_likert",
  "uniqueness_item_visit_for_pl_likert"
)

missing_cols <- setdiff(items, names(df))
if (length(missing_cols) > 0) stop("Missing columns:\n", paste(missing_cols, collapse = "\n"))

# -------------------------
# 2) prep: keep items, coerce to ordered (recommended for Likert)
#    - if your columns are already numeric 1..5, this still works
# -------------------------
cfa_df <- df |>
  select(all_of(items)) |>
  mutate(across(everything(), \(x) {
    # keep only non-missing; turn into ordered factors
    if (is.ordered(x)) return(x)
    if (is.factor(x))  return(as.ordered(x))
    # numeric/character -> ordered with sorted unique levels
    vals <- suppressWarnings(as.numeric(as.character(x)))
    # if coercion fails (all NA), keep as is to throw later
    if (all(is.na(vals))) return(as.ordered(x))
    as.ordered(vals)
  })) |>
  tidyr::drop_na()

ordered_items <- names(cfa_df)

# -------------------------
# 3) Second-order CFA model
# -------------------------
model_cfa <- "
# first-order
assortment =~
  assortment_item_category_coverage_likert +
  assortment_item_price_range_likert +
  assortment_item_wide_choice_likert

benefit =~
  benefit_item_choose_same_price_likert +
  benefit_item_saves_money_likert +
  benefit_item_value_money_likert

uniqueness =~
  uniqueness_item_new_interest_likert +
  uniqueness_item_unique_features_likert +
  uniqueness_item_visit_for_pl_likert

# second-order
perceived_value =~ assortment + benefit + uniqueness
"

# -------------------------
# 4) fit CFA
#    WLSMV is standard for ordered Likert
# -------------------------
fit <- cfa(
  model = model_cfa,
  data  = cfa_df,
  ordered = ordered_items,
  estimator = "WLSMV",
  std.lv = TRUE # fixes latent variances to 1 => clean standardized loadings
)

# -------------------------
# 5) Extract loadings (standardized) INCLUDING second-order
# -------------------------
pe_std <- parameterEstimates(fit, standardized = TRUE)

# all loadings (both levels): op == "=~"
loadings_all <- pe_std |>
  filter(op == "=~") |>
  transmute(
    factor = lhs,          # latent factor being measured
    indicator = rhs,       # item OR first-order factor (for 2nd order)
    loading_unstd = est,
    loading_std  = std.all,
    se = se,
    z  = z,
    p_value = pvalue
  ) |>
  arrange(factor, desc(abs(loading_std)))

# split for convenience
loadings_first_order <- loadings_all |>
  filter(!indicator %in% c("assortment", "benefit", "uniqueness"))

loadings_second_order <- loadings_all |>
  filter(indicator %in% c("assortment", "benefit", "uniqueness"))

# -------------------------
# 6) Fit measures (common set)
# -------------------------
fit_measures <- tibble(
  n = lavaan::nobs(fit),
  chisq = fitMeasures(fit, "chisq"),
  df = fitMeasures(fit, "df"),
  pvalue = fitMeasures(fit, "pvalue"),
  cfi = fitMeasures(fit, "cfi"),
  tli = fitMeasures(fit, "tli"),
  rmsea = fitMeasures(fit, "rmsea"),
  rmsea_ci_lower = fitMeasures(fit, "rmsea.ci.lower"),
  rmsea_ci_upper = fitMeasures(fit, "rmsea.ci.upper"),
  srmr = fitMeasures(fit, "srmr")
)

# -------------------------
# 7) Export to Excel
# -------------------------
out_file <- "cfa_second_order_loadings.xlsx"

write_xlsx(
  x = list(
    fit_measures = fit_measures,
    loadings_second_order = loadings_second_order,
    loadings_first_order  = loadings_first_order,
    loadings_all = loadings_all
  ),
  path = out_file
)

cat("\n==============================\n")
cat("Saved Excel:", out_file, "\n")
cat("Sheets: fit_measures, loadings_second_order, loadings_first_order, loadings_all\n")
cat("==============================\n")

# Optional: quick console peek
print(fit_measures)
print(loadings_second_order)
