library(readxl)
library(janitor)
library(dplyr)
library(stringr)
library(lavaan)
library(writexl)
library(tibble)

df <- read_excel("all_cfa_sem.xlsx") |> clean_names()

items <- c(
  # assortment
  "assortment_item_category_coverage_likert",
  "assortment_item_price_range_likert",
  "assortment_item_wide_choice_likert",
  # benefit
  "benefit_item_choose_same_price_likert",
  "benefit_item_saves_money_likert",
  "benefit_item_value_money_likert",
  # uniqueness
  "uniqueness_item_new_interest_likert",
  "uniqueness_item_unique_features_likert",
  "uniqueness_item_visit_for_pl_likert"
)

missing_cols <- setdiff(items, names(df))
if (length(missing_cols) > 0) stop("Missing columns:\n", paste(missing_cols, collapse = "\n"))

cfa_df <- df |>
  select(all_of(items)) |>
  mutate(across(everything(), \(x) {
    if (is.ordered(x)) return(x)
    if (is.factor(x))  return(as.ordered(x))
    vals <- suppressWarnings(as.numeric(as.character(x)))
    if (all(is.na(vals))) return(as.ordered(x))
    as.ordered(vals)
  })) |>
  tidyr::drop_na()

ordered_items <- names(cfa_df)

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

fit <- cfa(
  model = model_cfa,
  data  = cfa_df,
  ordered = ordered_items,
  estimator = "WLSMV",
  std.lv = TRUE 
)

pe_std <- parameterEstimates(fit, standardized = TRUE)

loadings_all <- pe_std |>
  filter(op == "=~") |>
  transmute(
    factor = lhs,          
    indicator = rhs,       
    loading_unstd = est,
    loading_std  = std.all,
    se = se,
    z  = z,
    p_value = pvalue
  ) |>
  arrange(factor, desc(abs(loading_std)))

loadings_first_order <- loadings_all |>
  filter(!indicator %in% c("assortment", "benefit", "uniqueness"))

loadings_second_order <- loadings_all |>
  filter(indicator %in% c("assortment", "benefit", "uniqueness"))

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

print(fit_measures)
print(loadings_second_order)
