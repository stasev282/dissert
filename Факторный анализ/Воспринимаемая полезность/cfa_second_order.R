library(readxl)
library(janitor)
library(lavaan)
library(dplyr)
library(writexl)

# -------------------------
# 0) load data
# -------------------------
all_q <- read_excel("all_cfa_sem.xlsx") |> clean_names()

# -------------------------
# 1) CFA model (2nd-order)
# -------------------------
model_2nd <- '
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
  perceived_value =~
    assortment +
    benefit +
    uniqueness
'

# -------------------------
# 2) fit CFA (ordinal, WLSMV)
# -------------------------
ordered_items <- c(
  "assortment_item_category_coverage_likert",
  "assortment_item_price_range_likert",
  "assortment_item_wide_choice_likert",
  "benefit_item_choose_same_price_likert",
  "benefit_item_saves_money_likert",
  "benefit_item_value_money_likert",
  "uniqueness_item_new_interest_likert",
  "uniqueness_item_unique_features_likert",
  "uniqueness_item_visit_for_pl_likert"
)

fit_2nd <- cfa(
  model     = model_2nd,
  data      = all_q,
  ordered   = ordered_items,
  estimator = "WLSMV",
  std.lv    = TRUE
)

# -------------------------
# 3) fit measures
# -------------------------
cat("\n--- FIT MEASURES: 2nd-Order ---\n")
fit_measures <- fitMeasures(
  fit_2nd,
  c("cfi", "tli", "rmsea", "rmsea.ci.lower", "rmsea.ci.upper", "srmr", "chisq", "df")
)
print(fit_measures)

fit_measures_tbl <- tibble(
  metric = names(fit_measures),
  value  = as.numeric(fit_measures)
)

# -------------------------
# 4) standardized loadings (1st + 2nd order)
# -------------------------
pe <- parameterEstimates(fit_2nd, standardized = TRUE)

# 1st-order loadings: items
loadings_1st <- pe |>
  filter(op == "=~", lhs %in% c("assortment", "benefit", "uniqueness")) |>
  select(factor = lhs, indicator = rhs, loading_std = std.all)

cat("\n--- Standardized loadings (1st-order) ---\n")
print(loadings_1st)

# 2nd-order loadings: factors
loadings_2nd <- pe |>
  filter(op == "=~", lhs == "perceived_value", rhs %in% c("assortment", "benefit", "uniqueness")) |>
  select(factor = lhs, indicator = rhs, loading_std = std.all)

cat("\n--- Standardized loadings (2nd-order) ---\n")
print(loadings_2nd)

# -------------------------
# 5) Cronbach's alpha (manual) for 1st-order factors
#    (как у тебя: numeric + ковариация)
# -------------------------
alpha_manual <- function(df_numeric) {
  df_numeric <- df_numeric[complete.cases(df_numeric), , drop = FALSE]
  S <- cov(df_numeric, use = "pairwise.complete.obs")
  k <- ncol(df_numeric)
  (k / (k - 1)) * (1 - sum(diag(S)) / sum(S))
}

assortment_df <- all_q |>
  select(
    assortment_item_category_coverage_likert,
    assortment_item_price_range_likert,
    assortment_item_wide_choice_likert
  ) |>
  mutate(across(everything(), as.numeric))

benefit_df <- all_q |>
  select(
    benefit_item_choose_same_price_likert,
    benefit_item_saves_money_likert,
    benefit_item_value_money_likert
  ) |>
  mutate(across(everything(), as.numeric))

uniqueness_df <- all_q |>
  select(
    uniqueness_item_new_interest_likert,
    uniqueness_item_unique_features_likert,
    uniqueness_item_visit_for_pl_likert
  ) |>
  mutate(across(everything(), as.numeric))

alpha_assortment <- alpha_manual(assortment_df)
alpha_benefit    <- alpha_manual(benefit_df)
alpha_uniqueness <- alpha_manual(uniqueness_df)

# alpha for 2nd-order factor perceived_value:
# по аналогии: берем "индикаторы" 2-го порядка (3 первичных фактора),
# но данных по ним нет -> берем факторные оценки (lv scores) и считаем альфу на них.
lv_scores <- lavPredict(fit_2nd, type = "lv")
pv_df <- as.data.frame(lv_scores[, c("assortment", "benefit", "uniqueness")])
alpha_perceived_value <- alpha_manual(pv_df)

alpha_tbl <- tibble(
  factor = c("assortment", "benefit", "uniqueness", "perceived_value"),
  cronbach_alpha = c(alpha_assortment, alpha_benefit, alpha_uniqueness, alpha_perceived_value)
)

cat("\n--- Cronbach's alpha (manual) ---\n")
print(alpha_tbl)

# -------------------------
# 6) Composite Reliability (CR) from standardized loadings
#    (как у тебя: (sum(l))^2 / ((sum(l))^2 + sum(1 - l^2)))
# -------------------------
cr_from_loadings <- function(loadings_vec) {
  (sum(loadings_vec))^2 / ((sum(loadings_vec))^2 + sum(1 - loadings_vec^2))
}

assortment_l <- loadings_1st |>
  filter(factor == "assortment") |>
  pull(loading_std)

benefit_l <- loadings_1st |>
  filter(factor == "benefit") |>
  pull(loading_std)

uniqueness_l <- loadings_1st |>
  filter(factor == "uniqueness") |>
  pull(loading_std)

perceived_value_l <- loadings_2nd |>
  pull(loading_std)

cr_tbl <- tibble(
  factor = c("assortment", "benefit", "uniqueness", "perceived_value"),
  CR = c(
    cr_from_loadings(assortment_l),
    cr_from_loadings(benefit_l),
    cr_from_loadings(uniqueness_l),
    cr_from_loadings(perceived_value_l)
  )
)

cat("\n--- Composite Reliability (CR) ---\n")
print(cr_tbl)

# -------------------------
# 7) pack reliability (alpha + CR)
# -------------------------
reliability_tbl <- alpha_tbl |>
  left_join(cr_tbl, by = "factor")

# -------------------------
# 8) export to Excel (2 sheets)
#    Sheet1: 1st-order loadings
#    Sheet2: 2nd-order loadings + alpha/CR + fit
# -------------------------
sheet1 <- loadings_1st |>
  arrange(factor, desc(abs(loading_std)))

sheet2 <- bind_rows(
  loadings_2nd |>
    mutate(section = "2nd_order_loadings") |>
    select(section, factor, indicator, value = loading_std),
  reliability_tbl |>
    transmute(section = "reliability", factor, indicator = "cronbach_alpha", value = cronbach_alpha),
  reliability_tbl |>
    transmute(section = "reliability", factor, indicator = "CR", value = CR),
  fit_measures_tbl |>
    transmute(section = "fit", factor = "model", indicator = metric, value)
)

out_file <- "cfa_second_order_perceived_value_wlsmv.xlsx"

write_xlsx(
  x = list(
    loadings_1st_order = sheet1,
    second_order_and_quality = sheet2
  ),
  path = out_file
)

cat("\n==============================\n")
cat("Saved Excel:", out_file, "\n")
cat("Sheets: loadings_1st_order, second_order_and_quality\n")
cat("==============================\n")
