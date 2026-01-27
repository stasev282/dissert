library(lavaan)
library(dplyr)

# -------------------------
# 1) CFA model (1-factor)
# -------------------------
model_1f <- '
general =~
  assortment_item_category_coverage_likert +
  assortment_item_price_range_likert +
  assortment_item_wide_choice_likert +
  benefit_item_choose_same_price_likert +
  benefit_item_saves_money_likert +
  benefit_item_value_money_likert +
  uniqueness_item_new_interest_likert +
  uniqueness_item_unique_features_likert +
  uniqueness_item_visit_for_pl_likert
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

fit_1f <- cfa(
  model = model_1f,
  data = all_q,
  ordered = ordered_items,
  estimator = "WLSMV",
  std.lv = TRUE
)

# -------------------------
# 3) fit measures
# -------------------------
cat("\n--- FIT MEASURES: 1-Factor ---\n")
print(fitMeasures(fit_1f, c("cfi", "tli", "rmsea", "rmsea.ci.lower", "rmsea.ci.upper", "srmr", "chisq", "df")))

# -------------------------
# 4) standardized loadings (1-factor)
# -------------------------
loadings_1f <- parameterEstimates(fit_1f, standardized = TRUE) |>
  filter(op == "=~") |>
  select(lhs, rhs, std.all)

cat("\n--- Standardized loadings (1-Factor) ---\n")
print(loadings_1f)

# -------------------------
# 5) Cronbach's alpha (manual, one factor)
# -------------------------
general_df <- all_q |>
  select(
    assortment_item_category_coverage_likert,
    assortment_item_price_range_likert,
    assortment_item_wide_choice_likert,
    benefit_item_choose_same_price_likert,
    benefit_item_saves_money_likert,
    benefit_item_value_money_likert,
    uniqueness_item_new_interest_likert,
    uniqueness_item_unique_features_likert,
    uniqueness_item_visit_for_pl_likert
  ) |>
  mutate(across(everything(), as.numeric))
general_df <- general_df[complete.cases(general_df), , drop = FALSE]
S_general <- cov(general_df, use = "pairwise.complete.obs")
k_general <- ncol(general_df)
alpha_general <- (k_general / (k_general - 1)) * (1 - sum(diag(S_general)) / sum(S_general))

cat("\n--- Cronbach's alpha (1-Factor) ---\n")
print(data.frame(factor = "general", cronbach_alpha = alpha_general))

# -------------------------
# 6) Composite Reliability (CR) from standardized loadings
# -------------------------
general_l <- loadings_1f |>
  filter(lhs == "general") |>
  pull(std.all)
cr_general <- (sum(general_l))^2 / ((sum(general_l))^2 + sum(1 - general_l^2))

cat("\n--- Composite Reliability (CR) ---\n")
print(data.frame(factor = "general", CR = cr_general))

# -------------------------
# 7) short reading notes (no numeric interpretation)
# -------------------------
cat("\n--- How to read the reliability/validity metrics ---\n")
cat("Cronbach's alpha: internal consistency within each factor (higher = more consistent).\n")
cat("CR (Composite Reliability): reliability based on standardized loadings (higher = more consistent).\n")
cat("AVE is not computed here because the request focused on alpha and CR.\n")
