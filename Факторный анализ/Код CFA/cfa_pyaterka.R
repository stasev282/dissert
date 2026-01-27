library(tidyverse)
library(readxl)
library(lavaan)
library(semTools)
library(janitor)
library(writexl)

all_df <- read_excel("pyaterka_cfa_sem.xlsx") |> clean_names()

# ======================
# CFA + Reliability: ALL
# ======================

items_by_factor <- list(
  perceived_value = c(
    "assortment_item_category_coverage_likert",
    "assortment_item_price_range_likert",
    "assortment_item_wide_choice_likert",
    "benefit_item_choose_same_price_likert",
    "benefit_item_saves_money_likert",
    "benefit_item_value_money_likert",
    "uniqueness_item_new_interest_likert",
    "uniqueness_item_unique_features_likert",
    "uniqueness_item_visit_for_pl_likert"
  ),
  attitude = c(
    "attitude_item_brand_alternative_likert",
    "attitude_item_prefer_available_likert"
  ),
  retailer_img = c(
    "retailer_brand_item_logo_quality_trust_likert",
    "retailer_brand_item_quality_guarantee_likert"
  ),
  loyalty_retailer = c(
    "loyalty_retailer_regular_purchase_likert",
    "loyalty_retailer_prefer_over_brands_likert",
    "loyalty_retailer_recommend_pl_likert"
  ),
  loyalty_item = c(
    "loyalty_item_regular_purchase_likert",
    "loyalty_item_prefer_over_brands_likert",
    "loyalty_item_recommend_pl_likert"
  )
)

needed_all <- unlist(items_by_factor, use.names = FALSE)
missing_cols_all <- setdiff(needed_all, names(all_df))
if (length(missing_cols_all) > 0) stop("ALL: Missing columns:\n", paste(missing_cols_all, collapse = "\n"))

# ---------------- 2) build model ----------------
model_cfa_all <- imap_chr(items_by_factor, \(items, fac) {
  paste0(fac, " =~ ", paste(items, collapse = " + "))
}) |> paste(collapse = "\n")

cat("\n--- CFA model (ALL) ---\n"); cat(model_cfa_all); cat("\n----------------------\n")

# ---------------- 3) fit CFA (ordinal) ----------------
fit_all <- cfa(
  model = model_cfa_all,
  data = all_df,
  ordered = needed_all,
  estimator = "WLSMV",
  std.lv = TRUE
)

cat("\n--- Fit measures (ALL) ---\n")
print(fitMeasures(fit_all, c("cfi","tli","rmsea","srmr")))

# ---------------- 4) loadings table ----------------
loadings_tbl_all <- parameterEstimates(fit_all, standardized = TRUE) |>
  filter(op == "=~") |>
  transmute(factor = lhs, item = rhs, loading_std = std.all, pvalue = pvalue) |>
  arrange(factor, desc(abs(loading_std)))

cat("\n--- Standardized loadings (ALL) ---\n")
print(loadings_tbl_all)

# ---------------- 5) Cronbach alpha per factor (no psych needed) ----------------
cronbach_alpha <- function(df_items) {
  x <- df_items |> mutate(across(everything(), as.numeric))
  x <- x[complete.cases(x), , drop = FALSE]
  k <- ncol(x)
  if (k < 2) return(NA_real_)
  S <- cov(x, use = "pairwise.complete.obs")
  alpha <- (k/(k-1)) * (1 - sum(diag(S)) / sum(S))
  as.numeric(alpha)
}

alpha_tbl_all <- imap_dfr(items_by_factor, \(items, fac) {
  tibble(
    factor = fac,
    cronbach_alpha = cronbach_alpha(all_df |> select(all_of(items)))
  )
}) |> arrange(factor)

cat("\n--- Cronbach alpha (ALL) ---\n")
print(alpha_tbl_all)

# ---------------- 6) Composite Reliability (CR / rho_c) from standardized loadings ----------------
cr_from_loadings <- function(lambdas) {
  lambdas <- as.numeric(lambdas)
  lambdas <- lambdas[!is.na(lambdas)]
  if (length(lambdas) < 2) return(NA_real_)
  num <- (sum(lambdas))^2
  den <- num + sum(1 - lambdas^2)
  as.numeric(num/den)
}

cr_tbl_all <- loadings_tbl_all |>
  group_by(factor) |>
  summarise(composite_reliability = cr_from_loadings(loading_std), .groups = "drop") |>
  arrange(factor)

cat("\n--- Composite Reliability (CR) (ALL) ---\n")
print(cr_tbl_all)

# ---------------- 7) final table ----------------
final_tbl_all <- alpha_tbl_all |> left_join(cr_tbl_all, by = "factor")

cat("\n--- Reliability summary (alpha + CR) (ALL) ---\n")
print(final_tbl_all)
