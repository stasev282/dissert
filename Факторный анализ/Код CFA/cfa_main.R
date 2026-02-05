library(tidyverse)
library(readxl)
library(lavaan)
library(semTools)
library(janitor)
library(writexl)

all_df <- read_excel("all_cfa_sem.xlsx") |> clean_names()

items_by_factor_1st <- list(
  assortment = c(
    "assortment_item_category_coverage_likert",
    "assortment_item_price_range_likert",
    "assortment_item_wide_choice_likert"
  ),
  benefit = c(
    "benefit_item_choose_same_price_likert",
    "benefit_item_saves_money_likert",
    "benefit_item_value_money_likert"
  ),
  uniqueness = c(
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

needed_all <- unlist(items_by_factor_1st, use.names = FALSE)
missing_cols_all <- setdiff(needed_all, names(all_df))
if (length(missing_cols_all) > 0) stop("Missing columns:\n", paste(missing_cols_all, collapse = "\n"))

model_cfa_1st <- imap_chr(items_by_factor_1st, \(items, fac) {
  paste0(fac, " =~ ", paste(items, collapse = " + "))
}) |> paste(collapse = "\n")

model_cfa_2nd <- "
perceived_value =~ assortment + benefit + uniqueness
"

model_cfa_all <- paste(model_cfa_1st, model_cfa_2nd, sep = "\n")

cat("\n--- CFA model (HCM) ---\n"); cat(model_cfa_all); cat("\n-----------------------\n")

fit_all <- cfa(
  model = model_cfa_all,
  data = all_df,
  ordered = needed_all,
  estimator = "WLSMV",
  std.lv = TRUE
)

fit_measures_tbl <- tibble(
  n = lavaan::nobs(fit_all),
  cfi = fitMeasures(fit_all, "cfi"),
  tli = fitMeasures(fit_all, "tli"),
  rmsea = fitMeasures(fit_all, "rmsea"),
  rmsea_ci_lower = fitMeasures(fit_all, "rmsea.ci.lower"),
  rmsea_ci_upper = fitMeasures(fit_all, "rmsea.ci.upper"),
  srmr = fitMeasures(fit_all, "srmr")
)

cat("\n--- Fit measures (HCM) ---\n")
print(fit_measures_tbl)

pe <- parameterEstimates(fit_all, standardized = TRUE)

loadings_all <- pe |>
  filter(op == "=~") |>
  transmute(
    factor = lhs,
    indicator = rhs,
    loading_unstd = est,
    loading_std = std.all,
    se = se,
    z = z,
    pvalue = pvalue
  ) |>
  arrange(factor, desc(abs(loading_std)))

loadings_second_order <- loadings_all |>
  filter(factor == "perceived_value" & indicator %in% c("assortment","benefit","uniqueness")) |>
  arrange(desc(abs(loading_std)))

loadings_first_order <- loadings_all |>
  filter(!(factor == "perceived_value" & indicator %in% c("assortment","benefit","uniqueness"))) |>
  arrange(factor, desc(abs(loading_std)))

cat("\n--- Standardized loadings (2nd order) ---\n")
print(loadings_second_order)

cat("\n--- Standardized loadings (1st order) ---\n")
print(loadings_first_order)

cronbach_alpha <- function(df_items) {
  x <- df_items |> mutate(across(everything(), as.numeric))
  x <- x[complete.cases(x), , drop = FALSE]
  k <- ncol(x)
  if (k < 2) return(NA_real_)
  S <- cov(x, use = "pairwise.complete.obs")
  alpha <- (k/(k-1)) * (1 - sum(diag(S)) / sum(S))
  as.numeric(alpha)
}

alpha_tbl_1st <- imap_dfr(items_by_factor_1st, \(items, fac) {
  tibble(
    factor = fac,
    cronbach_alpha = cronbach_alpha(all_df |> select(all_of(items)))
  )
}) |> arrange(factor)

alpha_tbl_2nd <- tibble(
  factor = "perceived_value",
  cronbach_alpha = NA_real_
)

alpha_tbl_all <- bind_rows(alpha_tbl_1st, alpha_tbl_2nd) |> arrange(factor)

cr_from_loadings <- function(lambdas) {
  lambdas <- as.numeric(lambdas)
  lambdas <- lambdas[!is.na(lambdas)]
  if (length(lambdas) < 2) return(NA_real_)
  num <- (sum(lambdas))^2
  den <- num + sum(1 - lambdas^2)
  as.numeric(num / den)
}

cr_tbl_1st <- loadings_first_order |>
  filter(factor %in% names(items_by_factor_1st)) |>
  group_by(factor) |>
  summarise(composite_reliability = cr_from_loadings(loading_std), .groups = "drop") |>
  arrange(factor)

cr_tbl_2nd <- loadings_second_order |>
  summarise(
    factor = "perceived_value",
    composite_reliability = cr_from_loadings(loading_std)
  )

cr_tbl_all <- bind_rows(cr_tbl_1st, cr_tbl_2nd) |> arrange(factor)

reliability_tbl <- alpha_tbl_all |>
  left_join(cr_tbl_all, by = "factor") |>
  arrange(factor)

cat("\n--- Reliability summary (alpha + CR) ---\n")
print(reliability_tbl)

out_file <- "cfa_hcm_loadings_reliability.xlsx"

write_xlsx(
  x = list(
    fit_measures = fit_measures_tbl,
    loadings_second_order = loadings_second_order,
    loadings_first_order  = loadings_first_order,
    reliability_alpha_cr  = reliability_tbl
  ),
  path = out_file
)

