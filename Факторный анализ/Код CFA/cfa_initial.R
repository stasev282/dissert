library(tidyverse)
library(readxl)
library(lavaan)
library(janitor)
library(writexl)

all_df <- read_excel("all_cfa_sem.xlsx") |> clean_names()

items_by_factor <- list(
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
  cannibalization = c(
    "cannibalization_item_too_similar_likert",
    "cannibalization_item_choice_difficulty_likert",
    "cannibalization_item_segment_clarity_likert"
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
  ),
  stm_knowledge = "stm_knowledge",
  stm_freq = "stm_freq",
  stm_share = "stm_share"
)

needed_all <- unlist(items_by_factor, use.names = FALSE)
missing_cols <- setdiff(needed_all, names(all_df))
if (length(missing_cols) > 0) stop(paste(missing_cols, collapse = "\n"))

make_block <- function(fac, items) {
  if (length(items) == 1) {
    item <- items[[1]]
    paste0(fac, " =~ 1*", item, "\n", item, " ~~ 0*", item)
  } else {
    paste0(fac, " =~ ", paste(items, collapse = " + "))
  }
}

model_cfa <- imap_chr(items_by_factor, make_block) |> paste(collapse = "\n")

fit <- cfa(
  model = model_cfa,
  data = all_df,
  ordered = needed_all,
  estimator = "WLSMV",
  std.lv = TRUE
)

fit_measures_tbl <- tibble(
  n = lavaan::nobs(fit),
  cfi = fitMeasures(fit, "cfi"),
  tli = fitMeasures(fit, "tli"),
  rmsea = fitMeasures(fit, "rmsea"),
  rmsea_ci_lower = fitMeasures(fit, "rmsea.ci.lower"),
  rmsea_ci_upper = fitMeasures(fit, "rmsea.ci.upper"),
  srmr = fitMeasures(fit, "srmr")
)

pe <- parameterEstimates(fit, standardized = TRUE)

loadings <- pe |>
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

cronbach_alpha <- function(df_items) {
  x <- df_items |> mutate(across(everything(), as.numeric))
  x <- x[complete.cases(x), , drop = FALSE]
  k <- ncol(x)
  if (k < 2) return(NA_real_)
  S <- cov(x, use = "pairwise.complete.obs")
  as.numeric((k/(k-1)) * (1 - sum(diag(S)) / sum(S)))
}

alpha_tbl <- imap_dfr(items_by_factor, \(items, fac) {
  if (length(items) < 2) {
    tibble(factor = fac, cronbach_alpha = NA_real_)
  } else {
    tibble(factor = fac, cronbach_alpha = cronbach_alpha(all_df |> select(all_of(items))))
  }
}) |> arrange(factor)

cr_from_loadings <- function(lambdas) {
  lambdas <- as.numeric(lambdas)
  lambdas <- lambdas[!is.na(lambdas)]
  if (length(lambdas) < 2) return(NA_real_)
  num <- (sum(lambdas))^2
  den <- num + sum(1 - lambdas^2)
  as.numeric(num / den)
}

cr_tbl <- loadings |>
  group_by(factor) |>
  summarise(
    composite_reliability = if (n() < 2) NA_real_ else cr_from_loadings(loading_std),
    .groups = "drop"
  ) |>
  arrange(factor)

reliability_tbl <- alpha_tbl |>
  left_join(cr_tbl, by = "factor") |>
  arrange(factor)

write_xlsx(
  x = list(
    fit_measures = fit_measures_tbl,
    loadings = loadings,
    reliability_alpha_cr = reliability_tbl
  ),
  path = "cfa_loadings_reliability.xlsx"
)
