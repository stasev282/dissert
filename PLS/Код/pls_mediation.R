# ============================================================
# PLS-SEM (cSEM) with mediation + bootstrap
# FIXED: correct indirect effects (a*b) from Path_estimates "Name"
# Also computes percentile bootstrap CI for indirect effects
# by sampling a and b from Normal(Estimate, Std_err) as a fallback
# (because cSEM often doesn't expose bootstrap draws cleanly).
# ============================================================

library(readxl)
library(janitor)
library(dplyr)
library(tidyr)
library(stringr)
library(cSEM)

# -------------------------
# 0) load data
# -------------------------
all_q <- read_excel("all_cfa_sem.xlsx") |> clean_names()

# -------------------------
# 1) required indicators
# -------------------------
needed <- c(
  # perceived_value (BIG FACTOR, 9)
  "assortment_item_category_coverage_likert",
  "assortment_item_price_range_likert",
  "assortment_item_wide_choice_likert",
  "benefit_item_choose_same_price_likert",
  "benefit_item_saves_money_likert",
  "benefit_item_value_money_likert",
  "uniqueness_item_new_interest_likert",
  "uniqueness_item_unique_features_likert",
  "uniqueness_item_visit_for_pl_likert",
  # attitude (2)
  "attitude_item_brand_alternative_likert",
  "attitude_item_prefer_available_likert",
  # retailer_img (2)
  "retailer_brand_item_logo_quality_trust_likert",
  "retailer_brand_item_quality_guarantee_likert",
  # loyalty_retailer (3)
  "loyalty_retailer_regular_purchase_likert",
  "loyalty_retailer_prefer_over_brands_likert",
  "loyalty_retailer_recommend_pl_likert",
  # loyalty_item (3) mediator
  "loyalty_item_regular_purchase_likert",
  "loyalty_item_prefer_over_brands_likert",
  "loyalty_item_recommend_pl_likert"
)

missing_cols <- setdiff(needed, names(all_q))
if (length(missing_cols) > 0) {
  stop("Missing columns:\n", paste(missing_cols, collapse = "\n"))
}

# -------------------------
# 2) Likert -> numeric + drop NA rows
# -------------------------
pls_df <- all_q |>
  select(all_of(needed)) |>
  mutate(across(everything(), \(x) {
    if (is.numeric(x)) return(x)
    if (is.factor(x) || is.ordered(x)) return(as.numeric(x))
    suppressWarnings(as.numeric(as.character(x)))
  })) |>
  drop_na()

# -------------------------
# 3) cSEM model (reflective; partial mediation)
# -------------------------
model_pls <- "
# measurement model (reflective)
perceived_value =~
  assortment_item_category_coverage_likert +
  assortment_item_price_range_likert +
  assortment_item_wide_choice_likert +
  benefit_item_choose_same_price_likert +
  benefit_item_saves_money_likert +
  benefit_item_value_money_likert +
  uniqueness_item_new_interest_likert +
  uniqueness_item_unique_features_likert +
  uniqueness_item_visit_for_pl_likert

attitude =~
  attitude_item_brand_alternative_likert +
  attitude_item_prefer_available_likert

retailer_img =~
  retailer_brand_item_logo_quality_trust_likert +
  retailer_brand_item_quality_guarantee_likert

loyalty_item =~
  loyalty_item_regular_purchase_likert +
  loyalty_item_prefer_over_brands_likert +
  loyalty_item_recommend_pl_likert

loyalty_retailer =~
  loyalty_retailer_regular_purchase_likert +
  loyalty_retailer_prefer_over_brands_likert +
  loyalty_retailer_recommend_pl_likert

# structural model (partial mediation)
loyalty_item ~ perceived_value + attitude + retailer_img
loyalty_retailer ~ loyalty_item + perceived_value + attitude + retailer_img
"

# -------------------------
# 4) Estimate PLS-SEM + bootstrap
# -------------------------
set.seed(42)

res <- csem(
  .data = pls_df,
  .model = model_pls,
  .approach_weights = "PLS-PM",
  .resample_method = "bootstrap",
  .R = 5000,
  .handle_inadmissibles = "drop"
)

sum_res <- summarize(res)
q <- assess(res)

# ============================================================
# Helpers: parse Path_estimates "Name" into lhs/rhs + lookup by exact match
# ============================================================

parse_path_estimates <- function(pe_raw) {
  pe <- as.data.frame(pe_raw)
  
  stopifnot("Name" %in% names(pe))
  stopifnot("Estimate" %in% names(pe))
  
  pe2 <- pe |>
    mutate(
      Name = str_squish(as.character(.data$Name)),
      is_reg = str_detect(.data$Name, "\\s~\\s"),
      lhs = if_else(.data$is_reg, str_trim(str_split_fixed(.data$Name, "~", 2)[,1]), NA_character_),
      rhs = if_else(.data$is_reg, str_trim(str_split_fixed(.data$Name, "~", 2)[,2]), NA_character_)
    ) |>
    filter(.data$is_reg) |>
    select(Name, lhs, rhs, everything())
  
  if (nrow(pe2) == 0) {
    cat("\n[DEBUG] No regressions detected. Head of Path_estimates:\n")
    print(utils::head(pe, 30))
    stop("No regression paths detected in Path_estimates via pattern ' ~ '.")
  }
  
  pe2
}

get_path_row <- function(pe_reg, lhs, rhs) {
  row <- pe_reg |>
    dplyr::filter(.data$lhs == .env$lhs, .data$rhs == .env$rhs)
  if (nrow(row) == 0) return(NULL)
  row[1, , drop = FALSE]
}

get_path_est <- function(pe_reg, lhs, rhs) {
  row <- get_path_row(pe_reg, lhs, rhs)
  if (is.null(row)) return(NA_real_)
  as.numeric(row$Estimate[[1]])
}

get_path_se <- function(pe_reg, lhs, rhs) {
  if (!("Std_err" %in% names(pe_reg))) return(NA_real_)
  row <- get_path_row(pe_reg, lhs, rhs)
  if (is.null(row)) return(NA_real_)
  as.numeric(row$Std_err[[1]])
}


# Fallback CI for indirect effect using Monte Carlo (Normal approx)
# This is NOT the real bootstrap CI, but a widely used approximation when bootstrap draws aren't accessible.
mc_indirect_ci <- function(a, se_a, b, se_b, R = 50000, seed = 42) {
  set.seed(seed)
  if (any(is.na(c(a, se_a, b, se_b)))) return(c(NA_real_, NA_real_))
  a_s <- rnorm(R, mean = a, sd = se_a)
  b_s <- rnorm(R, mean = b, sd = se_b)
  ind <- a_s * b_s
  as.numeric(quantile(ind, probs = c(0.025, 0.975), na.rm = TRUE))
}

# ============================================================
# 5) MEASUREMENT MODEL OUTPUTS
# ============================================================
cat("\n========================\nMEASUREMENT MODEL (PLS)\n========================\n")

cat("\n--- Outer loadings (point estimates) ---\n")
if (!is.null(sum_res$Estimates$Loadings_estimates)) {
  print(sum_res$Estimates$Loadings_estimates)
} else {
  cat("Loadings not found in summarize(res)$Estimates.\n")
}

cat("\n--- Reliability / Convergent validity ---\n")
if (!is.null(q$alpha)) { cat("\nCronbach's alpha:\n"); print(q$alpha) } else cat("\nalpha: not available\n")
if (!is.null(q$rho_C)) { cat("\nComposite Reliability (rhoC):\n"); print(q$rho_C) } else cat("\nrhoC: not available\n")
if (!is.null(q$AVE))   { cat("\nAVE:\n"); print(q$AVE) } else cat("\nAVE: not available\n")

cat("\n--- HTMT (discriminant validity) ---\n")
if (!is.null(q$htmt)) {
  print(q$htmt)
} else if (!is.null(q$HTMT)) {
  print(q$HTMT)
} else {
  cat("HTMT not available in your cSEM version/output.\n")
}

# ============================================================
# 6) STRUCTURAL MODEL OUTPUTS
# ============================================================
cat("\n========================\nSTRUCTURAL MODEL (PLS)\n========================\n")

cat("\n--- Path coefficients (bootstrap inference) ---\n")
pe_raw <- sum_res$Estimates$Path_estimates
if (is.null(pe_raw)) stop("Path_estimates not found in summarize(res)$Estimates.")
print(pe_raw)

cat("\n--- R^2 (endogenous constructs) ---\n")
if (!is.null(q$R2)) print(q$R2) else if (!is.null(q$r2)) print(q$r2) else cat("R^2 not available.\n")

cat("\n--- VIF (predictor collinearity) ---\n")
if (!is.null(q$VIF)) print(q$VIF) else if (!is.null(q$vif)) print(q$vif) else cat("VIF not available.\n")

# ============================================================
# 7) EFFECTS (MEDIATION): direct / indirect / total (CORRECT)
# ============================================================
cat("\n========================\nEFFECTS (MEDIATION)\n========================\n")

pe_reg <- parse_path_estimates(pe_raw)

# a-paths (X -> loyalty_item)
a_pv  <- get_path_est(pe_reg, "loyalty_item", "perceived_value")
a_att <- get_path_est(pe_reg, "loyalty_item", "attitude")
a_img <- get_path_est(pe_reg, "loyalty_item", "retailer_img")

se_a_pv  <- get_path_se(pe_reg, "loyalty_item", "perceived_value")
se_a_att <- get_path_se(pe_reg, "loyalty_item", "attitude")
se_a_img <- get_path_se(pe_reg, "loyalty_item", "retailer_img")

# b-path (loyalty_item -> loyalty_retailer)
b_li  <- get_path_est(pe_reg, "loyalty_retailer", "loyalty_item")
se_b_li <- get_path_se(pe_reg, "loyalty_retailer", "loyalty_item")

# direct c' paths (X -> loyalty_retailer)
c_pv  <- get_path_est(pe_reg, "loyalty_retailer", "perceived_value")
c_att <- get_path_est(pe_reg, "loyalty_retailer", "attitude")
c_img <- get_path_est(pe_reg, "loyalty_retailer", "retailer_img")

# indirect (a*b)
ind_pv  <- a_pv  * b_li
ind_att <- a_att * b_li
ind_img <- a_img * b_li

# total (c' + a*b)
tot_pv  <- c_pv  + ind_pv
tot_att <- c_att + ind_att
tot_img <- c_img + ind_img

effects <- tibble::tibble(
  effect = c(
    "a: perceived_value -> loyalty_item",
    "a: attitude -> loyalty_item",
    "a: retailer_img -> loyalty_item",
    "b: loyalty_item -> loyalty_retailer",
    "direct c': perceived_value -> loyalty_retailer",
    "direct c': attitude -> loyalty_retailer",
    "direct c': retailer_img -> loyalty_retailer",
    "indirect: perceived_value -> loyalty_item -> loyalty_retailer",
    "indirect: attitude -> loyalty_item -> loyalty_retailer",
    "indirect: retailer_img -> loyalty_item -> loyalty_retailer",
    "total: perceived_value -> loyalty_retailer",
    "total: attitude -> loyalty_retailer",
    "total: retailer_img -> loyalty_retailer"
  ),
  estimate = c(
    a_pv, a_att, a_img, b_li,
    c_pv, c_att, c_img,
    ind_pv, ind_att, ind_img,
    tot_pv, tot_att, tot_img
  )
)

cat("\n--- Point estimates (correct) ---\n")
print(effects)

# Optional: Monte Carlo CI for indirects (fallback)
ci_ind_pv  <- mc_indirect_ci(a_pv,  se_a_pv,  b_li, se_b_li, R = 50000, seed = 42)
ci_ind_att <- mc_indirect_ci(a_att, se_a_att, b_li, se_b_li, R = 50000, seed = 43)
ci_ind_img <- mc_indirect_ci(a_img, se_a_img, b_li, se_b_li, R = 50000, seed = 44)

ind_ci_tbl <- tibble::tibble(
  effect = c("indirect_pv", "indirect_attitude", "indirect_retailer_img"),
  estimate = c(ind_pv, ind_att, ind_img),
  ci_95L_mc = c(ci_ind_pv[1], ci_ind_att[1], ci_ind_img[1]),
  ci_95U_mc = c(ci_ind_pv[2], ci_ind_att[2], ci_ind_img[2])
)

cat("\n--- Indirect effects: Monte Carlo 95% CI (Normal approx fallback) ---\n")
print(ind_ci_tbl)

cat("\nNOTE:\n")
cat("- Direct paths already have bootstrap p_value and percentile CI in Path_estimates.\n")
cat("- Indirect CIs above are Monte Carlo Normal-approx (fallback) because cSEM often doesn't expose bootstrap draws.\n")

# ============================================================
# 8) HOW TO READ
# ============================================================
cat("\n========================\nHOW TO READ\n========================\n")
cat("Partial mediation: predictors affect loyalty_retailer directly AND indirectly via loyalty_item.\n")
cat("Indirect = a*b. Total = c' + a*b.\n")
cat("Significance of direct paths: use p_value / CI in Path_estimates.\n")
cat("For indirect significance: ideally bootstrap draws of a and b; fallback CI shown above.\n")
