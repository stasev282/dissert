library(readxl)
library(janitor)
library(psych)
library(dplyr)

# -------------------------
# 0) load data
# -------------------------
all_q <- read_excel("all_cfa_sem.xlsx") |> clean_names()

# -------------------------
# 1) indicators (only these)
# -------------------------
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

missing_cols <- setdiff(items, names(all_q))
if (length(missing_cols) > 0) {
  stop("Missing columns:\n", paste(missing_cols, collapse = "\n"))
}

# -------------------------
# 2) coerce to numeric (ordinal -> numeric)
# -------------------------
efa_df <- all_q |>
  select(all_of(items)) |>
  mutate(across(everything(), \(x) {
    if (is.numeric(x)) return(x)
    if (is.factor(x) || is.ordered(x)) return(as.numeric(x))
    suppressWarnings(as.numeric(x))
  }))

# -------------------------
# 3) polychoric correlations
# -------------------------
poly_out <- psych::polychoric(efa_df)
R_poly <- poly_out$rho

# -------------------------
# 4) eigenvalues + parallel analysis
# -------------------------
cat("\n--- Eigenvalues (polychoric correlation matrix) ---\n")
eigs <- eigen(R_poly, only.values = TRUE)$values
print(eigs)

cat("\n--- Parallel analysis (polychoric correlations) ---\n")
fa.parallel(R_poly, n.obs = nrow(efa_df), fa = "fa", fm = "minres")

# -------------------------
# 5) EFA: 1 factor
# -------------------------
cat("\n==============================\n")
cat("EFA: 1 factor (minres, no rotation)\n")
cat("==============================\n")
efa_1 <- psych::fa(R_poly, nfactors = 1, rotate = "none", fm = "minres", n.obs = nrow(efa_df))

cat("\n--- Loadings (>= .30) ---\n")
print(efa_1$loadings, cutoff = 0.30)

cat("\n--- Variance explained ---\n")
print(efa_1$Vaccounted)

# -------------------------
# 6) EFA: 2 factors
# -------------------------
cat("\n==============================\n")
cat("EFA: 2 factors (minres, oblimin)\n")
cat("==============================\n")
efa_2 <- psych::fa(R_poly, nfactors = 2, rotate = "oblimin", fm = "minres", n.obs = nrow(efa_df))

cat("\n--- Loadings (>= .30) ---\n")
print(efa_2$loadings, cutoff = 0.30)

cat("\n--- Variance explained ---\n")
print(efa_2$Vaccounted)

cat("\n--- Factor correlations (Phi) ---\n")
print(efa_2$Phi)

# -------------------------
# 7) cross-loadings (|loading| >= .30 on both factors)
# -------------------------
cat("\n--- Cross-loadings (|loading| >= .30 on both factors) ---\n")
L <- as.data.frame(unclass(efa_2$loadings))
if (ncol(L) >= 2) {
  cross_idx <- which(abs(L[[1]]) >= 0.30 & abs(L[[2]]) >= 0.30)
  if (length(cross_idx) == 0) {
    cat("No cross-loadings at |loading| >= .30.\n")
  } else {
    cross_tbl <- L[cross_idx, , drop = FALSE]
    cross_tbl$item <- rownames(cross_tbl)
    cross_tbl <- cross_tbl[, c("item", names(L))]
    print(cross_tbl)
  }
} else {
  cat("Not enough factors to check cross-loadings.\n")
}

# -------------------------
# 8) short structure flag (no numeric interpretation)
# -------------------------
cat("\n--- Structure flag (read with the loadings + variance + Phi) ---\n")
cat("If 1-factor loadings are broadly strong and 2-factor solution does not separate, then ~1 factor.\n")
cat("If 2-factor solution shows clean separation and low cross-loadings, then ~2 factors.\n")
cat("If many cross-loadings and mixed patterns, then structure is likely messy.\n")
