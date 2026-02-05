library(readxl)
library(janitor)
library(dplyr)
library(tidyr)
library(cSEM)
library(stringr)
library(writexl)

# ============================================================
# PLS-SEM: TWO-STAGE (2nd order perceived_value) + mediation
# Strict bootstrap significance of INDIRECT (a*b) within clusters
# Moderation by CLUSTER: pairwise diffs of indirect effects + p/CI
# Moderator column: cluster (string labels, 3 categories)
# Data: clustered.xlsx
# Output: Excel with sheets
# ============================================================

# -------------------------
# 0) load clustered data
# -------------------------
all_q <- read_excel("clustered.xlsx") |> clean_names()

# -------------------------
# 1) required indicators + moderator
# -------------------------
needed <- c(
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
  "loyalty_item_recommend_pl_likert",
  # moderator
  "cluster"
)

missing_cols <- setdiff(needed, names(all_q))
if (length(missing_cols) > 0) stop("Missing columns:\n", paste(missing_cols, collapse = "\n"))

# -------------------------
# 2) Clean cluster labels (keep only 3 named clusters)
# -------------------------
cluster_labels <- c(
  "Mainstream individual shoppers",
  "Young shared-decision shoppers",
  "Urban family-oriented shoppers"
)

df0 <- all_q |>
  select(all_of(needed)) |>
  mutate(
    cluster_clean = str_trim(as.character(cluster)),
    cluster_clean = if_else(cluster_clean %in% cluster_labels, cluster_clean, NA_character_),
    cluster_clean = factor(cluster_clean, levels = cluster_labels)
  ) |>
  filter(!is.na(cluster_clean))

cat("\nCluster sizes (after cleaning):\n")
print(table(df0$cluster_clean))

cluster_sizes <- as.data.frame(table(df0$cluster_clean))
names(cluster_sizes) <- c("cluster", "n")

# -------------------------
# 3) Likert -> numeric + drop NA rows
# -------------------------
pls_df <- df0 |>
  mutate(across(-c(cluster, cluster_clean), \(x) {
    if (is.numeric(x)) return(x)
    if (is.factor(x) || is.ordered(x)) return(as.numeric(x))
    suppressWarnings(as.numeric(as.character(x)))
  })) |>
  drop_na()

# split by cluster
pls_c1 <- pls_df |> filter(cluster_clean == cluster_labels[1]) |> select(-cluster, -cluster_clean)
pls_c2 <- pls_df |> filter(cluster_clean == cluster_labels[2]) |> select(-cluster, -cluster_clean)
pls_c3 <- pls_df |> filter(cluster_clean == cluster_labels[3]) |> select(-cluster, -cluster_clean)

cat("\nN C1:", nrow(pls_c1), " | N C2:", nrow(pls_c2), " | N C3:", nrow(pls_c3), "\n")
if (any(c(nrow(pls_c1), nrow(pls_c2), nrow(pls_c3)) < 30)) warning("One (or more) clusters has <30 observations. Results may be unstable.")

# -------------------------
# 4) TWO-STAGE model: perceived_value = 2nd order (assortment+benefit+uniqueness)
# partial mediation as in your code
# -------------------------
model_pls <- "
# first-order measurement
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

# second-order measurement (two-stage)
perceived_value =~ assortment + benefit + uniqueness

# structural (partial mediation)
loyalty_item ~ perceived_value + attitude + retailer_img
loyalty_retailer ~ loyalty_item + perceived_value + attitude + retailer_img
"

# -------------------------
# 5) Fit once + extract SECOND STAGE path matrix (robust for two-stage)
# -------------------------
fit_pls_once <- function(dat) {
  res <- csem(
    .data = dat,
    .model = model_pls,
    .approach_weights = "PLS-PM",
    .handle_inadmissibles = "drop"
  )
  res$Second_stage$Estimates$Path_estimates
}

resolve_name <- function(x, pool) {
  if (x %in% pool) return(x)
  x_temp <- paste0(x, "_temp")
  if (x_temp %in% pool) return(x_temp)
  x_notemp <- sub("_temp$", "", x)
  if (x_notemp %in% pool) return(x_notemp)
  NA_character_
}

get_beta <- function(path_mat, to, from) {
  rn <- rownames(path_mat); cn <- colnames(path_mat)
  if (is.null(rn) || is.null(cn)) return(NA_real_)
  to2 <- resolve_name(to, rn)
  from2 <- resolve_name(from, cn)
  if (is.na(to2) || is.na(from2)) return(NA_real_)
  as.numeric(path_mat[to2, from2])
}

ind_point_from_pm <- function(pm) {
  a_pv  <- get_beta(pm, to = "loyalty_item",     from = "perceived_value")
  a_att <- get_beta(pm, to = "loyalty_item",     from = "attitude")
  a_img <- get_beta(pm, to = "loyalty_item",     from = "retailer_img")
  b_li  <- get_beta(pm, to = "loyalty_retailer", from = "loyalty_item")

  c(
    indirect_pv = a_pv * b_li,
    indirect_attitude = a_att * b_li,
    indirect_retailer_img = a_img * b_li
  )
}

# -------------------------
# 6) strict bootstrap for indirect effects (a*b) within a cluster
# Uses POINT estimate from original data + percentile CI + sign-based p
# Skips invalid draws and collects exactly B valid draws
# -------------------------
bootstrap_indirect <- function(dat, B = 5000, seed = 42, label = "cluster") {
  set.seed(seed)
  n <- nrow(dat)

  pm0 <- fit_pls_once(dat)
  point <- ind_point_from_pm(pm0)

  ind_pv  <- numeric(0)
  ind_att <- numeric(0)
  ind_img <- numeric(0)

  it <- 0
  while (length(ind_pv) < B) {
    it <- it + 1
    idx <- sample.int(n, size = n, replace = TRUE)
    pm <- fit_pls_once(dat[idx, , drop = FALSE])

    a_pv  <- get_beta(pm, to = "loyalty_item",     from = "perceived_value")
    a_att <- get_beta(pm, to = "loyalty_item",     from = "attitude")
    a_img <- get_beta(pm, to = "loyalty_item",     from = "retailer_img")
    b_li  <- get_beta(pm, to = "loyalty_retailer", from = "loyalty_item")

    vals <- c(a_pv, a_att, a_img, b_li)
    if (any(!is.finite(vals))) next

    ind_pv  <- c(ind_pv,  a_pv  * b_li)
    ind_att <- c(ind_att, a_att * b_li)
    ind_img <- c(ind_img, a_img * b_li)

    if (length(ind_pv) %% 500 == 0) cat(label, ": valid bootstrap", length(ind_pv), "/", B, "\n")
  }

  summarize_dist <- function(x, point_est) {
    ci <- quantile(x, probs = c(0.025, 0.975), names = FALSE, na.rm = TRUE)
    p_lo <- mean(x <= 0, na.rm = TRUE)
    p_hi <- mean(x >= 0, na.rm = TRUE)
    p_two <- min(2 * min(p_lo, p_hi), 1)

    list(
      point_estimate = as.numeric(point_est),
      ci_low = ci[1],
      ci_high = ci[2],
      p_value = p_two,
      draws = x
    )
  }

  list(
    indirect_pv = summarize_dist(ind_pv, point["indirect_pv"]),
    indirect_attitude = summarize_dist(ind_att, point["indirect_attitude"]),
    indirect_retailer_img = summarize_dist(ind_img, point["indirect_retailer_img"])
  )
}

diff_test <- function(draw_a, draw_b) {
  Bmin <- min(length(draw_a), length(draw_b))
  d <- draw_a[1:Bmin] - draw_b[1:Bmin]
  d <- d[is.finite(d)]

  est <- mean(d, na.rm = TRUE)
  ci <- quantile(d, probs = c(0.025, 0.975), names = FALSE, na.rm = TRUE)

  p_lo <- mean(d <= 0, na.rm = TRUE)
  p_hi <- mean(d >= 0, na.rm = TRUE)
  p_two <- min(2 * min(p_lo, p_hi), 1)

  c(estimate = est, ci_low = ci[1], ci_high = ci[2], p_value = p_two)
}

# -------------------------
# 7) Point indirect effects (by cluster)
# -------------------------
pm1 <- fit_pls_once(pls_c1)
pm2 <- fit_pls_once(pls_c2)
pm3 <- fit_pls_once(pls_c3)

point_tbl <- bind_rows(
  data.frame(cluster = cluster_labels[1], t(ind_point_from_pm(pm1)), check.names = FALSE),
  data.frame(cluster = cluster_labels[2], t(ind_point_from_pm(pm2)), check.names = FALSE),
  data.frame(cluster = cluster_labels[3], t(ind_point_from_pm(pm3)), check.names = FALSE)
)
cat("\n========================\nPOINT INDIRECT EFFECTS (BY CLUSTER, two-stage)\n========================\n")
print(point_tbl)

# -------------------------
# 8) Bootstrap indirect significance (by cluster)
# -------------------------
B <- 5000
boot1 <- bootstrap_indirect(pls_c1, B = B, seed = 101, label = "C1")
boot2 <- bootstrap_indirect(pls_c2, B = B, seed = 202, label = "C2")
boot3 <- bootstrap_indirect(pls_c3, B = B, seed = 303, label = "C3")

mk_boot_tbl <- function(boot, cl) {
  data.frame(
    cluster = cl,
    effect = c(
      "perceived_value -> loyalty_item -> loyalty_retailer",
      "attitude -> loyalty_item -> loyalty_retailer",
      "retailer_img -> loyalty_item -> loyalty_retailer"
    ),
    point_estimate = c(
      boot$indirect_pv$point_estimate,
      boot$indirect_attitude$point_estimate,
      boot$indirect_retailer_img$point_estimate
    ),
    ci_low = c(
      boot$indirect_pv$ci_low,
      boot$indirect_attitude$ci_low,
      boot$indirect_retailer_img$ci_low
    ),
    ci_high = c(
      boot$indirect_pv$ci_high,
      boot$indirect_attitude$ci_high,
      boot$indirect_retailer_img$ci_high
    ),
    p_value = c(
      boot$indirect_pv$p_value,
      boot$indirect_attitude$p_value,
      boot$indirect_retailer_img$p_value
    )
  )
}

boot_tbl <- bind_rows(
  mk_boot_tbl(boot1, cluster_labels[1]),
  mk_boot_tbl(boot2, cluster_labels[2]),
  mk_boot_tbl(boot3, cluster_labels[3])
)

cat("\n========================\nBOOTSTRAP INDIRECT SIGNIFICANCE (BY CLUSTER)\n========================\n")
print(boot_tbl)

# -------------------------
# 9) Moderated mediation (pairwise diffs)
# -------------------------
pair_diff <- function(effect_name, draw_a, draw_b, name_a, name_b) {
  d <- diff_test(draw_a, draw_b)
  data.frame(
    effect = paste0("diff_", effect_name, " (", name_a, " - ", name_b, ")"),
    estimate = as.numeric(d["estimate"]),
    ci_low = as.numeric(d["ci_low"]),
    ci_high = as.numeric(d["ci_high"]),
    p_value = as.numeric(d["p_value"])
  )
}

diff_tbl <- bind_rows(
  # indirect_pv
  pair_diff("indirect_pv", boot1$indirect_pv$draws, boot2$indirect_pv$draws, cluster_labels[1], cluster_labels[2]),
  pair_diff("indirect_pv", boot1$indirect_pv$draws, boot3$indirect_pv$draws, cluster_labels[1], cluster_labels[3]),
  pair_diff("indirect_pv", boot2$indirect_pv$draws, boot3$indirect_pv$draws, cluster_labels[2], cluster_labels[3]),
  # indirect_attitude
  pair_diff("indirect_attitude", boot1$indirect_attitude$draws, boot2$indirect_attitude$draws, cluster_labels[1], cluster_labels[2]),
  pair_diff("indirect_attitude", boot1$indirect_attitude$draws, boot3$indirect_attitude$draws, cluster_labels[1], cluster_labels[3]),
  pair_diff("indirect_attitude", boot2$indirect_attitude$draws, boot3$indirect_attitude$draws, cluster_labels[2], cluster_labels[3]),
  # indirect_retailer_img
  pair_diff("indirect_retailer_img", boot1$indirect_retailer_img$draws, boot2$indirect_retailer_img$draws, cluster_labels[1], cluster_labels[2]),
  pair_diff("indirect_retailer_img", boot1$indirect_retailer_img$draws, boot3$indirect_retailer_img$draws, cluster_labels[1], cluster_labels[3]),
  pair_diff("indirect_retailer_img", boot2$indirect_retailer_img$draws, boot3$indirect_retailer_img$draws, cluster_labels[2], cluster_labels[3])
)

cat("\n========================\nMODERATED MEDIATION (PAIRWISE DIFF INDIRECT)\n========================\n")
print(diff_tbl)

cat("\nREADING RULES:\n")
cat("- Within cluster: indirect significant if 95% CI does NOT include 0 (or p_value < 0.05).\n")
cat("- Moderated mediation: diff significant if 95% CI does NOT include 0 (or p_value < 0.05).\n")

out_file <- "pls_2stage_cluster_indirect_diffs.xlsx"

write_xlsx(
  x = list(
    cluster_sizes = cluster_sizes,
    point_indirect = point_tbl,
    bootstrap_indirect_by_cluster = boot_tbl,
    diff_indirect_pairwise = diff_tbl
  ),
  path = out_file
)
