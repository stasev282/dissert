library(readxl)
library(janitor)
library(dplyr)
library(tidyr)
library(cSEM)
library(stringr)

# ============================================================
# PLS-SEM: mediation + strict bootstrap significance of INDIRECT
# Moderation by CLUSTER (named): compare indirect effects between clusters
# Moderator column: cluster (string labels)
# ============================================================

# -------------------------
# 0) load clustered data (IMPORTANT: use clustered.xlsx)
# -------------------------
all_q <- read_excel("clustered.xlsx") |> clean_names()

# -------------------------
# 1) required indicators + moderator
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
  "loyalty_item_recommend_pl_likert",
  # moderator
  "cluster"
)

missing_cols <- setdiff(needed, names(all_q))
if (length(missing_cols) > 0) {
  stop("Missing columns:\n", paste(missing_cols, collapse = "\n"))
}

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

# -------------------------
# 3) Likert -> numeric + drop NA rows
# -------------------------
pls_df <- df0 |>
  mutate(across(-c(cluster, cluster_clean), \(x) {
    if (is.numeric(x)) return(x)
    if (is.factor(x) || is.ordered(x)) return(as.numeric(x))
    suppressWarnings(as.numeric(x))
  })) |>
  drop_na()

# split by cluster (named)
pls_c1 <- pls_df |> filter(cluster_clean == cluster_labels[1]) |> select(-cluster, -cluster_clean)
pls_c2 <- pls_df |> filter(cluster_clean == cluster_labels[2]) |> select(-cluster, -cluster_clean)
pls_c3 <- pls_df |> filter(cluster_clean == cluster_labels[3]) |> select(-cluster, -cluster_clean)

cat("\nN C1:", nrow(pls_c1), " | N C2:", nrow(pls_c2), " | N C3:", nrow(pls_c3), "\n")
if (any(c(nrow(pls_c1), nrow(pls_c2), nrow(pls_c3)) < 30)) {
  warning("One (or more) clusters has <30 observations. Results may be unstable.")
}

# -------------------------
# 4) PLS-SEM mediation model (partial mediation)
# -------------------------
model_pls <- "
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

loyalty_item ~ perceived_value + attitude + retailer_img
loyalty_retailer ~ loyalty_item + perceived_value + attitude + retailer_img
"

# -------------------------
# 5) helper functions
# -------------------------
fit_pls_once <- function(dat) {
  res <- csem(
    .data = dat,
    .model = model_pls,
    .approach_weights = "PLS-PM",
    .handle_inadmissibles = "drop"
  )
  summarize(res)$Estimates$Path_estimates
}

get_path <- function(path_table, lhs, rhs) {
  key <- paste0(lhs, " ~ ", rhs)
  row <- path_table |> dplyr::filter(.data$Name == key)
  if (nrow(row) == 0) return(NA_real_)
  row$Estimate[[1]]
}

ind_point_from_pe <- function(pe) {
  a_pv  <- get_path(pe, "loyalty_item", "perceived_value")
  a_att <- get_path(pe, "loyalty_item", "attitude")
  a_img <- get_path(pe, "loyalty_item", "retailer_img")
  b_li  <- get_path(pe, "loyalty_retailer", "loyalty_item")
  
  c(
    indirect_pv = a_pv * b_li,
    indirect_attitude = a_att * b_li,
    indirect_retailer_img = a_img * b_li
  )
}

bootstrap_indirect <- function(dat, B = 5000, seed = 42, label = "group") {
  set.seed(seed)
  n <- nrow(dat)
  
  ind_pv  <- numeric(B)
  ind_att <- numeric(B)
  ind_img <- numeric(B)
  
  for (i in seq_len(B)) {
    idx <- sample.int(n, size = n, replace = TRUE)
    pe <- fit_pls_once(dat[idx, , drop = FALSE])
    
    a_pv  <- get_path(pe, "loyalty_item", "perceived_value")
    a_att <- get_path(pe, "loyalty_item", "attitude")
    a_img <- get_path(pe, "loyalty_item", "retailer_img")
    b_li  <- get_path(pe, "loyalty_retailer", "loyalty_item")
    
    ind_pv[i]  <- a_pv  * b_li
    ind_att[i] <- a_att * b_li
    ind_img[i] <- a_img * b_li
    
    if (i %% 500 == 0) cat(label, ": bootstrap", i, "/", B, "\n")
  }
  
  summarize_dist <- function(x) {
    x <- x[is.finite(x)]
    est <- mean(x)
    ci <- quantile(x, probs = c(0.025, 0.975), names = FALSE, na.rm = TRUE)
    
    p_lo <- mean(x <= 0, na.rm = TRUE)
    p_hi <- mean(x >= 0, na.rm = TRUE)
    p_two <- 2 * min(p_lo, p_hi)
    p_two <- min(p_two, 1)
    
    list(estimate = est, ci_low = ci[1], ci_high = ci[2], p_value = p_two, draws = x)
  }
  
  list(
    indirect_pv = summarize_dist(ind_pv),
    indirect_attitude = summarize_dist(ind_att),
    indirect_retailer_img = summarize_dist(ind_img)
  )
}

diff_test <- function(draw_a, draw_b) {
  Bmin <- min(length(draw_a), length(draw_b))
  d <- draw_a[1:Bmin] - draw_b[1:Bmin]
  
  est <- mean(d, na.rm = TRUE)
  ci <- quantile(d, probs = c(0.025, 0.975), names = FALSE, na.rm = TRUE)
  
  p_lo <- mean(d <= 0, na.rm = TRUE)
  p_hi <- mean(d >= 0, na.rm = TRUE)
  p_two <- 2 * min(p_lo, p_hi)
  p_two <- min(p_two, 1)
  
  c(estimate = est, ci_low = ci[1], ci_high = ci[2], p_value = p_two)
}

# -------------------------
# 6) Point indirect effects (by cluster)
# -------------------------
pe1 <- fit_pls_once(pls_c1)
pe2 <- fit_pls_once(pls_c2)
pe3 <- fit_pls_once(pls_c3)

cat("\n========================\nPOINT INDIRECT EFFECTS (BY CLUSTER)\n========================\n")
point_tbl <- bind_rows(
  data.frame(cluster = cluster_labels[1], t(ind_point_from_pe(pe1))),
  data.frame(cluster = cluster_labels[2], t(ind_point_from_pe(pe2))),
  data.frame(cluster = cluster_labels[3], t(ind_point_from_pe(pe3)))
)
print(point_tbl)

# -------------------------
# 7) Bootstrap indirect significance (by cluster)
# -------------------------
B <- 5000
boot1 <- bootstrap_indirect(pls_c1, B = B, seed = 101, label = "C1")
boot2 <- bootstrap_indirect(pls_c2, B = B, seed = 202, label = "C2")
boot3 <- bootstrap_indirect(pls_c3, B = B, seed = 303, label = "C3")

boot_tbl <- bind_rows(
  data.frame(cluster = cluster_labels[1], effect = "indirect_pv",
             estimate = boot1$indirect_pv$estimate, ci_low = boot1$indirect_pv$ci_low, ci_high = boot1$indirect_pv$ci_high, p_value = boot1$indirect_pv$p_value),
  data.frame(cluster = cluster_labels[1], effect = "indirect_attitude",
             estimate = boot1$indirect_attitude$estimate, ci_low = boot1$indirect_attitude$ci_low, ci_high = boot1$indirect_attitude$ci_high, p_value = boot1$indirect_attitude$p_value),
  data.frame(cluster = cluster_labels[1], effect = "indirect_retailer_img",
             estimate = boot1$indirect_retailer_img$estimate, ci_low = boot1$indirect_retailer_img$ci_low, ci_high = boot1$indirect_retailer_img$ci_high, p_value = boot1$indirect_retailer_img$p_value),
  
  data.frame(cluster = cluster_labels[2], effect = "indirect_pv",
             estimate = boot2$indirect_pv$estimate, ci_low = boot2$indirect_pv$ci_low, ci_high = boot2$indirect_pv$ci_high, p_value = boot2$indirect_pv$p_value),
  data.frame(cluster = cluster_labels[2], effect = "indirect_attitude",
             estimate = boot2$indirect_attitude$estimate, ci_low = boot2$indirect_attitude$ci_low, ci_high = boot2$indirect_attitude$ci_high, p_value = boot2$indirect_attitude$p_value),
  data.frame(cluster = cluster_labels[2], effect = "indirect_retailer_img",
             estimate = boot2$indirect_retailer_img$estimate, ci_low = boot2$indirect_retailer_img$ci_low, ci_high = boot2$indirect_retailer_img$ci_high, p_value = boot2$indirect_retailer_img$p_value),
  
  data.frame(cluster = cluster_labels[3], effect = "indirect_pv",
             estimate = boot3$indirect_pv$estimate, ci_low = boot3$indirect_pv$ci_low, ci_high = boot3$indirect_pv$ci_high, p_value = boot3$indirect_pv$p_value),
  data.frame(cluster = cluster_labels[3], effect = "indirect_attitude",
             estimate = boot3$indirect_attitude$estimate, ci_low = boot3$indirect_attitude$ci_low, ci_high = boot3$indirect_attitude$ci_high, p_value = boot3$indirect_attitude$p_value),
  data.frame(cluster = cluster_labels[3], effect = "indirect_retailer_img",
             estimate = boot3$indirect_retailer_img$estimate, ci_low = boot3$indirect_retailer_img$ci_low, ci_high = boot3$indirect_retailer_img$ci_high, p_value = boot3$indirect_retailer_img$p_value)
)

cat("\n========================\nBOOTSTRAP INDIRECT SIGNIFICANCE (BY CLUSTER)\n========================\n")
print(boot_tbl)

# -------------------------
# 8) Moderated mediation (pairwise diffs, named)
# -------------------------
cat("\n========================\nMODERATED MEDIATION (PAIRWISE DIFF INDIRECT; named)\n========================\n")

pair_diff <- function(effect, a, b, name_a, name_b) {
  d <- diff_test(a, b)
  data.frame(
    effect = paste0("diff_", effect, " (", name_a, " - ", name_b, ")"),
    estimate = d["estimate"],
    ci_low = d["ci_low"],
    ci_high = d["ci_high"],
    p_value = d["p_value"]
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

print(diff_tbl)

cat("\nREADING RULES:\n")
cat("- Within cluster: indirect effect is significant if 95% CI does NOT include 0 (or p_value < 0.05).\n")
cat("- Moderated mediation: diff_indirect_* significant if its CI does NOT include 0 (or p_value < 0.05).\n")
