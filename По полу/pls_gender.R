library(readxl)
library(janitor)
library(dplyr)
library(tidyr)
library(cSEM)
library(stringr)
library(writexl)   # [ADDED]

# ============================================================
# PLS-SEM: TWO-STAGE (2nd order perceived_value) + mediation
# Strict bootstrap significance of INDIRECT (a*b) within groups
# Moderation by gender: compare indirect effects between groups
# Moderator column: socdec_gender
# ============================================================

# -------------------------
# 0) load data
# -------------------------
all_q <- read_excel("all_cfa_sem.xlsx") |> clean_names()

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
  "socdec_gender"
)

missing_cols <- setdiff(needed, names(all_q))
if (length(missing_cols) > 0) stop("Missing columns:\n", paste(missing_cols, collapse = "\n"))

# -------------------------
# 2) Clean gender: keep only male/female (drop NA / prefer not answer)
# -------------------------
df0 <- all_q |>
  select(all_of(needed)) |>
  mutate(
    g = str_trim(str_to_lower(as.character(socdec_gender))),
    gender_clean = case_when(
      str_detect(g, "муж") ~ "male",
      str_detect(g, "жен") ~ "female",
      str_detect(g, "^m$|male") ~ "male",
      str_detect(g, "^f$|female") ~ "female",
      g %in% c("1","2") ~ g,
      TRUE ~ NA_character_
    ),
    gender_clean = case_when(
      gender_clean == "1" ~ "male",   # если у тебя наоборот — свапни
      gender_clean == "2" ~ "female",
      TRUE ~ gender_clean
    )
  ) |>
  filter(!is.na(gender_clean)) |>
  select(-g)

# -------------------------
# 3) Likert -> numeric + drop NA rows
# -------------------------
pls_df <- df0 |>
  mutate(across(-c(socdec_gender, gender_clean), \(x) {
    if (is.numeric(x)) return(x)
    if (is.factor(x) || is.ordered(x)) return(as.numeric(x))
    suppressWarnings(as.numeric(as.character(x)))
  })) |>
  drop_na()

# Split groups
pls_male   <- pls_df |> filter(gender_clean == "male")   |> select(-socdec_gender, -gender_clean)
pls_female <- pls_df |> filter(gender_clean == "female") |> select(-socdec_gender, -gender_clean)

cat("\nN male:", nrow(pls_male), " | N female:", nrow(pls_female), "\n")
if (nrow(pls_male) < 30 || nrow(pls_female) < 30) warning("One of the groups has <30 observations. Results may be unstable.")

# [ADDED] sample sizes sheet
sample_sizes <- data.frame(
  group = c("male", "female"),
  n = c(nrow(pls_male), nrow(pls_female))
)

# -------------------------
# 4) TWO-STAGE PLS-SEM model:
# perceived_value = 2nd order = assortment + benefit + uniqueness
# NOTE: partial mediation (direct X->loyalty_retailer present).
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

# structural (PARTIAL mediation)
loyalty_item ~ perceived_value + attitude + retailer_img
loyalty_retailer ~ loyalty_item + perceived_value + attitude + retailer_img
"

# -------------------------
# 5) Fit once + extract SECOND STAGE path matrix
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

# --- robust name resolver for two-stage matrices (handles *_temp) ---
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

  to2   <- resolve_name(to, rn)
  from2 <- resolve_name(from, cn)

  if (is.na(to2) || is.na(from2)) return(NA_real_)
  as.numeric(path_mat[to2, from2])
}

# point indirect effects a*b
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
# 6) strict bootstrap for indirect effects (a*b) within a group
# -------------------------
bootstrap_indirect <- function(dat, B = 5000, seed = 42, label = "group") {
  set.seed(seed)
  n <- nrow(dat)

  ind_pv  <- numeric(B)
  ind_att <- numeric(B)
  ind_img <- numeric(B)

  for (i in seq_len(B)) {
    idx <- sample.int(n, size = n, replace = TRUE)
    dat_b <- dat[idx, , drop = FALSE]

    pm <- fit_pls_once(dat_b)

    a_pv  <- get_beta(pm, to = "loyalty_item",     from = "perceived_value")
    a_att <- get_beta(pm, to = "loyalty_item",     from = "attitude")
    a_img <- get_beta(pm, to = "loyalty_item",     from = "retailer_img")
    b_li  <- get_beta(pm, to = "loyalty_retailer", from = "loyalty_item")

    ind_pv[i]  <- a_pv  * b_li
    ind_att[i] <- a_att * b_li
    ind_img[i] <- a_img * b_li

    if (i %% 500 == 0) cat(label, ": bootstrap", i, "/", B, "\n")
  }

  summarize_dist <- function(x) {
    x <- x[is.finite(x)]
    est <- mean(x, na.rm = TRUE)
    ci <- quantile(x, probs = c(0.025, 0.975), names = FALSE, na.rm = TRUE)

    p_lo <- mean(x <= 0, na.rm = TRUE)
    p_hi <- mean(x >= 0, na.rm = TRUE)
    p_two <- 2 * min(p_lo, p_hi)
    p_two <- min(p_two, 1)

    list(estimate = est, ci_low = ci[1], ci_high = ci[2], p_value = p_two)
  }

  list(
    ind_pv  = summarize_dist(ind_pv),
    ind_att = summarize_dist(ind_att),
    ind_img = summarize_dist(ind_img),
    draws = list(ind_pv = ind_pv, ind_att = ind_att, ind_img = ind_img)
  )
}

# -------------------------
# 7) Point indirect effects by group
# -------------------------
pm_m <- fit_pls_once(pls_male)
pm_f <- fit_pls_once(pls_female)

point_tbl <- bind_rows(
  data.frame(group = "male",   t(ind_point_from_pm(pm_m)), check.names = FALSE),
  data.frame(group = "female", t(ind_point_from_pm(pm_f)), check.names = FALSE)
)
names(point_tbl) <- c("group", "indirect_pv", "indirect_attitude", "indirect_retailer_img")

cat("\n========================\nPOINT INDIRECT EFFECTS (two-stage)\n========================\n")
print(point_tbl)

# -------------------------
# 8) Bootstrap indirect significance by group
# -------------------------
B <- 5000
boot_m <- bootstrap_indirect(pls_male,   B = B, seed = 100, label = "male")
boot_f <- bootstrap_indirect(pls_female, B = B, seed = 200, label = "female")

tab_boot <- function(boot, group) {
  data.frame(
    group = group,
    effect = c(
      "perceived_value -> loyalty_item -> loyalty_retailer",
      "attitude -> loyalty_item -> loyalty_retailer",
      "retailer_img -> loyalty_item -> loyalty_retailer"
    ),
    estimate = c(boot$ind_pv$estimate, boot$ind_att$estimate, boot$ind_img$estimate),
    ci_low   = c(boot$ind_pv$ci_low,   boot$ind_att$ci_low,   boot$ind_img$ci_low),
    ci_high  = c(boot$ind_pv$ci_high,  boot$ind_att$ci_high,  boot$ind_img$ci_high),
    p_value  = c(boot$ind_pv$p_value,  boot$ind_att$p_value,  boot$ind_img$p_value)
  )
}

boot_tbl <- bind_rows(
  tab_boot(boot_m, "male"),
  tab_boot(boot_f, "female")
)

cat("\n========================\nBOOTSTRAP INDIRECT SIGNIFICANCE (BY GROUP)\n========================\n")
print(boot_tbl)

# -------------------------
# 9) Moderated mediation: diff in indirect (male - female) + significance
# -------------------------
diff_test <- function(draw_m, draw_f) {
  Bm <- length(draw_m); Bf <- length(draw_f)
  Bmin <- min(Bm, Bf)
  d <- draw_m[1:Bmin] - draw_f[1:Bmin]
  d <- d[is.finite(d)]

  est <- mean(d, na.rm = TRUE)
  ci <- quantile(d, probs = c(0.025, 0.975), names = FALSE, na.rm = TRUE)

  p_lo <- mean(d <= 0, na.rm = TRUE)
  p_hi <- mean(d >= 0, na.rm = TRUE)
  p_two <- 2 * min(p_lo, p_hi)
  p_two <- min(p_two, 1)

  c(estimate = est, ci_low = ci[1], ci_high = ci[2], p_value = p_two)
}

diff_pv  <- diff_test(boot_m$draws$ind_pv,  boot_f$draws$ind_pv)
diff_att <- diff_test(boot_m$draws$ind_att, boot_f$draws$ind_att)
diff_img <- diff_test(boot_m$draws$ind_img, boot_f$draws$ind_img)

diff_tbl <- data.frame(
  effect = c(
    "diff: perceived_value -> loyalty_item -> loyalty_retailer (male - female)",
    "diff: attitude -> loyalty_item -> loyalty_retailer (male - female)",
    "diff: retailer_img -> loyalty_item -> loyalty_retailer (male - female)"
  ),
  estimate = c(diff_pv["estimate"],  diff_att["estimate"],  diff_img["estimate"]),
  ci_low   = c(diff_pv["ci_low"],    diff_att["ci_low"],    diff_img["ci_low"]),
  ci_high  = c(diff_pv["ci_high"],   diff_att["ci_high"],   diff_img["ci_high"]),
  p_value  = c(diff_pv["p_value"],   diff_att["p_value"],   diff_img["p_value"])
)

cat("\n========================\nMODERATED MEDIATION (DIFF INDIRECT male - female)\n========================\n")
print(diff_tbl)

# -------------------------
# 10) EXPORT TO EXCEL (sheets)
# -------------------------
out_file <- "pls_2stage_gender_indirect_diffs.xlsx"

write_xlsx(
  x = list(
    sample_sizes = sample_sizes,
    point_indirect = point_tbl,
    bootstrap_indirect_by_group = boot_tbl,
    diff_indirect = diff_tbl
  ),
  path = out_file
)

cat("\nSaved Excel:", out_file, "\n")
cat("Sheets: sample_sizes, point_indirect, bootstrap_indirect_by_group, diff_indirect\n")

cat("\nREADING RULES:\n")
cat("- Within group: indirect significant if 95% CI does NOT include 0 (or p_value < 0.05).\n")
cat("- Moderated mediation: diff significant if 95% CI does NOT include 0 (or p_value < 0.05).\n")
cat("- If your gender coding 1/2 is reversed, swap mapping in Step 2.\n")
