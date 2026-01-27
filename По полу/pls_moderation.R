library(readxl)
library(janitor)
library(dplyr)
library(tidyr)
library(cSEM)
library(stringr)

# ============================================================
# PLS-SEM: mediation + strict bootstrap significance of INDIRECT
# Moderation by gender = compare indirect effects between groups
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
  "socdec_gender"
)

missing_cols <- setdiff(needed, names(all_q))
if (length(missing_cols) > 0) {
  stop("Missing columns:\n", paste(missing_cols, collapse = "\n"))
}

# -------------------------
# 2) Clean gender: keep only male/female (drop "prefer not to answer")
#    - robust for RU/EN strings
#    - if socdec_gender is numeric (1/2), assumes 1=male, 2=female (edit if needed)
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
      gender_clean == "1" ~ "male",   # if your coding is opposite, swap these
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
    suppressWarnings(as.numeric(x))
  })) |>
  drop_na()

# Split groups
pls_male   <- pls_df |> filter(gender_clean == "male")   |> select(-socdec_gender, -gender_clean)
pls_female <- pls_df |> filter(gender_clean == "female") |> select(-socdec_gender, -gender_clean)

cat("\nN male:", nrow(pls_male), " | N female:", nrow(pls_female), "\n")
if (nrow(pls_male) < 30 || nrow(pls_female) < 30) {
  warning("One of the groups has <30 observations. Results may be unstable.")
}

# -------------------------
# 4) PLS-SEM mediation model (partial mediation)
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

# structural (mediation)
loyalty_item ~ perceived_value + attitude + retailer_img
loyalty_retailer ~ loyalty_item + perceived_value + attitude + retailer_img
"

# -------------------------
# 5) helper: fit PLS once and extract path table
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

# Point indirect effects from a path table
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

    pe <- fit_pls_once(dat_b)

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

    # two-sided bootstrap p-value via sign proportion
    p_lo <- mean(x <= 0, na.rm = TRUE)
    p_hi <- mean(x >= 0, na.rm = TRUE)
    p_two <- 2 * min(p_lo, p_hi)
    p_two <- min(p_two, 1)

    list(
      estimate = est,
      ci_low = ci[1],
      ci_high = ci[2],
      p_value = p_two
    )
  }

  list(
    ind_pv  = summarize_dist(ind_pv),
    ind_att = summarize_dist(ind_att),
    ind_img = summarize_dist(ind_img),
    draws = list(ind_pv = ind_pv, ind_att = ind_att, ind_img = ind_img)
  )
}

# -------------------------
# 7) Point indirect effects (no bootstrap) by group
# -------------------------
pe_m <- fit_pls_once(pls_male)
pe_f <- fit_pls_once(pls_female)

cat("\n========================\nPOINT INDIRECT EFFECTS\n========================\n")
point_tbl <- dplyr::bind_rows(
  data.frame(group = "male",   t(ind_point_from_pe(pe_m)), check.names = FALSE),
  data.frame(group = "female", t(ind_point_from_pe(pe_f)), check.names = FALSE)
)
names(point_tbl) <- c("group", "indirect_pv", "indirect_attitude", "indirect_retailer_img")
print(point_tbl)

# -------------------------
# 8) Bootstrap indirect significance by group
# -------------------------
B <- 5000  # for quick debug use 1000-2000, for final run 5000

boot_m <- bootstrap_indirect(pls_male,   B = B, seed = 100, label = "male")
boot_f <- bootstrap_indirect(pls_female, B = B, seed = 200, label = "female")

tab_boot <- function(boot, group) {
  data.frame(
    group = group,
    effect = c("indirect_pv", "indirect_attitude", "indirect_retailer_img"),
    estimate = c(boot$ind_pv$estimate, boot$ind_att$estimate, boot$ind_img$estimate),
    ci_low   = c(boot$ind_pv$ci_low,   boot$ind_att$ci_low,   boot$ind_img$ci_low),
    ci_high  = c(boot$ind_pv$ci_high,  boot$ind_att$ci_high,  boot$ind_img$ci_high),
    p_value  = c(boot$ind_pv$p_value,  boot$ind_att$p_value,  boot$ind_img$p_value)
  )
}

cat("\n========================\nBOOTSTRAP INDIRECT SIGNIFICANCE (BY GROUP)\n========================\n")
boot_tbl <- bind_rows(
  tab_boot(boot_m, "male"),
  tab_boot(boot_f, "female")
)
print(boot_tbl)

# -------------------------
# 9) Moderated mediation: test diff in indirect (male - female)
# -------------------------
diff_test <- function(draw_m, draw_f) {
  Bm <- length(draw_m); Bf <- length(draw_f)
  Bmin <- min(Bm, Bf)
  d <- draw_m[1:Bmin] - draw_f[1:Bmin]

  est <- mean(d, na.rm = TRUE)
  ci <- quantile(d, probs = c(0.025, 0.975), names = FALSE, na.rm = TRUE)

  p_lo <- mean(d <= 0, na.rm = TRUE)
  p_hi <- mean(d >= 0, na.rm = TRUE)
  p_two <- 2 * min(p_lo, p_hi)
  p_two <- min(p_two, 1)

  c(estimate = est, ci_low = ci[1], ci_high = ci[2], p_value = p_two)
}

cat("\n========================\nMODERATED MEDIATION (DIFF INDIRECT male - female)\n========================\n")

diff_pv  <- diff_test(boot_m$draws$ind_pv,  boot_f$draws$ind_pv)
diff_att <- diff_test(boot_m$draws$ind_att, boot_f$draws$ind_att)
diff_img <- diff_test(boot_m$draws$ind_img, boot_f$draws$ind_img)

diff_tbl <- data.frame(
  effect = c("diff_indirect_pv", "diff_indirect_attitude", "diff_indirect_retailer_img"),
  estimate = c(diff_pv["estimate"],  diff_att["estimate"],  diff_img["estimate"]),
  ci_low   = c(diff_pv["ci_low"],    diff_att["ci_low"],    diff_img["ci_low"]),
  ci_high  = c(diff_pv["ci_high"],   diff_att["ci_high"],   diff_img["ci_high"]),
  p_value  = c(diff_pv["p_value"],   diff_att["p_value"],   diff_img["p_value"])
)
print(diff_tbl)

cat("\nREADING RULES:\n")
cat("- Within group: indirect effect is significant if 95% CI does NOT include 0 (or p_value < 0.05).\n")
cat("- Moderated mediation: diff_indirect_* significant if its CI does NOT include 0 (or p_value < 0.05).\n")
cat("- If you suspect gender coding 1/2 is reversed, swap mapping in Step 2.\n")
