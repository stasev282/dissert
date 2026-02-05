library(readxl)
library(janitor)
library(dplyr)
library(stringr)
library(tidyr)
library(cSEM)
library(tibble)
library(writexl)

# ============================================================
# Two-stage HCM + Moderation (gender):
# - run csem bootstrap separately in male/female
# - parse summarize(res) blocks (etalon-style) for beta/se/t/p/CI
# - proof of moderation: bootstrap DIFF (male - female) for effects
# Data: all_cfa_sem.xlsx
# Moderator: socdec_gender
# ============================================================

# -------------------------
# 0) LOAD DATA
# -------------------------
raw <- read_excel("all_cfa_sem.xlsx") |> clean_names()

vars <- c(
  "assortment_item_category_coverage_likert",
  "assortment_item_price_range_likert",
  "assortment_item_wide_choice_likert",
  "benefit_item_choose_same_price_likert",
  "benefit_item_saves_money_likert",
  "benefit_item_value_money_likert",
  "uniqueness_item_new_interest_likert",
  "uniqueness_item_unique_features_likert",
  "uniqueness_item_visit_for_pl_likert",
  "attitude_item_brand_alternative_likert",
  "attitude_item_prefer_available_likert",
  "retailer_brand_item_logo_quality_trust_likert",
  "retailer_brand_item_quality_guarantee_likert",
  "loyalty_item_regular_purchase_likert",
  "loyalty_item_prefer_over_brands_likert",
  "loyalty_item_recommend_pl_likert",
  "loyalty_retailer_regular_purchase_likert",
  "loyalty_retailer_prefer_over_brands_likert",
  "loyalty_retailer_recommend_pl_likert",
  "socdec_gender"
)

missing_cols <- setdiff(vars, names(raw))
if (length(missing_cols) > 0) stop("Missing columns:\n", paste(missing_cols, collapse = "\n"))

# -------------------------
# 1) CLEAN gender + numeric + drop_na
# -------------------------
df0 <- raw |>
  select(all_of(vars)) |>
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
      gender_clean == "1" ~ "male",    # swap if your coding is opposite
      gender_clean == "2" ~ "female",
      TRUE ~ gender_clean
    )
  ) |>
  filter(!is.na(gender_clean)) |>
  select(-g)

df_num <- df0 |>
  mutate(across(-c(socdec_gender, gender_clean), \(x) {
    if (is.numeric(x)) return(x)
    if (is.factor(x) || is.ordered(x)) return(as.numeric(x))
    suppressWarnings(as.numeric(as.character(x)))
  })) |>
  drop_na()

male_df   <- df_num |> filter(gender_clean == "male")   |> select(-socdec_gender, -gender_clean)
female_df <- df_num |> filter(gender_clean == "female") |> select(-socdec_gender, -gender_clean)

cat("\nN male:", nrow(male_df), "| N female:", nrow(female_df), "\n")

# -------------------------
# 2) MODEL: two-stage HCM + mediation
# -------------------------
model <- "
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

perceived_value =~ assortment + benefit + uniqueness

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

# structural (partial mediation)
loyalty_item ~ perceived_value + attitude + retailer_img
loyalty_retailer ~ loyalty_item + perceived_value + attitude + retailer_img
"

# -------------------------
# 3) PARSER (same as your etalon)
# -------------------------
parse_block <- function(txt, block_title) {
  i0 <- grep(paste0("^", block_title, ":"), txt)
  if (length(i0) == 0) return(tibble())

  start <- min(grep("~", txt[(i0[1]):length(txt)])) + i0[1] - 1

  end_candidates <- c(
    grep("^Estimated loadings:", txt),
    grep("^Estimated weights:", txt),
    grep("^Estimated construct correlations:", txt),
    grep("^------------------------------------", txt),
    length(txt) + 1
  )
  end <- min(end_candidates[end_candidates > start]) - 1

  block <- txt[start:end]
  block <- block[nzchar(trimws(block))]
  block <- block[str_detect(block, "\\s~\\s")]

  parse_line <- function(s) {
    s <- str_squish(s)

    ci <- str_match(s, "\\[\\s*([\\-0-9\\.]+)\\s*;\\s*([\\-0-9\\.]+)\\s*\\]$")

    ci_l <- suppressWarnings(as.numeric(ci[,2]))
    ci_u <- suppressWarnings(as.numeric(ci[,3]))

    s0 <- str_replace(s, "\\[.*\\]$", "") |> str_squish()

    nums <- str_extract_all(s0, "[-]?[0-9]+\\.[0-9]+")[[1]]
    if (length(nums) < 4) return(NULL)
    tail4 <- as.numeric(tail(nums, 4))

    est <- tail4[1]
    se  <- tail4[2]
    t   <- tail4[3]
    p   <- tail4[4]

    pos_first_num <- regexpr("[-]?[0-9]+\\.[0-9]+", s0)[1]
    path_part <- str_trim(substr(s0, 1, pos_first_num - 1))

    lhs <- str_trim(str_split_fixed(path_part, "~", 2)[,1])
    rhs <- str_trim(str_split_fixed(path_part, "~", 2)[,2])

    tibble(
      from = rhs,
      to = lhs,
      path = paste0(rhs, " -> ", lhs),
      beta = est,
      se = se,
      t_stat = t,
      p_value = p,
      ci_95L = ci_l,
      ci_95U = ci_u
    )
  }

  bind_rows(lapply(block, parse_line))
}

extract_tables_from_res <- function(res, group_label) {
  txt <- capture.output(cSEM::summarize(res))

  direct <- parse_block(txt, "Estimated path coefficients") |>
    mutate(effect_type = "direct", group = group_label)

  indirect <- parse_block(txt, "Estimated indirect effects") |>
    mutate(effect_type = "indirect", group = group_label)

  r2 <- res$Second_stage$Estimates$R2
  r2_tbl <- tibble(group = group_label, construct = names(r2), r2 = as.numeric(r2))

  list(direct = direct, indirect = indirect, r2 = r2_tbl)
}

# -------------------------
# 4) FIT + BOOTSTRAP (per group)
# -------------------------
B <- 5000
set.seed(42)

res_m <- csem(
  .data = male_df,
  .model = model,
  .approach_weights = "PLS-PM",
  .resample_method = "bootstrap",
  .R = B,
  .handle_inadmissibles = "drop"
)

res_f <- csem(
  .data = female_df,
  .model = model,
  .approach_weights = "PLS-PM",
  .resample_method = "bootstrap",
  .R = B,
  .handle_inadmissibles = "drop"
)

tab_m <- extract_tables_from_res(res_m, "male")
tab_f <- extract_tables_from_res(res_f, "female")

direct_m <- tab_m$direct
direct_f <- tab_f$direct
indirect_m <- tab_m$indirect
indirect_f <- tab_f$indirect
r2_m <- tab_m$r2
r2_f <- tab_f$r2

if (nrow(direct_m) == 0 || nrow(direct_f) == 0) stop("Direct paths parse failed — summarize(res) format changed.")
if (nrow(indirect_m) == 0 || nrow(indirect_f) == 0) warning("Indirect parse empty in a group — check mediation or summarize format.")

# -------------------------
# 5) Moderation proof via bootstrap DIFF (male - female)
# We compute diff on bootstrap draws (not on parsed text).
# -------------------------

# helper: safe get cell from matrix by names
get_cell <- function(mat, row, col) {
  if (!is.matrix(mat)) return(NA_real_)
  rn <- rownames(mat); cn <- colnames(mat)
  if (is.null(rn) || is.null(cn)) return(NA_real_)
  if (!(row %in% rn) || !(col %in% cn)) return(NA_real_)
  as.numeric(mat[row, col])
}

summ_diff <- function(d) {
  d <- d[is.finite(d)]
  est <- mean(d)
  ci <- quantile(d, probs = c(0.025, 0.975), names = FALSE, na.rm = TRUE)
  p_lo <- mean(d <= 0, na.rm = TRUE)
  p_hi <- mean(d >= 0, na.rm = TRUE)
  p_two <- min(2 * min(p_lo, p_hi), 1)
  tibble(estimate = est, ci_low = ci[1], ci_high = ci[2], p_value = p_two)
}

# We will diff:
# - direct paths (from Resample$Estimates$Path_estimates)
# - indirect effects (from Resample$Estimates$Indirect_effects) if present
# If indirect slot missing, we rebuild indirect = (a*b) from path matrices.

Bm <- length(res_m$Resample$Estimates$Path_estimates)
Bf <- length(res_f$Resample$Estimates$Path_estimates)
Bmin <- min(Bm, Bf)

# define which direct paths to test moderation for
direct_targets <- tribble(
  ~to,              ~from,
  "loyalty_item",    "perceived_value",
  "loyalty_item",    "attitude",
  "loyalty_item",    "retailer_img",
  "loyalty_retailer","loyalty_item",
  "loyalty_retailer","perceived_value",
  "loyalty_retailer","attitude",
  "loyalty_retailer","retailer_img"
)

# direct diff table
diff_direct <- bind_rows(lapply(seq_len(nrow(direct_targets)), \(k) {
  lhs <- direct_targets$to[[k]]
  rhs <- direct_targets$from[[k]]

  dm <- vapply(seq_len(Bmin), \(i) get_cell(res_m$Resample$Estimates$Path_estimates[[i]], lhs, rhs), numeric(1))
  df <- vapply(seq_len(Bmin), \(i) get_cell(res_f$Resample$Estimates$Path_estimates[[i]], lhs, rhs), numeric(1))

  tibble(effect = paste0("diff_direct: ", rhs, " -> ", lhs)) |>
    bind_cols(summ_diff(dm - df))
}))

# indirect diff table
have_ind_m <- !is.null(res_m$Resample$Estimates$Indirect_effects)
have_ind_f <- !is.null(res_f$Resample$Estimates$Indirect_effects)

# define which indirect effects to test moderation for (via loyalty_item mediator)
ind_targets <- tribble(
  ~x,               ~m,           ~y,
  "perceived_value", "loyalty_item","loyalty_retailer",
  "attitude",        "loyalty_item","loyalty_retailer",
  "retailer_img",    "loyalty_item","loyalty_retailer"
)

diff_indirect <- bind_rows(lapply(seq_len(nrow(ind_targets)), \(k) {
  x <- ind_targets$x[[k]]
  m <- ind_targets$m[[k]]
  y <- ind_targets$y[[k]]

  if (have_ind_m && have_ind_f) {
    # try to get indirect from Indirect_effects matrices
    dm <- vapply(seq_len(Bmin), \(i) get_cell(res_m$Resample$Estimates$Indirect_effects[[i]], y, x), numeric(1))
    df <- vapply(seq_len(Bmin), \(i) get_cell(res_f$Resample$Estimates$Indirect_effects[[i]], y, x), numeric(1))
  } else {
    # rebuild as (x -> m) * (m -> y) from path matrices
    dm <- vapply(seq_len(Bmin), \(i) {
      pe <- res_m$Resample$Estimates$Path_estimates[[i]]
      get_cell(pe, m, x) * get_cell(pe, y, m)
    }, numeric(1))

    df <- vapply(seq_len(Bmin), \(i) {
      pe <- res_f$Resample$Estimates$Path_estimates[[i]]
      get_cell(pe, m, x) * get_cell(pe, y, m)
    }, numeric(1))
  }

  tibble(effect = paste0("diff_indirect: ", x, " -> ", m, " -> ", y)) |>
    bind_cols(summ_diff(dm - df))
}))

# -------------------------
# 6) EXPORT
# -------------------------
out_file <- "pls_2stage_gender_moderation_proofs.xlsx"

sample_info <- tibble(
  file = "all_cfa_sem.xlsx",
  moderator = "socdec_gender",
  n_male = nrow(male_df),
  n_female = nrow(female_df),
  B = B,
  note = "Two-stage inference taken from summarize(res) parsing (direct/indirect with beta,se,t,p,CI). Moderation proof = bootstrap diff (male-female) CI/p."
)

write_xlsx(
  list(
    sample_info = sample_info,
    direct_male = direct_m,
    direct_female = direct_f,
    indirect_male = indirect_m,
    indirect_female = indirect_f,
    r2_male = r2_m,
    r2_female = r2_f,
    diff_direct_male_minus_female = diff_direct,
    diff_indirect_male_minus_female = diff_indirect
  ),
  path = out_file
)

cat("\nSaved:", out_file, "\n")
cat("Sheets: sample_info, direct_*, indirect_*, r2_*, diff_direct_*, diff_indirect_*\n")
cat("\nREADING RULES:\n")
cat("- Within group: effect significant if 95% CI does NOT include 0 (or p_value < 0.05).\n")
cat("- Moderation: diff_* significant if its CI does NOT include 0 (or p_value < 0.05).\n")
