# ============================================================
# cSEM 0.6.1 — EXPORT (Magnit):
# - quality_reliabilities
# - quality_r2
# - quality_vif
# - direct_paths (beta + p if available; else MC CI + MC p using SE fallback)
# - indirect_paths_mc (a*b + MC CI + MC p)
# ============================================================

library(readxl)
library(janitor)
library(dplyr)
library(stringr)
library(cSEM)
library(writexl)
library(tibble)

# -------------------------
# 0) load data
# -------------------------
all_q <- read_excel("all_cfa_sem.xlsx") |> clean_names()

# -------------------------
# 1) required indicators
# -------------------------
needed <- c(
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
  "loyalty_retailer_regular_purchase_likert",
  "loyalty_retailer_prefer_over_brands_likert",
  "loyalty_retailer_recommend_pl_likert",
  "loyalty_item_regular_purchase_likert",
  "loyalty_item_prefer_over_brands_likert",
  "loyalty_item_recommend_pl_likert"
)

missing_cols <- setdiff(needed, names(all_q))
if (length(missing_cols) > 0) stop("Missing columns:\n", paste(missing_cols, collapse = "\n"))

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
  tidyr::drop_na()

# -------------------------
# 3) cSEM model (reflective; partial mediation)
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
est <- sum_res$Estimates

# ============================================================
# Helpers
# ============================================================

as_num_safe <- function(x) {
  if (is.list(x)) {
    return(vapply(x, function(z) {
      if (length(z) == 0) return(NA_real_)
      suppressWarnings(as.numeric(z[[1]]))
    }, numeric(1)))
  }
  suppressWarnings(as.numeric(x))
}

numify_df <- function(df) {
  df <- as.data.frame(df)
  for (nm in names(df)) {
    if (is.list(df[[nm]])) df[[nm]] <- as_num_safe(df[[nm]])
  }
  df
}

safe_quality_table <- function(x, name) {
  out_note <- function(msg) tibble(note = paste0("[", name, "] ", msg))
  if (is.null(x)) return(out_note("not available"))

  if (is.atomic(x) && is.vector(x) && !is.null(names(x))) {
    return(tibble(construct = names(x), value = as.numeric(x)))
  }

  if (is.matrix(x) || is.data.frame(x)) {
    df <- as.data.frame(x)
    df$construct <- rownames(df)
    return(df |> dplyr::relocate(construct))
  }

  if (is.list(x) && !is.data.frame(x)) {
    table_idx <- which(vapply(x, function(z) is.data.frame(z) || is.matrix(z), logical(1)))
    if (length(table_idx) > 0) {
      df <- as.data.frame(x[[table_idx[1]]])
      df$construct <- rownames(df)
      return(df |> dplyr::relocate(construct))
    }
    return(out_note("is a list (non-tabular); cannot coerce safely"))
  }

  out_note(paste0("unsupported type: ", paste(class(x), collapse = "/")))
}

# Monte Carlo for DIRECT: Normal(beta, se)
mc_ci_p_direct <- function(beta, se, R = 50000, seed = 42) {
  set.seed(seed)
  if (any(is.na(c(beta, se)))) return(list(ciL = NA_real_, ciU = NA_real_, p_mc = NA_real_))
  sims <- rnorm(R, mean = beta, sd = se)
  ci <- as.numeric(stats::quantile(sims, probs = c(0.025, 0.975), na.rm = TRUE))
  p_mc <- 2 * min(mean(sims <= 0, na.rm = TRUE), mean(sims >= 0, na.rm = TRUE))
  list(ciL = ci[1], ciU = ci[2], p_mc = p_mc)
}

# Monte Carlo for INDIRECT: simulate a and b then multiply
mc_ci_p_indirect <- function(a, se_a, b, se_b, R = 50000, seed = 42) {
  set.seed(seed)
  if (any(is.na(c(a, se_a, b, se_b)))) return(list(est = a*b, ciL = NA_real_, ciU = NA_real_, p_mc = NA_real_))
  a_s <- rnorm(R, mean = a, sd = se_a)
  b_s <- rnorm(R, mean = b, sd = se_b)
  ind <- a_s * b_s
  ci <- as.numeric(stats::quantile(ind, probs = c(0.025, 0.975), na.rm = TRUE))
  p_mc <- 2 * min(mean(ind <= 0, na.rm = TRUE), mean(ind >= 0, na.rm = TRUE))
  list(est = a*b, ciL = ci[1], ciU = ci[2], p_mc = p_mc)
}

# ============================================================
# 5) QUALITY (separate sheets)
# ============================================================
quality_reliabilities <- safe_quality_table(est$Reliabilities, "Reliabilities")
quality_r2            <- safe_quality_table(est$R2, "R2")
quality_vif           <- safe_quality_table(est$VIF, "VIF")

# ============================================================
# 6) DIRECT PATHS (robust SE fallback)
# ============================================================
# ============================================================
# 6) DIRECT PATHS (human-readable + hard check)
# ============================================================

paths_df <- numify_df(est$Path_estimates)
if (!("Name" %in% names(paths_df))) paths_df$Name <- rownames(paths_df)

paths_reg <- paths_df |>
  mutate(
    Name = str_squish(as.character(Name)),
    is_reg = str_detect(Name, "\\s~\\s"),
    lhs = if_else(is_reg, str_trim(str_split_fixed(Name, "~", 2)[,1]), NA_character_),
    rhs = if_else(is_reg, str_trim(str_split_fixed(Name, "~", 2)[,2]), NA_character_)
  ) |>
  filter(is_reg)

col_est <- if ("Estimate" %in% names(paths_reg)) "Estimate" else names(paths_reg)[1]
col_se1 <- names(paths_reg)[str_detect(names(paths_reg), regex("^se$|std_err|std\\.err|std\\.error", ignore_case = TRUE))][1]
col_p   <- names(paths_reg)[str_detect(names(paths_reg), regex("^p$|p_value|p-value", ignore_case = TRUE))][1]

direct_paths <- paths_reg |>
  transmute(
    # IMPORTANT: from/to = rhs -> lhs (so it reads like arrows)
    from = rhs,
    to   = lhs,
    path = paste0(from, " -> ", to),
    beta = as_num_safe(.data[[col_est]]),
    se_raw = if (!is.na(col_se1)) as_num_safe(.data[[col_se1]]) else NA_real_,
    p_value = if (!is.na(col_p)) as_num_safe(.data[[col_p]]) else NA_real_
  )

# SE fallback from est$SE if needed
se_fallback <- NULL
if (!is.null(est$SE)) {
  se_tbl <- est$SE
  if (is.data.frame(se_tbl) || is.matrix(se_tbl)) {
    se_tbl <- as.data.frame(se_tbl)
    se_tbl$Name <- rownames(se_tbl)

    se_col <- names(se_tbl)[str_detect(names(se_tbl), regex("^se$|std_err|std\\.err|std\\.error", ignore_case = TRUE))][1]
    if (is.na(se_col)) {
      num_cols <- names(se_tbl)[vapply(se_tbl, is.numeric, logical(1))]
      if (length(num_cols) > 0) se_col <- num_cols[1]
    }

    if (!is.na(se_col)) {
      se_fallback <- se_tbl |> transmute(Name, se_fb = as_num_safe(.data[[se_col]]))
    }
  }
}

# attach fallback SE by Name = "lhs ~ rhs" (original convention)
if (!is.null(se_fallback)) {
  direct_paths <- direct_paths |>
    mutate(Name = paste0(to, " ~ ", from)) |>
    left_join(se_fallback, by = "Name") |>
    mutate(se = dplyr::coalesce(se_raw, se_fb)) |>
    select(-se_raw, -se_fb, -Name)
} else {
  direct_paths <- direct_paths |> mutate(se = se_raw) |> select(-se_raw)
}

# Monte Carlo CI/p for direct effects (if se exists)
direct_paths$seed_mc <- 100 + seq_len(nrow(direct_paths))

direct_paths <- direct_paths |>
  rowwise() |>
  mutate(
    mc = list(mc_ci_p_direct(beta, se, seed = seed_mc)),
    ci_95L_mc = mc$ciL,
    ci_95U_mc = mc$ciU,
    p_value_mc = mc$p_mc
  ) |>
  ungroup() |>
  select(-mc, -seed_mc)

# HARD CHECK: make sure loyalty_item -> loyalty_retailer exists
if (nrow(direct_paths |> filter(from == "loyalty_item", to == "loyalty_retailer")) == 0) {
  cat("\n[DEBUG] Available direct paths:\n")
  print(direct_paths |> select(path, beta) |> arrange(path))
  stop("Direct path NOT found: loyalty_item -> loyalty_retailer. Check model string or construct names.")
}

# ============================================================
# Helpers for lookup (NOW uses from/to, not lhs/rhs)
# ============================================================

get_beta <- function(to, from) {
  r <- direct_paths |> dplyr::filter(.data$to == .env$to, .data$from == .env$from)
  if (nrow(r) == 0) return(NA_real_)
  r$beta[[1]]
}

get_se <- function(to, from) {
  r <- direct_paths |> dplyr::filter(.data$to == .env$to, .data$from == .env$from)
  if (nrow(r) == 0) return(NA_real_)
  r$se[[1]]
}

# ============================================================
# 7) INDIRECT PATHS (only what you need) — uses to/from
# ============================================================

# a-paths: X -> loyalty_item  =>  loyalty_item is "to"
a_pv   <- get_beta(to = "loyalty_item", from = "perceived_value")
se_a_pv  <- get_se(to = "loyalty_item", from = "perceived_value")

a_att  <- get_beta(to = "loyalty_item", from = "attitude")
se_a_att <- get_se(to = "loyalty_item", from = "attitude")

a_img  <- get_beta(to = "loyalty_item", from = "retailer_img")
se_a_img <- get_se(to = "loyalty_item", from = "retailer_img")

# b-path: loyalty_item -> loyalty_retailer  => loyalty_retailer is "to"
b_li   <- get_beta(to = "loyalty_retailer", from = "loyalty_item")
se_b_li  <- get_se(to = "loyalty_retailer", from = "loyalty_item")

# HARD CHECK (so you instantly know if it’s missing)
if (is.na(b_li)) {
  cat("\n[DEBUG] No direct path found: loyalty_item -> loyalty_retailer\n")
  cat("[DEBUG] Available paths:\n")
  print(direct_paths |> dplyr::select(path, beta) |> dplyr::arrange(path))
  stop("Missing path: loyalty_item -> loyalty_retailer (check model or construct names).")
}

# Indirect effects (a*b) + MC CI/p
ind_pv  <- mc_ci_p_indirect(a_pv,  se_a_pv,  b_li, se_b_li, seed = 201)
ind_att <- mc_ci_p_indirect(a_att, se_a_att, b_li, se_b_li, seed = 202)
ind_img <- mc_ci_p_indirect(a_img, se_a_img, b_li, se_b_li, seed = 203)

indirect_paths <- tibble::tibble(
  effect = c(
    "perceived_value -> loyalty_item -> loyalty_retailer",
    "attitude -> loyalty_item -> loyalty_retailer",
    "retailer_img -> loyalty_item -> loyalty_retailer"
  ),
  estimate   = c(ind_pv$est, ind_att$est, ind_img$est),
  ci_95L_mc  = c(ind_pv$ciL, ind_att$ciL, ind_img$ciL),
  ci_95U_mc  = c(ind_pv$ciU, ind_att$ciU, ind_img$ciU),
  p_value_mc = c(ind_pv$p_mc, ind_att$p_mc, ind_img$p_mc)
)


# ============================================================
# 8) Export Excel (NO total effects)
# ============================================================
out_file <- "pls_all_direct_indirect.xlsx"

write_xlsx(
  x = list(
    quality_reliabilities = quality_reliabilities,
    quality_r2 = quality_r2,
    quality_vif = quality_vif,
    direct_paths = direct_paths,
    indirect_paths_mc = indirect_paths
  ),
  path = out_file
)

cat("\n==============================\n")
cat("Saved Excel:", out_file, "\n")
cat("Sheets: quality_reliabilities, quality_r2, quality_vif, direct_paths, indirect_paths_mc\n")
cat("==============================\n")
