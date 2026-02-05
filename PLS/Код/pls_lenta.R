
library(readxl)
library(janitor)
library(dplyr)
library(stringr)
library(tidyr)
library(cSEM)
library(tibble)
library(writexl)

# -------------------------
# 0) LOAD DATA (explicit)
# -------------------------
raw <- read_excel("lenta_cfa_sem.xlsx") |> clean_names()

# Explicit variables used in the model (your indicators)
vars <- c(
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
  "uniqueness_item_visit_for_pl_likert",
  # attitude
  "attitude_item_brand_alternative_likert",
  "attitude_item_prefer_available_likert",
  # retailer image
  "retailer_brand_item_logo_quality_trust_likert",
  "retailer_brand_item_quality_guarantee_likert",
  # loyalty item
  "loyalty_item_regular_purchase_likert",
  "loyalty_item_prefer_over_brands_likert",
  "loyalty_item_recommend_pl_likert",
  # loyalty retailer
  "loyalty_retailer_regular_purchase_likert",
  "loyalty_retailer_prefer_over_brands_likert",
  "loyalty_retailer_recommend_pl_likert"
)

missing_cols <- setdiff(vars, names(raw))
if (length(missing_cols) > 0) stop("Missing columns:\n", paste(missing_cols, collapse = "\n"))

# Subset + numeric + drop NA rows (explicit preprocessing)
df <- raw |>
  select(all_of(vars)) |>
  mutate(across(everything(), \(x) {
    if (is.numeric(x)) return(x)
    if (is.factor(x) || is.ordered(x)) return(as.numeric(x))
    suppressWarnings(as.numeric(as.character(x)))
  })) |>
  drop_na()

cat("\nDATA USED FOR ESTIMATION:\n")
cat("- file:", "all_cfa_sem.xlsx", "\n")
cat("- n rows after drop_na:", nrow(df), "\n")
cat("- columns used:", ncol(df), "\n")

# -------------------------
# 1) MODEL (explicit): 2nd order + mediation
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

# structural (mediation)
loyalty_item ~ perceived_value + attitude + retailer_img
loyalty_retailer ~ loyalty_item
"

# -------------------------
# 2) FIT + BOOTSTRAP (explicit)
# -------------------------
set.seed(42)
res <- csem(
  .data = df,
  .model = model,
  .approach_weights = "PLS-PM",
  .resample_method = "bootstrap",
  .R = 5000,
  .handle_inadmissibles = "drop"
)

# -------------------------
# 3) PARSE summarize(res) FOR DIRECT + INDIRECT (beta, p, CI)
# (two-stage friendly: summarize prints inference even if not stored in slots)
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
    ci_l <- as.numeric(ci[,2])
    ci_u <- as.numeric(ci[,3])

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

txt <- capture.output(cSEM::summarize(res))

direct_paths <- parse_block(txt, "Estimated path coefficients") |>
  mutate(effect_type = "direct")

indirect_paths <- parse_block(txt, "Estimated indirect effects") |>
  mutate(effect_type = "indirect")

if (nrow(direct_paths) == 0) stop("No direct paths parsed — summarize(res) format changed.")
if (nrow(indirect_paths) == 0) warning("No indirect paths parsed (check mediation structure).")

# -------------------------
# 4) R2 (explicit from second stage estimates)
# -------------------------
R2 <- res$Second_stage$Estimates$R2
r2_tbl <- tibble(
  construct = names(R2),
  r2 = as.numeric(R2)
)

# -------------------------
# 5) EXPORT
# -------------------------
out_file <- "lenta_pls_2stage_direct_indirect_r2.xlsx"

write_xlsx(
  list(
    direct_paths = direct_paths,
    indirect_paths = indirect_paths,
    r2 = r2_tbl
  ),
  path = out_file
)

cat("\nSaved:", out_file, "\n")
cat("Sheets: direct_paths, indirect_paths, r2\n")
