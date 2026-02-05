# ============================================================
# FULL PIPELINE (single cell):
# 1) select socdec vars
# 2) Gower + hierarchical clustering
# 3) cut k=3
# 4) name clusters based on profiles
# 5) test differences across clusters for clustering vars (Chi-square / Fisher)
# 6) add cluster (named) to all_q
# 7) save clustered.xlsx
# ============================================================

library(dplyr)
library(cluster)
library(writexl)

# ---- 0) Inputs ----
# all_q must already exist in the environment (e.g., read_excel(...) |> clean_names())
socdec_vars <- c(
  "socdec_age_cat",
  "socdec_overall_purchase_frequency",
  "socdec_primary_shopper",
  "socdec_household_size",
  "socdec_residence_type"
)

# ---- 1) Checks ----
missing_vars <- setdiff(socdec_vars, names(all_q))
if (length(missing_vars) > 0) {
  stop(paste("Missing variables in all_q:", paste(missing_vars, collapse = ", ")))
}

cat("Variables used for clustering:\n")
print(socdec_vars)

# ---- 2) Select + cleanup ----
soc <- all_q %>% select(all_of(socdec_vars))
soc <- soc %>% select(where(~ !all(is.na(.))))  # drop all-NA columns just in case

keep_rows <- rowSums(!is.na(soc)) > 0          # keep rows with at least one value
soc <- soc[keep_rows, , drop = FALSE]

# ---- 3) Coerce types safely (categorical/ordinal where appropriate) ----
to_factor_safely <- function(x) {
  if (is.factor(x) || is.ordered(x)) return(x)
  if (is.character(x)) return(factor(x))
  if (is.logical(x)) return(factor(x))
  
  if (is.numeric(x) || is.integer(x)) {
    ux <- sort(unique(x[!is.na(x)]))
    # small integer-coded scales -> treat as ordinal categories
    if (length(ux) <= 15 && all(abs(ux - round(ux)) < 1e-9)) {
      return(ordered(as.character(x), levels = as.character(ux)))
    }
    # truly numeric -> leave numeric (Gower supports numeric)
    return(x)
  }
  
  factor(as.character(x))
}

soc2 <- soc %>% mutate(across(everything(), to_factor_safely))

# ---- 4) Gower distance + hierarchical clustering ----
gower_dist <- daisy(soc2, metric = "gower")
hc_gower <- hclust(gower_dist, method = "average")

# Plot dendrogram (interactive sessions show it in Plots)
plot(
  hc_gower,
  main = "Hierarchical clustering (Gower) on selected socdec variables",
  xlab = "",
  sub = "",
  cex = 0.6
)

# ---- 5) Cut into k clusters ----
k <- 3
clusters <- cutree(hc_gower, k = k)

cat("\nCluster sizes (k = ", k, "):\n", sep = "")
print(table(clusters))

# ---- 6) Name clusters (based on your observed profiles) ----
cluster_labels <- c(
  "Mainstream individual shoppers",   # cluster 1
  "Young shared-decision shoppers",   # cluster 2
  "Urban family-oriented shoppers"    # cluster 3
)

cluster_named <- factor(clusters, levels = 1:3, labels = cluster_labels)

# ---- 7) Test differences between clusters for clustering variables ----
# (Chi-square; fallback to Fisher if expected < 5)
cluster_tests <- lapply(names(soc2), function(v) {
  tab <- table(soc2[[v]], cluster_named, useNA = "no")
  
  # If table is degenerate (e.g., only 1 level), handle safely
  if (min(dim(tab)) < 2) {
    return(data.frame(
      variable = v,
      test = "Not testable (single level)",
      p_value = NA_real_
    ))
  }
  
  chi <- suppressWarnings(chisq.test(tab))
  
  if (any(chi$expected < 5)) {
    # Fisher can be heavy for big tables; tryCatch to avoid crashes
    p_val <- tryCatch(
      fisher.test(tab)$p.value,
      error = function(e) NA_real_
    )
    test_type <- ifelse(is.na(p_val), "Fisher (failed; table too large?)", "Fisher exact")
  } else {
    p_val <- chi$p.value
    test_type <- "Chi-square"
  }
  
  data.frame(variable = v, test = test_type, p_value = p_val)
})

cluster_tests_df <- bind_rows(cluster_tests) %>%
  arrange(p_value)

cat("\n--- Cluster differences on clustering variables ---\n")
print(cluster_tests_df)

# ---- 8) Add named cluster to all_q and save ----
all_q$cluster <- NA_character_
all_q$cluster[which(keep_rows)] <- as.character(cluster_named)

cat("\nCluster label counts in all_q:\n")
print(table(all_q$cluster, useNA = "ifany"))

write_xlsx(all_q, path = "clustered.xlsx")
cat("\nSaved: clustered.xlsx\n")
