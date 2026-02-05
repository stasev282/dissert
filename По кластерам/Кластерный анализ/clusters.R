library(dplyr)
library(cluster)
library(writexl)

socdec_vars <- c(
  "socdec_age_cat",
  "socdec_overall_purchase_frequency",
  "socdec_primary_shopper",
  "socdec_household_size",
  "socdec_residence_type"
)

missing_vars <- setdiff(socdec_vars, names(all_q))
if (length(missing_vars) > 0) {
  stop(paste("Missing variables in all_q:", paste(missing_vars, collapse = ", ")))
}

cat("Variables used for clustering:\n")
print(socdec_vars)

soc <- all_q %>% select(all_of(socdec_vars))
soc <- soc %>% select(where(~ !all(is.na(.))))

keep_rows <- rowSums(!is.na(soc)) > 0   
soc <- soc[keep_rows, , drop = FALSE]

to_factor_safely <- function(x) {
  if (is.factor(x) || is.ordered(x)) return(x)
  if (is.character(x)) return(factor(x))
  if (is.logical(x)) return(factor(x))
  
  if (is.numeric(x) || is.integer(x)) {
    ux <- sort(unique(x[!is.na(x)]))
    if (length(ux) <= 15 && all(abs(ux - round(ux)) < 1e-9)) {
      return(ordered(as.character(x), levels = as.character(ux)))
    }
    return(x)
  }
  
  factor(as.character(x))
}

soc2 <- soc %>% mutate(across(everything(), to_factor_safely))

gower_dist <- daisy(soc2, metric = "gower")
hc_gower <- hclust(gower_dist, method = "average")

plot(
  hc_gower,
  main = "Hierarchical clustering (Gower) on selected socdec variables",
  xlab = "",
  sub = "",
  cex = 0.6
)

k <- 3
clusters <- cutree(hc_gower, k = k)

cat("\nCluster sizes (k = ", k, "):\n", sep = "")
print(table(clusters))

cluster_labels <- c(
  "Mainstream individual shoppers",   
  "Young shared-decision shoppers",   
  "Urban family-oriented shoppers"    
)

cluster_named <- factor(clusters, levels = 1:3, labels = cluster_labels)

cluster_tests <- lapply(names(soc2), function(v) {
  tab <- table(soc2[[v]], cluster_named, useNA = "no")
  
  if (min(dim(tab)) < 2) {
    return(data.frame(
      variable = v,
      test = "Not testable (single level)",
      p_value = NA_real_
    ))
  }
  
  chi <- suppressWarnings(chisq.test(tab))
  
  if (any(chi$expected < 5)) {
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

all_q$cluster <- NA_character_
all_q$cluster[which(keep_rows)] <- as.character(cluster_named)

cat("\nCluster label counts in all_q:\n")
print(table(all_q$cluster, useNA = "ifany"))

write_xlsx(all_q, path = "clustered.xlsx")

