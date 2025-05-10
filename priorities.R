library(vote)
library(tidyverse)
library(readxl)

priorities <- read_excel("data/Priorities 2025.xlsx")
names(priorities) <- str_replace(names(priorities), ".*\\[", "")
names(priorities) <- str_replace(names(priorities), " \\(.*", "")
priorities <- priorities %>%
  select(-Timestamp)
priorities <- priorities %>%
  map(~ str_replace(., ".* prioritized .(\\d).", "\\1")) %>%
  data.frame()
results <- condorcet(priorities[, 2:6], quiet = T)$totals %>%
  data.frame()
ranks <- nrow(results) - results %>%
  rowSums() %>%
  sort(decreasing = T)

print_results <- function() {
  cat("Results of ranked-choice voting in decreasing order of priority:\n")
  for (i in 1:length(names(ranks))) {
    cat(paste0("\n", i, ". ", gsub("\\.", " ", names(ranks)[i], "\n")))
  }
}
