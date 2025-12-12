##############################################
# Cell-specific protein expression in Alzheimer's disease
# Author: Steliyana Chakarova
# Date: 12/12/2025
##############################################
setwd("C:/Users/siska/Desktop/Data_Course_CHAKAROVA2/Final_Project")


library(tidyverse)
library(purrr)
library(pheatmap)

# ===========================================================
# 1) Simulate clean proteomics dataset
# ===========================================================
set.seed(123)

# Define proteins and samples
proteins <- paste0("Protein", 1:50)
samples <- paste0("Sample", 1:12)
group <- rep(c("Control", "AD"), each = 6)

# Expand grid to long format
dat_long <- expand.grid(Sample = samples, Protein = proteins) %>%
  mutate(
    Group = rep(group, times = length(proteins)),
    Intensity = rnorm(nrow(.),
                      mean = ifelse(Group == "Control", 100, 120), # AD slightly higher
                      sd = 15)
  )


head(dat_long)

# ===========================================================
# 2) Summary statistics
# ===========================================================
summary_stats <- dat_long %>%
  group_by(Protein, Group) %>%
  summarise(
    mean_intensity = mean(Intensity),
    sd_intensity = sd(Intensity),
    n = n(),
    .groups = "drop"
  )

head(summary_stats)

# ===========================================================
# 3) Statistical comparison: t-tests
# ===========================================================
valid_proteins <- dat_long %>%
  group_by(Protein) %>%
  summarise(n_groups = n_distinct(Group), .groups = "drop") %>%
  filter(n_groups == 2) %>%
  pull(Protein)

t_test_results <- dat_long %>%
  filter(Protein %in% valid_proteins) %>%
  group_by(Protein) %>%
  summarise(
    t_test = list(t.test(Intensity ~ Group)),
    .groups = "drop"
  ) %>%
  mutate(
    estimate = map_dbl(t_test, ~ .$estimate[2] - .$estimate[1]),
    p_value = map_dbl(t_test, ~ .$p.value)
  ) %>%
  select(Protein, estimate, p_value) %>%
  arrange(p_value)

head(t_test_results)

# ===========================================================
# 4) Plot top 6 proteins
# ===========================================================
top_proteins <- t_test_results %>%
  slice_min(p_value, n = 6, with_ties = FALSE) %>%
  pull(Protein)

ggplot(dat_long %>% filter(Protein %in% top_proteins),
       aes(x = Group, y = Intensity, fill = Group)) +
  geom_violin(trim = FALSE, alpha = 0.5) +
  geom_boxplot(width = 0.2, outlier.shape = NA) +
  geom_jitter(width = 0.15, alpha = 0.6) +
  facet_wrap(~ Protein, scales = "free_y") +
  theme_bw() +
  labs(title = "Top 6 Differentially Expressed Proteins",
       x = "Group",
       y = "Protein Intensity")
top_proteins

# ===========================================================
# 5) Heatmap of top 10 proteins
# ===========================================================
top10_proteins <- t_test_results %>%
  slice_min(p_value, n = 10, with_ties = FALSE) %>%
  pull(Protein)

mat_top <- dat_long %>%
  filter(Protein %in% top10_proteins) %>%
  select(Sample, Protein, Intensity) %>%
  pivot_wider(names_from = Protein, values_from = Intensity, values_fn = mean) %>%
  column_to_rownames("Sample") %>%
  as.matrix()

mat_top_scaled <- t(scale(t(mat_top)))

pheatmap(mat_top_scaled,
         scale = "none",
         clustering_distance_rows = "euclidean",
         clustering_distance_cols = "euclidean",
         clustering_method = "complete",
         color = colorRampPalette(c("blue", "white", "red"))(100),
         main = "Heatmap of Top 10 Proteins")

# ===========================================================
# 6) Summary and Interpretation (notes)
# ===========================================================
# Number of proteins: 50
# Number of samples: 12
# Groups: Control and AD (6 samples each)
#
# Top 6 proteins with lowest p-values (most differential expression):
# (listed in top_proteins variable)
#
# Interpretation:
# The top proteins show higher intensities in AD samples compared to Control,
# reflecting the simulated upregulation in disease. 
# The violin and boxplots illustrate the distribution of protein intensities
# within each group, while the heatmap of the top 10 proteins shows clustering 
# patterns across samples.
# In a real dataset, these analyses could identify potential biomarkers or
# proteins associated with disease pathology.