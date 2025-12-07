required_packages <- c("tidyverse", "here", "gganimate", "gifski", "transformr")

# Install any missing packages automatically
for (pkg in required_packages) {
  if (!require(pkg, character.only = TRUE)) {
    install.packages(pkg, dependencies = TRUE)
  }
  library(pkg, character.only = TRUE)
}
dat <- read_csv(here("Data", "BioLog_Plate_Data.csv"))
cat("\n--- COLUMN NAMES IN YOUR DATA ---\n")
print(colnames(dat))
cat("\n--------------------------------\n")

##tidy data
dat_tidy <- dat %>%
  pivot_longer(
    cols = starts_with("Hr_"),        
    names_to = "Time_hr",
    values_to = "Absorbance"
  ) %>%
  mutate(
    Time_hr = as.numeric(str_replace(Time_hr, "Hr_", "")),
    
    SourceType = case_when(
      str_detect(get(names(dat)[1]), "Soil") ~ "Soil",   
      str_detect(get(names(dat)[1]), "Water") ~ "Water",
      TRUE ~ "Other"
    )
  )

glimpse(dat_tidy)


#Static plot
plot_static <- dat_tidy %>%
  filter(Dilution == 0.1) %>%
  ggplot(aes(x = Time_hr, y = Absorbance, color = SourceType)) +
  geom_point(alpha = 0.7) +
  geom_line(aes(group = interaction(Substrate, SourceType)), alpha = 0.5) +
  facet_wrap(~Substrate, scales = "free_y") +
  theme_bw() +
  labs(
    title = "Carbon Source Utilization at Dilution 0.1",
    subtitle = "Absorbance over Time by Source Type",
    x = "Time (hours)",
    y = "Absorbance"
  )

if (!dir.exists(here("Output"))) dir.create(here("Output"))

ggsave(here("Output", "plot_static.png"), plot = plot_static, width = 10, height = 6)

#animated plot

itaconic_avg <- dat_tidy %>%
  filter(Substrate == "Itaconic Acid") %>%
  group_by(SourceType, Dilution, Time_hr) %>%
  summarise(MeanAbs = mean(Absorbance, na.rm = TRUE), .groups = "drop")

anim_plot <- ggplot(itaconic_avg, aes(x = Time_hr, y = MeanAbs, color = SourceType)) +
  geom_line(size = 1.2) +
  geom_point(size = 2) +
  theme_minimal() +
  labs(
    title = "Utilization of Itaconic Acid Over Time",
    subtitle = "Mean Absorbance by Source Type",
    x = "Time (hours)",
    y = "Mean Absorbance"
  ) +
  transition_states(Dilution, transition_length = 2, state_length = 1) +
  labs(title = 'Dilution: {closest_state}')

# Save animated GIF
anim_save(here("Output", "Itaconic_Acid_Animated.gif"), animation = anim_plot)

ggplot(dat_filtered, aes(x = Substrate, y = Absorbance, fill = SourceType)) +
  geom_boxplot() +
  labs(title = "Carbon Source Utilization at Dilution 0.1",
       x = "Substrate",
       y = "Absorbance (mean of replicates)") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5))

print(
  ggplot(dat_filtered, aes(x = Substrate, y = Absorbance, fill = SourceType)) +
    geom_boxplot() +
    labs(title = "Carbon Source Utilization at Dilution 0.1",
         x = "Substrate",
         y = "Absorbance (mean of replicates)") +
    theme_minimal() +
    theme(axis.text.x = element_text(angle = 90, vjust = 0.5))
)
