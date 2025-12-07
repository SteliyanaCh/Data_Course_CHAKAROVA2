library(tidyverse)
library(gganimate)
dat <- read_csv("C:/Users/siska/Desktop/Data_Course_CHAKAROVA2/Data/BioLog_Plate_Data.csv")

tidy_dat <- dat %>%
  pivot_longer(
    cols = starts_with("Hr_"),        
    names_to = "Time",
    values_to = "Absorbance"
  ) %>%
  mutate(
    Time = as.numeric(str_remove(Time, "Hr_")),
    # Create Source column based on Sample ID
    Source = if_else(str_detect(`Sample ID`, regex("soil", ignore_case = TRUE)),
                     "Soil", "Water")
  )

tidy_0.1 <- tidy_dat %>%
  filter(Dilution == 0.1)

static_plot <- ggplot(tidy_0.1, aes(x = Time, y = Absorbance, color = Source, group = `Sample ID`)) +
  geom_line(size = 1) +
  theme_minimal(base_size = 14) +
  labs(
    title = "Growth Curves at Dilution 0.1",
    x = "Time (hours)",
    y = "Absorbance (OD)",
    color = "Sample Source"
  ) +
  theme(plot.title = element_text(face = "bold", hjust = 0.5))

print(static_plot)


tidy_itac <- tidy_dat %>%
  filter(Dilution == 0.1, Substrate == "Itaconic Acid") %>%
  group_by(Source, Time) %>%
  summarise(
    Mean_Absorbance = mean(Absorbance, na.rm = TRUE),
    SD_Absorbance = sd(Absorbance, na.rm = TRUE),  
    .groups = "drop"
  )


animated_plot <- ggplot(tidy_itac, aes(x = Time, y = Mean_Absorbance, color = Source)) +
  geom_line(size = 1.2) +
  geom_point(size = 2) +
  # Optional: add shading for ± SD
  geom_ribbon(aes(ymin = Mean_Absorbance - SD_Absorbance,
                  ymax = Mean_Absorbance + SD_Absorbance,
                  fill = Source), alpha = 0.2, color = NA) +
  theme_minimal(base_size = 14) +
  labs(
    title = "Growth Curve on Itaconic Acid (Dilution 0.1)",
    x = "Time (hours)",
    y = "Mean Absorbance (OD)",
    color = "Sample Source",
    fill = "Sample Source"
  ) +
  transition_reveal(Time) +
  theme(plot.title = element_text(face = "bold", hjust = 0.5))

animate(animated_plot, width = 800, height = 500, fps = 20)

