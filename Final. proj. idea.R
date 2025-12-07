install.packages("readxl")
library(readxl)
protein_data <- read_excel("Initial_Dataset_Protein.xlsx")
getwd()
protein_data <- read_excel("Initial_Dataset_Protein.xlsx")
head(protein_data)
View(protein_data)


library(readxl)
library(ggplot2)

protein_data <- read_excel("Initial_Dataset_Protein.xlsx")
ggplot(protein_data, aes(x = Protein_Concentration, y = Absorbance)) +
  geom_point(color = "steelblue", size = 3, alpha = 0.7) +
  geom_smooth(method = "lm", color = "darkred", se = TRUE) +
  labs(
    title = "Protein Concentration vs Absorbance",
    x = "Protein Concentration (µg/mL)",
    y = "Absorbance (a.u.)"
  ) +
  theme_minimal()



library(ggplot2)

# Create fake data
set.seed(42)
fake_data <- data.frame(
  Protein_Concentration = seq(0, 200, by = 10),
  Absorbance = seq(0, 200, by = 10) * 0.004 + rnorm(21, 0, 0.02)
)

# Plot
ggplot(fake_data, aes(x = Protein_Concentration, y = Absorbance)) +
  geom_point(color = "darkblue", size = 3) +
  geom_smooth(method = "lm", color = "red", se = FALSE) +
  labs(
    title = "Simulated Protein Standard Curve",
    x = "Protein Concentration (µg/mL)",
    y = "Absorbance (a.u.)"
  ) +
  theme_minimal()
