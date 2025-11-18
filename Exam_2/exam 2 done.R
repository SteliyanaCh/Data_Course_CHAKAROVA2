library(tidyverse)
library(readr)
library(ggplot2)
library(broom)

unicef <- read_csv("unicef-u5mr.csv")
head(unicef)
glimpse(unicef)

u5mr_tidy <- unicef %>%
  pivot_longer(
    cols = starts_with("U5MR."),
    names_to = "Year",
    names_prefix = "U5MR.",
    values_to = "U5MR"
  ) %>%
  mutate(Year = as.integer(Year))
head(u5mr_tidy)

plot1 <- ggplot(u5mr_tidy, aes(x = Year, y = U5MR, group = CountryName)) +
  geom_line(alpha = 0.5) +
  facet_wrap(~ Continent, scales = "free_y") +
  theme_minimal() +
  labs(title = "Under-5 Mortality Rate (U5MR) by Country",
       x = "Year", y = "U5MR (deaths per 1000 live births)")
plot1
ggsave("Chakarova_Plot_1.png", plot1, width = 12, height = 8)

mean_u5mr <- u5mr_tidy %>%
  group_by(Continent, Year) %>%
  summarise(mean_U5MR = mean(U5MR, na.rm = TRUE), .groups = "drop")

plot2 <- ggplot(mean_u5mr, aes(x = Year, y = mean_U5MR, color = Continent)) +
  geom_line(size = 1.2) +
  theme_minimal() +
  labs(title = "Mean Under-5 Mortality Rate by Continent",
       x = "Year", y = "Mean U5MR")
plot2
ggsave("Chakarova_Plot_2.png", plot2, width = 10, height = 6)

mod1 <- lm(U5MR ~ Year, data = u5mr_tidy)
mod2 <- lm(U5MR ~ Year + Continent, data = u5mr_tidy)
mod3 <- lm(U5MR ~ Year * Continent, data = u5mr_tidy)
summary(mod1)
summary(mod2)
summary(mod3)
AIC(mod1, mod2, mod3)

predictions <- u5mr_tidy %>%
  mutate(
    pred_mod1 = predict(mod1, newdata = .),
    pred_mod2 = predict(mod2, newdata = .),
    pred_mod3 = predict(mod3, newdata = .)
  )

pred_long <- predictions %>%
  pivot_longer(cols = starts_with("pred_mod"), names_to = "Model", values_to = "Predicted") %>%
  mutate(Model = recode(Model,
                        pred_mod1 = "mod1",
                        pred_mod2 = "mod2",
                        pred_mod3 = "mod3"))

plot_preds <- ggplot(pred_long, aes(x = Year, y = Predicted, color = Model, group = CountryName)) +
  geom_line(alpha = 0.5) +
  facet_wrap(~ Continent) +
  theme_minimal() +
  labs(title = "Model Predictions of U5MR by Country",
       x = "Year", y = "Predicted U5MR")
plot_preds
ggsave("Chakarova_Plot_3.png", plot_preds, width = 12, height = 8)

ecuador_2020 <- data.frame(CountryName = "Ecuador", Continent = "Americas", Year = 2020)
ecuador_pred <- predict(mod3, newdata = ecuador_2020)
ecuador_diff <- ecuador_pred - 13
ecuador_pred
ecuador_diff

mod4 <- lm(log(U5MR) ~ Year * Continent, data = u5mr_tidy)
ecuador_pred_mod4 <- exp(predict(mod4, newdata = ecuador_2020))
ecuador_diff_mod4 <- ecuador_pred_mod4 - 13
ecuador_pred_mod4
ecuador_diff_mod4

##Just the checks below to make sure everything looks correct :D
head(unicef)
head(u5mr_tidy)
plot1
plot2
plot_preds
summary(mod1)
summary(mod2)
summary(mod3)
AIC(mod1, mod2, mod3)
head(predictions)
head(pred_long)
ecuador_pred
ecuador_diff
ecuador_pred_mod4
ecuador_diff_mod4
list.files(pattern = "Chakarova_Plot")
