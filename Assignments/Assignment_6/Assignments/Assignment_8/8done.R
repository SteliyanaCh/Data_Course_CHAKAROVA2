library(tidyverse)
library(modelr)
library(performance)
library(report)

mushrooms <- read_csv("mushroom_growth.csv")
stopifnot("GrowthRate" %in% colnames(mushrooms))
glimpse(mushrooms)

mushrooms <- mushrooms %>%
  mutate(Humidity = factor(Humidity),
         Temperature = as.numeric(Temperature))

ggplot(mushrooms, aes(x=Light, y=GrowthRate)) + geom_point() + theme_minimal()
ggplot(mushrooms, aes(x=Temperature, y=GrowthRate)) + geom_point() + theme_minimal()
ggplot(mushrooms, aes(x=Humidity, y=GrowthRate)) + geom_point() + theme_minimal()
ggplot(mushrooms, aes(x=Light, y=GrowthRate, color=Humidity)) + geom_point() + theme_minimal()

mod1 <- lm(GrowthRate ~ Light, data=mushrooms)
mod2 <- lm(GrowthRate ~ Temperature, data=mushrooms)
mod3 <- lm(GrowthRate ~ Light + Temperature, data=mushrooms)
mod4 <- lm(GrowthRate ~ Light*Temperature + Humidity, data=mushrooms)

mse1 <- mean(mod1$residuals^2)
mse2 <- mean(mod2$residuals^2)
mse3 <- mean(mod3$residuals^2)
mse4 <- mean(mod4$residuals^2)
print(c(mse1, mse2, mse3, mse4))

best_model <- mod4
stopifnot(inherits(best_model, "lm"))

newdata <- data.frame(
  Light = c(100, 200, 300),
  Temperature = c(20, 25, 30),
  Humidity = factor(c("Low", "Medium", "High"), levels = levels(mushrooms$Humidity))
)

hyp_preds <- predict(best_model, newdata=newdata)
hyp_df <- data.frame(newdata, PredictedGrowth = hyp_preds, Type="Hypothetical")

mushrooms <- mushrooms %>% add_predictions(best_model)
mushrooms$Type <- "Real"

full_preds <- full_join(mushrooms, hyp_df, by=c("Light","Temperature","Humidity","Type"))
stopifnot(all(c("Light","Temperature","Humidity","pred","Type") %in% colnames(full_preds)))

ggplot(full_preds, aes(x=Light, y=pred, color=Type)) +
  geom_point(size=3) +
  geom_point(aes(y=GrowthRate), color="black") +
  theme_minimal()

summary(best_model)
report(best_model)
