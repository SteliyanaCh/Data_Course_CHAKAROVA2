library(tidyverse)
library(pROC)

admit_data <- read_csv("../../Data/GradSchool_Admissions.csv")

admit_data <- admit_data %>%
  mutate(rank = factor(rank, levels = 1:4, labels = c("Top-tier", "Tier2", "Tier3", "Tier4")))

ggplot(admit_data, aes(x = gre)) +
  geom_histogram(binwidth = 10, fill = "skyblue", color = "black") +
  theme_minimal() +
  labs(title = "Distribution of GRE scores")

ggplot(admit_data, aes(x = gpa)) +
  geom_histogram(binwidth = 0.1, fill = "salmon", color = "black") +
  theme_minimal() +
  labs(title = "Distribution of GPA")

ggplot(admit_data, aes(x = rank)) +
  geom_bar(fill = "lightgreen") +
  theme_minimal() +
  labs(title = "Counts of Undergraduate Institution Rank")

admit_data %>%
  group_by(rank) %>%
  summarise(admit_rate = mean(admit)) %>%
  ggplot(aes(x = rank, y = admit_rate)) +
  geom_col(fill = "orange") +
  theme_minimal() +
  labs(title = "Admission Rate by Undergraduate Rank", y = "Admission Rate")

model1 <- glm(admit ~ gre + gpa + rank, data = admit_data, family = binomial)

summary(model1)

admit_data$pred_prob <- predict(model1, type = "response")
admit_data$pred_admit <- ifelse(admit_data$pred_prob > 0.5, 1, 0)

table(Predicted = admit_data$pred_admit, Actual = admit_data$admit)

roc_obj <- roc(admit_data$admit, admit_data$pred_prob)
plot(roc_obj, col = "blue", main = "ROC Curve for Admission Model")
auc(roc_obj)

