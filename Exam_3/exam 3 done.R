library(tidyverse)
library(broom)

faculty <- read_csv("FacultySalaries_1995.csv")

faculty_tidy <- faculty %>%
  filter(Tier != 0) %>%
  select(State, Tier, AvgFullProfSalary, AvgAssocProfSalary, AvgAssistProfSalary) %>%
  pivot_longer(
    cols = starts_with("Avg"),
    names_to = "Rank",
    values_to = "Salary"
  ) %>%
  mutate(
    Rank = case_when(
      Rank == "AvgAssistProfSalary" ~ "Assistant",
      Rank == "AvgAssocProfSalary" ~ "Associate",
      Rank == "AvgFullProfSalary" ~ "Full"
    ),
    Rank = factor(Rank, levels = c("Assistant", "Associate", "Full"))
  )

anova_model <- aov(Salary ~ State + Rank, data = faculty_tidy)
summary(anova_model)

juniper <- read_csv("Juniper_Oils.csv")

chemicals <- c("alpha-pinene","para-cymene","alpha-terpineol","cedr-9-ene",
               "alpha-cedrene","beta-cedrene","cis-thujopsene","alpha-himachalene",
               "beta-chamigrene","cuparene","compound 1","alpha-chamigrene",
               "widdrol","cedrol","beta-acorenol","alpha-acorenol",
               "gamma-eudesmol","beta-eudesmol","alpha-eudesmol",
               "cedr-8-en-13-ol","cedr-8-en-15-ol","compound 2","thujopsenal")

juniper_tidy <- juniper %>%
  pivot_longer(cols = all_of(chemicals),
               names_to = "ChemicalID",
               values_to = "Concentration")

ggplot(juniper_tidy, aes(x = YearsSinceBurn, y = Concentration)) +
  geom_point() +
  facet_wrap(~ChemicalID, scales = "free_y")

glm_results <- juniper_tidy %>%
  group_by(ChemicalID) %>%
  do(tidy(glm(Concentration ~ YearsSinceBurn, data = .))) %>%
  ungroup() %>%
  filter(term == "YearsSinceBurn", p.value < 0.05)

glm_results



library(tidyverse)
library(broom)

juniper <- read_csv("Juniper_Oils.csv")

chemicals <- c("alpha-pinene","para-cymene","alpha-terpineol","cedr-9-ene",
               "alpha-cedrene","beta-cedrene","cis-thujopsene","alpha-himachalene",
               "beta-chamigrene","cuparene","compound 1","alpha-chamigrene",
               "widdrol","cedrol","beta-acorenol","alpha-acorenol",
               "gamma-eudesmol","beta-eudesmol","alpha-eudesmol",
               "cedr-8-en-13-ol","cedr-8-en-15-ol","compound 2","thujopsenal")

juniper_long <- juniper %>%
  select(YearsSinceBurn, all_of(chemicals)) %>%
  pivot_longer(cols = -YearsSinceBurn, names_to = "ChemicalID", values_to = "Concentration")

ggplot(juniper_long, aes(x = YearsSinceBurn, y = Concentration)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE, color = "red") +
  facet_wrap(~ ChemicalID, scales = "free_y") +
  theme_bw() +
  labs(title = "Juniper Oil Concentrations vs Years Since Burn",
       x = "Years Since Burn",
       y = "Concentration")

glm_results <- juniper_long %>%
  group_by(ChemicalID) %>%
  do(tidy(glm(Concentration ~ YearsSinceBurn, data = .))) %>%
  filter(term == "YearsSinceBurn" & p.value < 0.05) %>%
  select(ChemicalID, estimate, std.error, statistic, p.value) %>%
  arrange(p.value)

glm_results

