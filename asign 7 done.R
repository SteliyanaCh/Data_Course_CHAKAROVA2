library(tidyverse)

relig <- read_csv("Utah_Religions_by_County.csv")

relig_tidy <- relig %>%
  pivot_longer(
    cols = -County,
    names_to = "Religion",
    values_to = "Percent"
  ) %>%
  mutate(Percent = as.numeric(Percent))

ggplot(relig_tidy, aes(x = Percent)) +
  geom_histogram(bins = 20) +
  facet_wrap(~ Religion, scales = "free_y")

ggplot(relig_tidy, aes(y = Percent, x = Religion)) +
  geom_boxplot() +
  coord_flip()

if("Population" %in% names(relig)){
  
  relig_pop_tidy <- relig %>%
    pivot_longer(
      cols = -c(County, Population),
      names_to = "Religion",
      values_to = "Percent"
    ) %>%
    mutate(Percent = as.numeric(Percent))
  
  pop_corr <- relig_pop_tidy %>%
    group_by(Religion) %>%
    summarise(
      cor_with_population = cor(Population, Percent, use = "complete.obs")
    )
  
  print(pop_corr)
  
  ggplot(relig_pop_tidy, aes(x = Population, y = Percent)) +
    geom_point() +
    facet_wrap(~ Religion, scales = "free_y") +
    geom_smooth(method = "lm")
}

possible_nonrelig <- relig_tidy %>%
  filter(str_detect(Religion, regex("non|una", ignore_case = TRUE))) %>%
  pull(Religion) %>%
  unique()

print(possible_nonrelig)

nonrelig_col <- possible_nonrelig[1]

nonrelig_df <- relig %>%
  select(County, all_of(nonrelig_col)) %>%
  rename(NonReligious = all_of(nonrelig_col))

relig_vs_non <- relig_tidy %>%
  filter(Religion != nonrelig_col) %>%
  left_join(nonrelig_df, by = "County")

cor_non <- relig_vs_non %>%
  group_by(Religion) %>%
  summarise(
    cor_with_nonrelig = cor(Percent, NonReligious, use = "complete.obs")
  )

print(cor_non)

ggplot(relig_vs_non, aes(x = Percent, y = NonReligious)) +
  geom_point() +
  geom_smooth(method = "lm") +
  facet_wrap(~ Religion, scales = "free")

