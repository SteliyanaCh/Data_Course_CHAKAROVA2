library(tidyverse)
dat <- read_csv("C:/Users/siska/Desktop/Data_Course_CHAKAROVA2/Data/BioLog_Plate_Data.csv")
head(dat)     
glimpse(dat)

library(tidyverse)

tidy_dat <- dat %>%
  # 1. Pivot longer: all columns that start with "Sample" become rows
  pivot_longer(
    cols = starts_with("Sample"),   
    names_to = "SampleID",          # creates this new column
    values_to = "Measurement"
  ) %>%
  # 2. Separate the SampleID into Plate and Well if needed
  separate(
    SampleID,
    into = c("Plate", "Well"),
    sep = "_",
    fill = "right",
    extra = "merge"
  )
tidy_dat %>% filter(!str_detect(SampleID, "_"))





dat %>%
  filter(!str_detect(SampleID, "_"))
tidy_dat <- dat %>%
  separate(SampleID, into = c("Plate", "Well"), sep = "_", extra = "merge")
separate(SampleID, into = c("Plate", "Well"), sep = "_", fill = "right")
  pivot_longer(cols = starts_with("Sample"),   
               names_to = "SampleID",
               values_to = "Measurement") %>%
  separate(SampleID, into = c("Plate", "Well"), sep = "_")
