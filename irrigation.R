# oct 1, 2020

library(tidyverse)
# Get a play data set:
irrigation <- read_csv("data/irrigation_wide.csv")
irrigation %>%
  mutate(ratio = height/width)
ls()
summary()
