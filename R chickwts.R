library(tidyverse)
chickwts

summary(chickwts)
chickwts %>%
  group_by(feed) %>%
  summarise(n = length(feed),
            average = mean(weight),
            SD = sd(weight))


