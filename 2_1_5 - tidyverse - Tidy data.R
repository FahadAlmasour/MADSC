# 2.1.5 tidyverse - Tidy data
# Misk Academy Data Science Immersive, 2020


library(tidyverse)
# Get a play data set:
PlayData <- read_tsv("data/PlayData.txt")
PlayData %>%
  mutate(ratio = height / width)

# Let's see the scenarios we discussed in class:
# Scenario 1: Transformation across height & width


# For the other scenarios, tidy data would be a
# better starting point:
# Specify the ID variables (i.e. Exclude them)
PlayData %>%
  pivot_longer(-c(type, time),
               names_to = "key",
               values_to = "value")

# Now try the same thing but specify the MEASURE variables (i.e. Include them)
PlayData %>%
  pivot_longer(c(height, width),
               names_to = "key",
               values_to = "value")

PlayData_t <- PlayData %>%
              pivot_longer(c(height, width),
                           names_to = "key",
                           values_to = "value")

PlayData_t

# Scenario 2: Transformation across time 1 & 2 (group by type & key)
# Difference from time 1 to time 2 for each type and key (time2 - time1)
# we only want one value as output
PlayData_t %>%
  group_by(type, key) %>%
  summarise(timediff = value[time == 2] - value[time == 1])

PlayData_t

# As another example: standardize to time 1 i.e time2/time1
PlayData_t %>%
  group_by(type, key) %>%
  summarise(change = value[time == 2] / value[time == 1])

PlayData_t


# Keep all values




# Scenario 3: Transformation across type A & B
# Ratio of A/B for each time and key
PlayData_t %>%
  group_by(time, key) %>%
  summarise(key_ratio = value[type == "A"] / value[type == "B"])

PlayData_t

# Aggregate mean() width & height
PlayData_t %>%
  group_by(time, type) %>%
  summarise(avg = mean(value))

# Aggregate mean() time 1 & time 2
PlayData_t %>%
  group_by(type, key) %>%
  summarise(avg = mean(value))

# Aggregate mean() type A & type B :
PlayData_t %>%
  group_by(time, key) %>%
  summarise(avg = mean(value))

