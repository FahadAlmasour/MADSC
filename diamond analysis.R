# Diamond Analysis
# Rick Scavetta
# 29.09.2020
# A small case study for EDA and stats

# load packages
library(tidyverse)
library(rio)

# Read in the data (csv format):
# Newer methods from tidyr package
jems <- read_csv("data/diamonds.csv")

# super convenient way
# library(rio) # R i/o
# jems2 <- import("data/diamonds.csv")

# Get familiar with our data:
summary(jems)
names(jems)
glimpse(jems)

# more detail:
attributes(jems)
typeof(jems)


# test
test <- jems %>%
  group_by(cut) %>%
  group_split()

test
# Group Exercise
# http://scavetta.academy/misk/Misk_DSI_R/_book/case-study-diamonds.html#exercises-for-plotting-transforming-and-eda

jems %>%
  filter(clarity == "VVS2" & cut == "Good")


# How many diamonds with a clarity of category “IF” are present in the data-set?
# What fraction of the total do they represent?
jems %>%
  filter(clarity == "IF") %>%
  nrow()
# another way
sum(jems$clarity == "IF")


jems %>%
  filter(clarity == "IF") %>%
  summarise("fraction_of_total %" = length(clarity) / length(jems$clarity) * 100)
# better way
nrow(clarity) / nrow(jems)


# What is the cheapest diamond price overall?
# What is the range of diamond prices?
# What is the average diamond price in each category of cut and color?

jems %>%
  filter(price == min(price))
# min
min(jems$price)
# another way
jems %>%
  filter(price == min(price))
# range
jems %>%
  filter(price == range(jems$price))
# another way
range(jems$price)



# What is the average diamond price in each category of cut and color?

jems %>%
  group_by(cut, color) %>%
  summarise(avg = mean(price))

# What proportion of the whole is made up of each category of clarity?
jems %>%
  group_by(clarity) %>%
  summarise(propotion = length(clarity) / length(jems$clarity) * 100)
# another way
jems %>%
  group_by(clarity) %>%
  count() %>%
  mutate(prop = n / nrow(jems))


# Make a scatter plot that shows the price of a diamond as described
# by another continous variable, like the carat.
ggplot(jems, aes(x = carat, y = price)) +
  geom_point()


lm(jems$carat ~ jems$price, jems)

price_log10 <- log10(jems$price)
carat_log10 <- log10(jems$carat)
#
#
# ggplot(jems, aes(x = carat_log10 , y = price_log10)) +
#   geom_point() +
#   geom_smooth(lm(price_log10 ~ carat_log10, jems))


# # Worked !
# ggplot(jems, aes(x = carat_log10 , y = price_log10)) +
#   geom_point() +
#   stat_smooth(method = "lm", col = "red")

# first Mutate the data and add two columns to it
jems %>%
  mutate(price_log10 = log10(jems$price),
         carat_log10 = log10(jems$carat))

# Produce our model first
jems_lm <- lm(price_log10 ~ carat_log10, data = jems)

# Getting the Coefficients :
Coefficients <- jems_lm$coefficients
Coefficients

ggplot(jems, aes(x = carat_log10, y = price_log10)) +
  geom_point() +
  geom_smooth(method = "lm", col = "red")


# Saving as a csv file
jems  %>%
write.csv(jems, "jems.csv")



#indexing exercise :

