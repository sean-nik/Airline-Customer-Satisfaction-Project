# include dependencies
library(dplyr)
library(ggplot2)
library(imputeTS)
library(tidyr)
library(reshape2)
library(grDevices)
library(readxl)
install.packages("naniar")
library(naniar)
library(visdat)
install.packages("ggcharts")
library(ggcharts)

# read in the data
raw_data <- read_excel("./data/SatisfactionSurvey.xlsx")

# take a look at the missing data
vis_miss(raw_data) # too large to visualize

# take a look at the structure
str(raw_data)

# check for NA's
any(is.na(raw_data))

# check for column names where NA's exist
colnames(raw_data)[apply(raw_data, 2, anyNA)]

# store columns with missing data in a new variable
na_test <- raw_data[c(1, 23, 24, 26)]
str(na_test)

# visualize how much data is missing
vis_miss(na_test)

# find how many elements are missing from each and their relationships
gg_miss_upset(na_test)

# count number of missing elements from each variable
gg_miss_var(na_test)

# discover which ratings the most missing data is from 
gg_miss_var(na_test, facet = Satisfaction)

