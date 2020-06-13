# Load libraries
# specify the packages of interest
packages = c("dplyr",
             "ggplot2",
             "imputeTS",
             "tidyr",
             "reshape2",
             "grDevices",
             "readxl",
             "naniar",
             "visdat",
             "ggcharts")

# use this function to check if each package is on the local machine
# if a package is installed, it will be loaded
# if any are not, the missing package(s) will be installed and loaded
package.check <- lapply(packages, FUN = function(x) {
  if (!require(x, character.only = TRUE)) {
    install.packages(x, dependencies = TRUE)
    library(x, character.only = TRUE)
  }
})

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
# seems to work more consistently with its default theme
vis_miss(na_test)

# find how many elements are missing from each and their relationships
# does not work with theme_dark() or labs()
gg_miss_upset(na_test)

# count number of missing elements from each variable
gg_miss_var(na_test) + 
  theme_dark() +
  theme(plot.title = element_text(hjust = 0.5)) +
  labs(title = "Number of NA's per Variable")

# discover which ratings the most missing data is from 
gg_miss_var(na_test, facet = Satisfaction) + 
  theme_dark() +
  theme(plot.title = element_text(hjust = 0.5),
        plot.subtitle = element_text(hjust = 0.5)) +
  labs(title = "Number of NA's per Variable",
       subtitle = "Grouped by Satisfaction Score")

# replace NA's with column means to preserve the rest of the data
df <- data.frame(na_mean(raw_data, option = "median"))

# verify no NA's remain
any(is.na(df))

