# run install.packages("readxl") if not already installed
library("readxl")
library("ggplot2")
raw_data <- read_excel("./data/SatisfactionSurvey.xlsx")

meanFunc <- function(vector) {
  return(mean(vector))
}

sdFunc <- function(vector) {
  return(sd(vector))
}

