# run install.packages("readxl") if not already installed
# replaced the install.packages("readxl") with require
require("readxl")
library("readxl")
raw_data <- read_excel("./data/SatisfactionSurvey.xlsx")

