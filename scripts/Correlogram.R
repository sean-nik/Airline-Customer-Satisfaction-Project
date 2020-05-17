## Correlogram
install.packages("ggcorrplot")
library(ggplot2)
library(ggcorrplot)
library(dplyr)

# pull all numeric columns from df
dfNums <- dplyr::select_if(df, is.numeric)

# Correlation matrix
corr <- round(cor(dfNums), 1)

# plot the correlogram and save to path
ggcorrplot(corr, 
           type = "lower", 
           lab = TRUE, 
           lab_size = 3, 
           method="circle", 
           colors = c("tomato2", "white", "springgreen3"), 
           title="Correlogram of mtcars") +
  ggsave("correlogram.png",
         plot = last_plot(),
         path = "./images")
