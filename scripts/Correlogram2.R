# Load libraries
# specify the packages of interest
packages = c("dplyr",
             "ggplot2",
             "ggcorrplot")

# use this function to check if each package is on the local machine
# if a package is installed, it will be loaded
# if any are not, the missing package(s) will be installed and loaded
package.check <- lapply(packages, FUN = function(x) {
  if (!require(x, character.only = TRUE)) {
    install.packages(x, dependencies = TRUE)
    library(x, character.only = TRUE)
  }
})

# pull all numeric columns from df
dfChars <- dplyr::select_if(df, is.character())

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