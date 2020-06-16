# custom function to list descriptive stats for a variable
library("moments")
printVecInfo <- function(variable){
  cat("printVecInfo results:","\n")
  cat("mean:",mean(variable),"\n")
  cat("median:",median(variable),"\n")
  cat("min:",min(variable),"\n")
  cat("max:",max(variable),"\n")
  cat("standard deviation:",sd(variable),"\n")
  quant <- quantile(variable,c(0.05,.95))
  cat("5th to 95th percentile:",quant[1],"--",quant[2],"\n")
  cat("skewness:",skewness(variable),"\n")
}