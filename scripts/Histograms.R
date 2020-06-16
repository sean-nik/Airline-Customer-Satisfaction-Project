# requires AirlineDataAnalysis.R to be run prior to this
# check customer satisfaction across airline status and travel class
ggplot(df,aes(as.factor(Satisfaction), fill = Airline.Status)) +
  facet_wrap(~df$Class, scales = "free_y") +
  geom_bar() + 
  theme_dark() +
  theme(plot.title = element_text(hjust = 0.5)) +
  labs(title = "Customer Satisfaction Bar Graph")

# check customer satisfaction against frequency of flights with other airlines
ggplot(df,aes(X..of.Flight.with.other.Airlines, fill = as.factor(Satisfaction))) +
  #facet_wrap(~Satisfaction, scales = "free_y") +
  geom_histogram(binwidth = 10) + 
  scale_y_sqrt() +
  theme_dark() +
  theme(plot.title = element_text(hjust = 0.5)) +
  labs(title = "Customer Satisfaction Histogram")

# take a look at the structure
str(df$Satisfaction)

# requires DescriptiveStats.R
printVecInfo(df$X..of.Flight.with.other.Airlines)

# 
colnames(df)
