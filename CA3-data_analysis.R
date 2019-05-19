# Shubhanshu Sharma
# CA3 Data Analysis
# Dataset - Business Sectors in Ireland

# We are Reading CSV file into R

business_data <- read.csv("CA Data.csv", header = TRUE)
business_data

# to check the data structure
str(business_data)

# change the attribute from Factor to Numeric
# In order to apply the Shapiro test it should not be a factor
business_data$Business.Value.in.K <- as.numeric(business_data$Business.Value.in.K)

# Checking normality of distributed data
shapiro.test(business_data$Business.Value.in.K)

# p value is calculated for a particular sample mean
# The p-value is less than 0.05 
# which proves that the data is not normally distributed

# deploying Q-Q plot to check the normality of data
qqnorm(business_data$Business.Value.in.K)
qqline(business_data$Business.Value.in.K, col = "blue")

# Installing package "pwr" in order to do
# the power analysis

# Installing libraries to apply the respective "pwr" function

install.packages("pwr")
library(pwr)
library(dplyr)

power_testing <- pwr.r.test(n= NULL,
                            r = .5,
                            sig.level = .05,
                            power = .95,
                            alternative = "two.sided")
power_testing
plot(power_testing)
# the result showing that we need 105 samples from each 
# group to inorder to find an effect size of 0.5 with 5% 
# of chance for type-1 error and 5% of type 2 error

cohen.ES(test = "r", size = "large")


# finding the correlation between the 
# business value and the profit made
# since the variables are continuous and 
# non-parametric we are applying Spearman correlation test 
# to find whether any relation exist between two attributes.

relation <- cor.test(business_data$Business.Value.in.K, business_data$Profit.in.K, 
                     method = "spearman", exact = FALSE)
relation

# P-value is greater than 0.05 
# so fails to reject the null hypothesis
# which shows that the business value has no effect on Profit



