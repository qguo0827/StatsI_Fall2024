#####################
# load libraries
# set wd
# clear global .envir
#####################

# remove objects
rm(list=ls())
# detach all libraries
detachAllPackages <- function() {
  basic.packages <- c("package:stats", "package:graphics", "package:grDevices", "package:utils", "package:datasets", "package:methods", "package:base")
  package.list <- search()[ifelse(unlist(gregexpr("package:", search()))==1, TRUE, FALSE)]
  package.list <- setdiff(package.list, basic.packages)
  if (length(package.list)>0)  for (package in package.list) detach(package,  character.only=TRUE)
}
detachAllPackages()

# load libraries
pkgTest <- function(pkg){
  new.pkg <- pkg[!(pkg %in% installed.packages()[,  "Package"])]
  if (length(new.pkg)) 
    install.packages(new.pkg,  dependencies = TRUE)
  sapply(pkg,  require,  character.only = TRUE)
}

# here is where you load any necessary packages
# ex: stringr
# lapply(c("stringr"),  pkgTest)

# set wd for current folder
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

# read in data
inc.sub <- read.csv("https://raw.githubusercontent.com/ASDS-TCD/StatsI_Fall2024/main/datasets/incumbents_subset.csv")


#Question1:
# Run a regression where the outcome variable is voteshare and the explanatory variable is difflog
model1 <- lm(voteshare ~ difflog, data=inc.sub)

# Make a scatterplot of the two variables and add the regression line
library(ggplot2)
ggplot(inc.sub, aes(x=difflog, y=voteshare)) +
  geom_point() +
  geom_smooth(method="lm", col="purple") +
  labs(title="Scatterplot of Vote Share vs Campaign Spending Difference", x="Difference in Campaign Spending", y="Vote Share")

# Save the residuals of the model in a separate object
residuals1 <- residuals(model1)

# Write the prediction equation
coefficients1 <- coef(model1)
prediction_equation1 <- paste("voteshare = ",
                              round(coefficients1[1], 2), " + ",
                              round(coefficients1[2], 2), " * difflog")
print(prediction_equation1)


#Question2:
# Run the regression
model2 <- lm(presvote ~ difflog, data = inc.sub)

# Create a scatterplot and add the regression line
ggplot(inc.sub, aes(x = difflog, y = presvote)) +
  geom_point() +  # Add scatterplot
  geom_smooth(method = "lm", color = "green") +  # Add regression line
  labs(title = "Scatterplot of Presidential Vote Share vs Campaign Spending Difference",
       x = "Difference in Campaign Spending ",
       y = "Presidential Vote Share")

# Save the residuals of the model in a separate object
residuals2 <- residuals(model2)

# Write the prediction equation
coefficients2 <- coef(model2)
prediction_equation2 <- paste("presvote = ",
                             round(coefficients[1], 2), " + ",
                             round(coefficients[2], 2), " * difflog")
print(prediction_equation2)


#Question3:
# Run the regression
model3 <- lm(voteshare ~ presvote, data = inc.sub)

# Create a scatterplot and add the regression line
ggplot(inc.sub, aes(x = presvote, y = voteshare)) +
  geom_point() +  # Add scatterplot
  geom_smooth(method = "lm", color = "red") +  # Add regression line
  labs(title = "Scatterplot of Vote Share vs Presidential Vote Share",
       x = "Presidential Vote Share",
       y = "Vote Share")

# Write the prediction equation
coefficients3 <- coef(model3)
prediction_equation3 <- paste("voteshare = ",
                              round(coefficients3[1], 2), " + ",
                              round(coefficients3[2], 2), " * presvote")
print(prediction_equation3)


#Question4：
# Run the regression
model4 <- lm(residuals1 ~ residuals2)

# Create a scatterplot and add the regression line
ggplot(data.frame(residuals1, residuals2), aes(x = residuals2, y = residuals1)) +
  geom_point() +  # Add scatterplot
  geom_smooth(method = "lm", color = "yellow") +  # Add regression line
  labs(title = "Scatterplot of Residuals from Question 1 vs Residuals from Question 2",
       x = "Residuals from Question 2",
       y = "Residuals from Question 1")

# Write the prediction equation
coefficients4 <- coef(model4)
prediction_equation4 <- paste("residuals1 = ",
                              round(coefficients4[1], 2), " + ",
                              round(coefficients4[2], 2), " * residuals2")
print(prediction_equation4)


#Question5:
  # Run the regression
  model5 <- lm(voteshare ~ difflog + presvote, data = inc.sub)

# Write the prediction equation
coefficients5 <- coef(model5)
prediction_equation5 <- paste("voteshare = ",
                              round(coefficients5[1], 2), " + ",
                              round(coefficients5[2], 2), " * difflog + ",
                              round(coefficients5[3], 2), " * presvote")
print(prediction_equation5)
