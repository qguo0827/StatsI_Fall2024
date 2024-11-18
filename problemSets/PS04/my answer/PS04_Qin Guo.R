#Question1:
# Install and load the car package
install.packages("car")
library(car)
# Load the Prestige dataset
data(Prestige)

#a
# Create a new variable professional
# use the ifelse function to create a new variable professional based on the type variable in the Prestige dataset.
Prestige$professional <- ifelse(Prestige$type == "prof", 1, 0)

#b
# Fit the linear model
model <- lm(prestige ~ income + professional + income:professional, data = Prestige)
#fit a linear model with prestige as the outcome and income, professional, and their interaction as predictors.
summary(model)
 
#f
#Given coefficients
income_coefficient <- 0.003
professional_coefficient <- 37.781
interaction_coefficient <- -0.002

# Calculate the effect of a $1,000 increase in income on prestige score for professionals
income_coefficient <- 0.003
interaction_coefficient <- -0.002
income_effect_professional <- (income_coefficient + interaction_coefficient) * 1000
income_effect_professional

#g
# Calculate the effect of changing from non-professional to professional at an income of $6,000
professional_coefficient <- 37.781
income_level <- 6000
professional_effect <- professional_coefficient + (interaction_coefficient * income_level)
professional_effect


#Question2
#a
#calculate t-statistic
coef_signs <- 0.042
se_signs <- 0.016
statistic_signs <- coef_signs / se_signs
#calculate P-value
#df = N - K - 1
df <- 131 - 2 - 1
p_value <- 2* pt(statistic_signs , df , lower.tail = FALSE )
#t=2.625  p=0.0097

#b
#calculate t-statistic
coef_adjacent <- 0.042
se_adjacent <- 0.016
statistic_adjacent <- coef_adjacent / se_adjacent
#calculate P-value
#df = N - K - 1
df <- 131 - 2 - 1
p_value_adjacent <- 2* pt(statistic_adjacent , df , lower.tail = FALSE )
#t=3.231  p=0.00157
