library(ggplot2)
# Load data set
insurance <- read.csv(file.choose())

# View starting rows of data set
head(insurance)

# Check the structure of the data set
str(insurance)

# linear regression
# predict charges based on age, BMI, and smoking status
model <- lm(charges ~ age + bmi + smoker, data = insurance)

# Print the summary of model
summary(model)

# Predict values
predictions <- predict(model, insurance)

# Add predictions to the data set
insurance$predictions <- predictions

# Visualization
#Scatter plot: actual vs. predicted charges
ggplot(insurance, aes(x = age, y = charges)) +
  geom_point(color = 'darkgreen', alpha = 0.6) +
  geom_line(aes(y = predictions), color = 'orange') +
  ggtitle('Linear Regression') +
  xlab('Age') +
  ylab('Insurance Charge')

# Residual plot
ggplot(insurance, aes(x = age, y = residuals(model))) +
  geom_point(color = 'brown', alpha = 0.6) +
  geom_hline(yintercept = 0, color = 'blue') +
  ggtitle('Residuals Plot') +
  xlab('Age') +
  ylab('Residuals')