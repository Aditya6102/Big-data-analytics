# Create a sample dataset
car_data <- data.frame(
  mileage = c(25, 30, 35, 28, 32, 22, 27, 31, 29, 33),
  make = c("Toyota", "Honda", "Ford", "Chevy", "BMW", "Audi", "Hyundai", "Kia", "Nissan", "Mazda"),
  model = c("Corolla", "Civic", "Focus", "Malibu", "X3", "A4", "Elantra", "Soul", "Altima", "3"),
  fuel = c("Gasoline", "Gasoline", "Diesel", "Gasoline", "Diesel", "Gasoline", "Gasoline", "Diesel", "Gasoline", "Diesel"),
  gear = c(5, 6, 6, 5, 5, 6, 5, 5, 6, 6)
)

# View the dataset
print(car_data)

# Convert fuel to a binary variable
car_data$fuel_binary <- ifelse(car_data$fuel == "Diesel", 1, 0)

# Perform linear regression
linear_model <- lm(mileage ~ fuel_binary + gear, data = car_data)

# Summarize the model
summary(linear_model)

# Load necessary library
if(!require(ggplot2)) install.packages("ggplot2", dependencies = TRUE)
library(ggplot2)

# Plot the data points and regression line
ggplot(car_data, aes(x = gear, y = mileage, color = fuel)) +
  geom_point(size = 3) +
  geom_smooth(method = "lm", aes(group = 1), se = FALSE, color = "black") +
  labs(title = "Linear Regression: Mileage vs Gear and Fuel Type",
       x = "Gear",
       y = "Mileage") +
  theme_minimal()


# Create a sample dataset
car_data <- data.frame(
  mileage = c(25, 30, 35, 28, 32, 22, 27, 31, 29, 33),
  make = c("Toyota", "Honda", "Ford", "Chevy", "BMW", "Audi", "Hyundai", "Kia", "Nissan", "Mazda"),
  model = c("Corolla", "Civic", "Focus", "Malibu", "X3", "A4", "Elantra", "Soul", "Altima", "3"),
  fuel = c("Gasoline", "Gasoline", "Diesel", "Gasoline", "Diesel", "Gasoline", "Gasoline", "Diesel", "Gasoline", "Diesel"),
  gear = c(5, 6, 6, 5, 5, 6, 5, 5, 6, 6),
  offertype = c("Lease", "Purchase", "Lease", "Purchase", "Lease", "Purchase", "Lease", "Purchase", "Lease", "Purchase"),
  price = c(20000, 22000, 24000, 21000, 25000, 23000, 20500, 22500, 23500, 24500),
  horsepower = c(150, 160, 170, 155, 165, 158, 152, 162, 168, 175),
  year = c(2018, 2019, 2020, 2018, 2019, 2020, 2018, 2019, 2020, 2020)
)

# View the dataset
print(car_data)

# Add polynomial terms to the dataset
car_data$horsepower2 <- car_data$horsepower^2
car_data$price2 <- car_data$price^2

# Perform polynomial regression
polynomial_model <- lm(mileage ~ horsepower + horsepower2 + price + price2, data = car_data)

# Summarize the model
summary(polynomial_model)

# Load necessary library
if(!require(scatterplot3d)) install.packages("scatterplot3d", dependencies = TRUE)
library(scatterplot3d)

# Create 3D scatter plot
scatterplot3d(car_data$horsepower, car_data$price, car_data$mileage,
              pch = 19, color = "blue", main = "Polynomial Regression: Mileage vs Horsepower and Price",
              xlab = "Horsepower", ylab = "Price", zlab = "Mileage")

# Add the polynomial regression plane
s3d <- scatterplot3d(car_data$horsepower, car_data$price, car_data$mileage, type = "n")
s3d$plane3d(polynomial_model, draw_polygon = TRUE, draw_lines = TRUE, polygon_args = list(col = "lightblue"))


# Load necessary libraries
library(dplyr)
library(ggplot2)

# Set the number of rows for the dataset
n <- 100

# Create a sample dataset
set.seed(123) # For reproducibility
dataset <- data.frame(
  make = sample(c("Toyota", "Honda", "Ford", "Chevrolet", "BMW"), n, replace = TRUE),
  mileage = runif(n, 10000, 100000), # Random mileage between 10,000 and 100,000
  power = runif(n, 70, 300), # Random power between 70 and 300 HP
  model = sample(c("Sedan", "SUV", "Truck", "Coupe", "Convertible"), n, replace = TRUE),
  fuel = sample(c("Petrol", "Diesel", "Electric", "Hybrid"), n, replace = TRUE),
  gear = sample(c("Manual", "Automatic"), n, replace = TRUE),
  offertype = sample(c("New", "Used", "Certified Pre-Owned"), n, replace = TRUE),
  price = runif(n, 5000, 80000), # Random price between 5,000 and 80,000
  year = sample(2000:2023, n, replace = TRUE), # Random year between 2000 and 2023
  horsepower = runif(n, 60, 400) # Random horsepower between 60 and 400 HP
)

# View the first few rows of the dataset
head(dataset)

# Function to create scatterplots
create_scatterplot <- function(data, x_var, y_var) {
  ggplot(data, aes_string(x = x_var, y = y_var)) +
    geom_point() +
    theme_minimal() +
    labs(title = paste("Scatterplot of", x_var, "vs", y_var),
         x = x_var,
         y = y_var)
}

# Create scatterplots for selected pairs of variables
plot1 <- create_scatterplot(dataset, "mileage", "price")
plot2 <- create_scatterplot(dataset, "power", "price")
plot3 <- create_scatterplot(dataset, "year", "price")
plot4 <- create_scatterplot(dataset, "horsepower", "price")

# Display the plots
print(plot1)
print(plot2)
print(plot3)
print(plot4)


























