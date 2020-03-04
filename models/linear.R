source("init.R")
library(recipes)
library(MASS)

vehicle_data <- load_vehicle_data()
glimpse(vehicle_data)

# vehicle_data %>% ggplot(mapping = aes(x = Odometer, y = Price^.5)) + geom_point()

# Test linear regression models into sample data
simple_linear_model <- lm(Price ~ Odometer + Age + Distance + vRank, vehicle_data)
plot(simple_linear_model)
boxcox(simple_linear_model)

simple_linear_model_transformed <- lm(Price^-.5 ~ Odometer + Age + Distance + vRank, vehicle_data)
summary(simple_linear_model_transformed)
plot(simple_linear_model_transformed)

# Think of difference aspect to tackle a problem of filtering data 
# (include the categorical data or run linear regression on the fly)

# Simulate or lookup for filtering with too few observations