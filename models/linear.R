source("init.R")
library(recipes)
library(dplyr)
library(tidyverse)
library(ggplot2)
library(caret)
#library(vip)
library(MASS)
library(ggpubr)
library(broom)

vehicle_data <- load_vehicle_data() %>% filter(!is.na(vRank))
ggqqplot(vehicle_data$Price^-.5)
glimpse(vehicle_data)
# vehicle_data %>% ggplot(mapping = aes(x = Odometer, y = Price^.5)) + geom_point()

linear_formula <- Price^-.5 ~ Odometer + Distance + Age

# Test linear regression models into sample data
simple_linear_model <- lm(Price ~ Odometer + vRank,vehicle_data)
plot(simple_linear_model)

simple_robust_linear_transformed_model <- rlm(linear_formula,vehicle_data)
plot(simple_robust_linear_transformed_model)

simple_linear_model_transformed <- lm(linear_formula, vehicle_data)
wts <- 1/fitted( lm(abs(residuals(simple_linear_model_transformed))~fitted(simple_linear_model_transformed)) )^2
wls_model <- lm(linear_formula, vehicle_data, weights=wts)
plot(wls_model)


summary(simple_linear_model)
summary(simple_linear_model_transformed)
summary(simple_robust_linear_transformed_model)
summary(wls_model)

predicted_df <- data.frame(price_pred = predict(simple_linear_model_transformed, vehicle_data), odometer=vehicle_data$Odometer)
ggplot(data = vehicle_data, aes(x = Odometer, y = Price^-.5)) + 
  geom_point(color='gray50') +
  geom_line(color='blue',data = predicted_df, aes(x=odometer, y=price_pred))

vehicle_data %>% count(MakeModel) %>% nrow() - nrow(linear_models_per_group)  

# Simulate how the regression is currently made
vehicle_nested_data <- vehicle_data %>% group_by(MakeModel) %>% filter(n() > 30) %>% nest()

linear_models_per_group <-  vehicle_nested_data %>% 
                mutate(model = map(data, ~lm(linear_formula, data = .x))) %>% 
                mutate(info = map(model, ~tidy(.x))) %>%
                mutate(coef = map(model, ~glance(.x))) %>%
                mutate(augment = map(model, ~augment(.x)))
 
#mutate(linear_model = map( ~ lm(linear_formula, data = .) %>% glance(), .x = data))

           
linear_models_per_group %>% unnest(info) %>% filter(term == "Odometer") %>% ggplot(aes(x = estimate)) + geom_histogram(bins = 50)



linear_models_per_group %>% unnest(coef) %>% filter(!is.nan(adj.r.squared)) %>% 
          ungroup() %>% mutate(MakeModel = fct_reorder(MakeModel, adj.r.squared), 
                               Rate = ifelse(adj.r.squared >= .80, "Good",
                                             ifelse(adj.r.squared >= .55, "Fair","Bad"))) %>%
          ggplot(aes(x = MakeModel, y=adj.r.squared, fill=Rate)) + geom_col() +coord_flip() 


linear_models_per_group %>% unnest(coef) %>% filter(!is.nan(adj.r.squared)) %>% arrange(desc(r.squared)) %>% 
                  head(1) %>% unnest(augment) %>% mutate (PriceOrg = `Price..0.5`^-2) %>%
                  ggplot(aes(x = Odometer, y = PriceOrg)) + geom_point() + geom_line(aes(y= .fitted^-2), color="blue")

# Best vs Worst with linear models
linear_models_per_group %>% unnest(coef) %>% filter(!is.nan(adj.r.squared)) %>% arrange(desc(-r.squared)) %>% dplyr::select(MakeModel, r.squared)
    
vehicle_data %>% filter(MakeModel == "2019 Chrysler Pacifica Touring L") %>% ggplot(mapping=aes(x=Odometer, y=Price)) + geom_point() + ggtitle("2019 Chrysler Pacifica Touring L")

vehicle_data %>% filter(MakeModel == "2020 Toyota Corolla SE") %>% ggplot(mapping=aes(x=Odometer, y=Price)) + geom_point() + ggtitle("2020 Toyota Corolla SE")
vehicle_data %>% filter(MakeModel == "2016 Toyota Corolla LE") %>% ggplot(mapping=aes(x=Odometer, y=Price)) + geom_point() + ggtitle("2020 Toyota Corolla SE")

vehicle_data %>% filter(MakeModel == "2019 Jeep Wrangler Unlimited Sport") %>% ggplot(mapping=aes(x=Odometer, y=Price)) + geom_point() + ggtitle("2019 Jeep Wrangler Unlimited Sport")




# Simulate or lookup for filtering with too few observations