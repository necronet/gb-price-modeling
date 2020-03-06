# Back script for assiting on preliminary report variables
## @knitr raw_data
source("init.R")
raw_data <- load_raw_vehicle_data()

## @knitr vehicle_data
vehicle_data <- load_vehicle_data()

## @knitr missingness_plot
library(visdat)
missingness_plot <- vis_miss(raw_data, cluster = TRUE)

## @knitr plot_color_percentage
library(forcats)
library(ggplot2)
plot_color_percentage <- load_vehicle_data() %>% group_by(Color) %>% summarise(n = n()) %>% mutate(freq = (n/sum(n)) ) %>% 
  arrange(desc(freq)) %>% mutate(cumsum = cumsum(freq)) %>% filter(cumsum < .99) %>%
  mutate(Color = fct_reorder(Color, n), count =n) %>% 
  ggplot(aes(x = Color, y=freq)) + geom_col() + coord_flip() + scale_y_continuous(labels=scales::percent) +
  ggtitle("Vehicle percentage per color") + labs(x = "Percentage")

## @knitr plot_density_age
plot_density_age <- load_vehicle_data() %>% mutate(Age = Age^1) %>% ggplot(aes(x = Age)) + geom_density(color = "black", fill = "gray") + 
  geom_vline(aes(xintercept = mean(Age)), color = "red", linetype = "dashed", size = .5) +
  geom_vline(aes(xintercept = median(Age)), color = "blue", linetype = 4, size = .5) +
  ggtitle("Age density distribution")

## @knitr plot_drive_train
plot_drive_train <- load_vehicle_data() %>% group_by(DriveTrain) %>% summarise(n = n()) %>% mutate(freq = (n/sum(n)) ) %>% 
  arrange(desc(freq)) %>% mutate(cumsum = cumsum(freq)) %>% mutate(Color = fct_reorder(DriveTrain, -n), count =n) %>% 
  ggplot(aes(x = Color, y=freq)) + geom_col() + scale_y_continuous(labels=scales::percent) +
  ggtitle("Vehicle percentage Drive Train")  + labs(y = "Percentage")

## @knitr plot_vehicle_model
plot_vehicle_model <- load_vehicle_data() %>% group_by(Model) %>% summarise(n = n()) %>% mutate(freq = (n/sum(n)) ) %>% 
  arrange(desc(freq)) %>% mutate(cumsum = cumsum(freq)) %>%
  mutate(Model = fct_reorder(Model, n), count =n) %>% 
  ggplot(aes(x = Model, y=freq)) + geom_col() + coord_flip() + scale_y_continuous(labels=scales::percent) +
  ggtitle("Vehicle percentage Model")  + labs(x = "Percentage")


# @knitr count_certified
count_certified<-load_vehicle_data() %>% count(Certified)
# @knitr count_CARFAX1Owner
count_CARFAX1Owner<-load_vehicle_data() %>% count(CARFAX1Owner)
# @knitr count_CARFAX1OwnerReportOnline
count_CARFAX1OwnerReportOnline<-load_vehicle_data() %>% count(CARFAX1OwnerReportOnline)
# @knitr count_CARFAXCleanTitle
count_CARFAXCleanTitle<-load_vehicle_data() %>% count(CARFAXCleanTitle)
# @knitr plot_geo_observation
library(ggthemes)
us_states <- map_data("state")
plot_geo_observation <- load_vehicle_data()  %>% 
  group_by(SellerState) %>% summarise(count = n()) %>%
  mutate(region = tolower(state.name[match(SellerState, state.abb)]), freq = count/sum(count)) %>% 
  filter(!is.na(region)) %>%
  right_join(us_states) %>% ggplot(mapping = aes(x = long, y =lat, group=group, fill=freq)) + 
  geom_polygon(color="gray90", size = 0.2) + coord_map(projection="albers", lat0=39, lat1=45) + 
  labs(fill="Percent") + theme_map() 





