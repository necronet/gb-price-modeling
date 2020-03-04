source("init.R")
library(ggplot2)
library(dplyr)
library(forcats)
library(maps)
library(maps)
library(mapproj)
library(ggthemes)
library(MASS)

vehicle_data <- load_vehicle_data()
total_mean_price <- mean(vehicle_data$Price)
total_sd_price <- sd(vehicle_data$Price)

# Unlevel distribution accross color  
vehicle_data %>% group_by(Color) %>% summarise(n = n()) %>% mutate(freq = (n/sum(n)) ) %>% 
    arrange(desc(freq)) %>% mutate(cumsum = cumsum(freq)) %>% filter(cumsum < .99) %>%
    mutate(Color = fct_reorder(Color, n), count =n) %>% 
  ggplot(aes(x = Color, y=freq)) + geom_col() + coord_flip() + scale_y_continuous(labels=scales::percent)


# Long trailed data on Age 
vehicle_data %>% ggplot(aes(x = Age)) + geom_density(color = "black", fill = "gray") + 
    geom_vline(aes(xintercept = mean(Age)), color = "red", linetype = "dashed", size = .5) +
    geom_vline(aes(xintercept = median(Age)), color = "blue", linetype = 4, size = .5)


# Analyis on body boxplot shows distribution on some categories contianing several points
# with a number of observations that contain several points outside the 1.5sd pulling the mean away
# from the median significantly
vehicle_data %>% ggplot(aes(x = Body, y=Price)) + geom_boxplot(fill = "#4271AE", colour = "#1F3552", alpha = 0.7) + 
  stat_summary(fun.y=mean, geom="errorbar", width = .75, linetype = "dashed", aes(ymax = ..y.., ymin = ..y..)) +
  scale_y_continuous(name = "Mean price per body type", breaks = seq(0, 60000, 20000)) + theme_bw()


# A deeper look on the connection between price and miles categorize by Body
# reflect on the linear nature of many of these categories and also shows clearly how 
# some categories are overrepresented(or under) making them more dificiult to infer a straignforward
# linear correlation between price and miles (i.e 4D Sedan, 4D supercrew and 4D sport)
vehicle_data %>% ggplot(aes(x = Odometer, y = Price)) + geom_point() + facet_wrap(~Body)

# Most of the vehicles are not certified (unbalance category)
vehicle_data %>% count(Certified)
# Most of the vehicles have had a previous owner?? 
vehicle_data %>% count(CARFAX1Owner)
# Most of the vehicles do not have a owner report online ??
vehicle_data %>% count(CARFAX1OwnerReportOnline)
# Most of the vehicles have a clean title
vehicle_data %>% count(CARFAXCleanTitle)

# Distance can be viewed as age visually, display what it appear a multimodal distribution
# They also seem to be behaving similar to the overall2 peaks multimodal even when group by Body type
vehicle_data %>% ggplot(aes(x = Distance)) + geom_density(color = "black", fill = "gray") + 
  geom_vline(aes(xintercept = mean(Distance)), color = "red", linetype = "dashed", size = .5) +
  geom_vline(aes(xintercept = median(Distance)), color = "blue", linetype = 4, size = .5) + facet_wrap(~Body)


# DriveTrain data show most data is distributed accross FWD and RWD leaving an unbalance check accross the rest of
# the categories, there are also some unlabeld vehicles that could introduce some noise if not filter out
# or infer their DriveTrain by imputation.
vehicle_data %>% group_by(DriveTrain) %>% summarise(n = n()) %>% mutate(freq = (n/sum(n)) ) %>% 
  arrange(desc(freq)) %>% mutate(cumsum = cumsum(freq)) %>% mutate(Color = fct_reorder(DriveTrain, -n), count =n) %>% 
  ggplot(aes(x = Color, y=freq)) + geom_col() + scale_y_continuous(labels=scales::percent)

# So far engine is the most balanced distributed data among categories, even if it shows a long tail is not as prominent
# as with other categorical data so far and can be handle with some smoothing accross the data 
# It also interesting how balance the data is when it comes to price and engine showing a smooth trend
# dowward without too many bumpy or sudden drops in price. 
engine_grouped_data <- vehicle_data %>% group_by(Engine) %>% summarise(price_mean = mean(Price), n = n()) %>% mutate(freq = (n/sum(n)) ) %>% 
  arrange(desc(freq)) %>% mutate(cumsum = cumsum(freq)) 
  
engine_grouped_data %>% mutate(Engine = fct_reorder(Engine, n)) %>% ggplot(aes(x = Engine, y=freq)) + geom_col() + coord_flip() + scale_y_continuous(labels=scales::percent)
engine_grouped_data %>% mutate(Engine = fct_reorder(Engine, price_mean)) %>% ggplot(aes(x = Engine, y=price_mean)) + geom_col() + coord_flip()

#Interior will require a more complex treatment as it contains information in a text driven manner
head(vehicle_data$Interior)


# There is a significant amount of vehicles that have different text of interior with a broad range of prices
# having then a very flat surface of poit accross te data, but also the most frequent interior type vehicles
# seems to be grouping very close around the mean, being the most frequent interior vehicle Black 
vehicle_data %>% filter(Interior!="") %>% group_by(Interior) %>% 
  summarise(n=n(), mean_price=mean(Price)) %>% 
  arrange(desc(mean_price)) %>% ggplot(aes(x=mean_price, y=n)) + geom_point() + 
  geom_text(aes(label=ifelse(n>500,Interior,'')),hjust=0,vjust=0) +
  geom_vline(aes(xintercept = total_mean_price), color = "red", linetype = "dashed", size = .5) +
  geom_vline(aes(xintercept = total_mean_price-total_sd_price), color = "blue", linetype = "dashed", size = .5) +
  geom_vline(aes(xintercept = total_mean_price+total_sd_price), color = "blue", linetype = "dashed", size = .5)
  

# Similar trend as with body although here most brand have a group of outlier that make a long tail 
# kind of distribution accross different maker with their price. An extra graph shows even a more interesting
# correlation between models and price, as with other grouping there are some model that are naturally
# linear correlated (at least it looks like this graphically) and other that are worthwhile exploring into
# why it's getting plot this way (i.e Doged, and Jeep)
vehicle_data %>% ggplot(aes(x = Make, y=Price)) + geom_boxplot(fill = "#4271AE", colour = "#1F3552", alpha = 0.7) + 
  stat_summary(fun.y=mean, geom="errorbar", width = .75, linetype = "dashed", aes(ymax = ..y.., ymin = ..y..)) +
  scale_y_continuous(name = "Price per Maker", breaks = seq(0, 60000, 20000)) + theme_bw()
vehicle_data %>% ggplot(aes(x = Odometer, y = Price)) + geom_point() + facet_wrap(~Make)

# Make model also appear to be following a similar distribution as Model showing price spread out accross different models
# although with some very interesting vehicles being highly popular withing the range of the mean values
vehicle_data %>% group_by(MakeModel) %>% summarise(count = n(), mean_price = mean(Price)) %>% arrange(desc(count)) %>%  
  ggplot(aes(x = mean_price, y = count)) + geom_point() +
  geom_text(aes(label=ifelse(count>500, as.character(MakeModel),'')),hjust=0,vjust=0, size = 3) +
  geom_vline(aes(xintercept = total_mean_price), color = "red", linetype = "dashed", size = .5) +
  geom_vline(aes(xintercept = total_mean_price-total_sd_price), color = "blue", linetype = "dashed", size = .5) +
  geom_vline(aes(xintercept = total_mean_price+total_sd_price), color = "blue", linetype = "dashed", size = .5)

# Vehicle distribution per model bases follows a smooth distribution on frequency bases most model seems
# to gravitate toward a subset of model Cotrolla, Sentra, Rogue, Malibu, C-Class, Versa, Soul
vehicle_data %>% group_by(Model) %>% summarise(n = n()) %>% mutate(freq = (n/sum(n)) ) %>% 
  arrange(desc(freq)) %>% mutate(cumsum = cumsum(freq)) %>%
  mutate(Model = fct_reorder(Model, n), count =n) %>% 
  ggplot(aes(x = Model, y=freq)) + geom_col() + coord_flip() + scale_y_continuous(labels=scales::percent)

vehicle_data %>% ggplot() + geom_point(aes(x = Rank, y = Price)) + facet_wrap(~Body)

# Rank does seem to have a relationship linearly dependent with odometer when look at a separation between
# Model or Body with Rank this linear relationship gets stronger for some model and loose in others 
vehicle_data %>% filter(!is.na(Rank)) %>% ggplot(aes(x = Rank, y = Odometer)) + geom_point() + facet_wrap(~Body)

# A closer look reviel that only some models do have a density function asymetric and standard with some distribution
# others seem flat that may be the lack of data from these type of model that could be autogenerated by a model later on
vehicle_data %>% filter(!is.na(Rank)) %>% ggplot(aes(x = Rank, fill=Body, alpha=.5)) + geom_density() + facet_wrap(~Make)

# Vrank is observing to have a very odd trends accross different Makers focusing on some areas of the plot 
# although it is not odometer we are concern to predict but price so it should be find to avoid colinearity
vehicle_data %>% filter(!is.na(vRank)) %>% ggplot(aes(x = vRank, y = Odometer)) + geom_point() + facet_wrap(~Make)
# While when building agaisnt price it seem that vRank is behaving similar to rank
vehicle_data %>% filter(!is.na(vRank)) %>% ggplot(aes(x = vRank, y = Price)) + geom_point() + facet_wrap(~Make)
# Actually there is a very clear relationship between both of these signals that may give trouble when building OLS
# with these parameter with them
vehicle_data %>% filter(!is.na(vRank), !is.na(Rank)) %>% ggplot(aes(x = vRank, y = Rank)) + geom_point() + facet_wrap(~Make)


# Address and location of seller information
glimpse(vehicle_data)

# Most of the business is related toward florida, it is having a lot of impact when it comes
# price setting, the curve that plot the average price of car per city follows a rather clear 
# sinosoudial pattern, with most of the data within the 1 s. deviation from the mean 
vehicle_data %>% filter(SellerState=="FL") %>% group_by(SellerState, SellerCity) %>% 
    summarise(price_mean=mean(Price)) %>% 
    ggplot(mapping=aes(x = price_mean, y = reorder(SellerCity, price_mean))) +
    geom_vline(xintercept = total_mean_price, color = "red", linetype="dashed") + 
    geom_vline(xintercept = total_mean_price+total_sd_price, color = "blue", linetype="dashed") + 
    geom_vline(xintercept = total_mean_price-total_sd_price, color = "blue", linetype="dashed") + 
    geom_point(size = 2) 


glimpse(vehicle_data)
us_states <- map_data("state")

vehicle_data_with_geolocation <- vehicle_data %>% 
          group_by(SellerState) %>% summarise(count = n()) %>%
          mutate(region = tolower(state.name[match(SellerState, state.abb)]), freq = count/sum(count)) %>% 
          filter(!is.na(region)) %>%
          right_join(us_states)

vehicle_data_with_geolocation %>% ggplot(mapping = aes(x = long, y =lat, group=group, fill=freq)) + 
    geom_polygon(color="gray90", size = 0.2) + coord_map(projection="albers", lat0=39, lat1=45) + 
    labs(fill="Percent") + theme_map() 


# Odometer distribution

vehicle_data %>% ggplot(mapping = aes(x = Odometer)) + geom_density()

vehicle_data %>% mutate(Price = log(Price)) %>% ggplot(mapping = aes(x = Price)) + geom_density()




