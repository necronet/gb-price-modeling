# Script to load vehicle related data

load_vehicle_data <- function() {
  vehicle_data <- read.csv("data/data.csv", sep = ";", 
                           colClasses=c("Interior"="character", "SellerCity"="character", 
                                        "SellerState"="character", 
                                        "SellerAddress"="character"), 
                           header = TRUE) %>% 
                  select(-X_id, -VIN, -Link, -LastExported, -Exported, -Seller, -SellerAddress) %>%
                  mutate(SellerCity = as.factor(toupper(SellerCity)))
}


