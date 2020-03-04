library(dplyr)
# Script to load vehicle related data

load_raw_vehicle_data <- function() {
  read.csv("data/data.csv", sep = ";", 
           colClasses=c("Interior"="character", "SellerCity"="character", 
                        "SellerState"="character", 
                        "SellerAddress"="character"), 
           header = TRUE) 
}

load_vehicle_data <- function() {
  load_raw_vehicle_data() %>% dplyr::select(-X_id, -VIN, -Link, -LastExported, -Exported, -Seller, -SellerAddress) %>%
                  mutate(SellerCity = as.factor(toupper(SellerCity)))
}


