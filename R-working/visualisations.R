library(devtools)
library(ggmap)
library(ggplot2)
library(maptools)
library(dplyr)
library(rgeos)
library(broom)

# Setting up the map------------------------------------------------------------------------------------------

register_google(key="AIzaSyCYIMu3bBtnw0_QVoGGq0XXpTaZ_07DDps")

melbourne_google <- get_googlemap("Melbourne, Australia",zoom=14, maptype = "terrain")
melbourne_stamen <- get_map("Melbourne,Australia", zoom=14, source= "stamen", maptype = "terrain")
melb1 <- ggmap(melbourne_google)
melb2 <- ggmap(melbourne_stamen)
#### melbourne_rough <- get_map("Melbourne,Australia", zoom=14, maptype="satellite)
#### melb3 <- ggmap(melbourne_rough)


# Import the datasets------------------------------------------------------------------------------------------

bay_information <- read.csv("~/Desktop/Applied Research Project/Parking Sensor Data/bay information.csv")
parking_status <- read.csv("~/Desktop/Applied Research Project/Parking Sensor Data/parking status.csv")
parking_specifics <- read.csv("~/Desktop/Applied Research Project/Parking Sensor Data/parking specifics.csv")
property_parkings <- read.csv("~/Desktop/Applied Research Project/Parking Sensor Data/Off-street_car_parking_2019.csv")
avg_duration_street <- read.csv("~/Desktop/Applied Research Project/Parking Sensor Data/subsetted datasets/avg_duration_street.csv")
avg_duration_street_nodays <- read.csv("~/Desktop/Applied Research Project/Parking Sensor Data/subsetted datasets/avg_duration_street_nodays.csv")
bay_durations_locations <- read.csv("~/Desktop/Applied Research Project/Parking Sensor Data/subsetted datasets/bay_durations_locations.csv")
sub <- read.csv("~/Desktop/Applied Research Project/Parking Sensor Data/subsetted datasets/sub.csv")


# Data Formatting

bay_locations <- parking_status %>% select(c(1,4,5,6))

class(bay_locations$bay_id)
class(bay_locations$lat)
class(bay_locations$lon)
class(bay_locations$location)

block_parkings <- property_parkings %>% group_by(Block.ID) %>% summarise(parkings = sum(Parking.spaces),
                                                                         lon = mean(x.coordinate,na.rm=T),
                                                                         lat = mean(y.coordinate,na.rm=T))
block_parkings$parkings <- as.numeric(block_parkings$parkings)

avg_duration_street$StreetId <- factor(avg_duration_street$StreetId, 
                                       levels=c(unique(avg_duration_street$StreetId)))
avg_duration_street$departure_days_cat <- factor(avg_duration_street$departure_days_cat,
                                                 levels=c(unique(avg_duration_street$departure_days_cat)))
avg_duration_street <- avg_duration_street[!avg_duration_street$avg_duration < 0,]

avg_duration_street_nodays$StreetId <- factor(avg_duration_street_nodays$StreetId, levels=c(unique(avg_duration_street_nodays$StreetId)))

sub$BayId <- factor(sub$BayId, levels=c(unique(sub$BayId)))

# Data Visualisations ------------------------------------------------------------------------------------------

melb_parking_points <- melb1 + geom_point(data = bay_locations,
                                          aes(x=lon, y=lat),
                                          size=1, colour = "red", alpha=0.75) + labs(title = "Melbourne Parking Bays",
                                                                                     x= "Longitude", 
                                                                                     y="Latitude")
melb_parking_points #saved as all parking bays





melb_parking_points_bins <- melb2 + stat_bin2d(data = bay_locations,
                                               aes(x=lon, y=lat),
                                               size=1, 
                                               alpha=0.75,
                                               bins=75) + scale_fill_gradientn(colours=c("blue","red"))  + labs(title = "Melbourne Parking Bays",
                                                                                                                x= "Longitude", 
                                                                                                                y="Latitude")
melb_parking_points_bins # saved as all parking bays density





off_street_parkings <- melb2 + geom_point(data = block_parkings,
                                          aes(x=lon, y=lat,color = parkings),
                                          size=2) + scale_color_gradient(low="blue", high="red")+ labs(title = "Melbourne Parking Bays by Block",
                                                                                                       x= "Longitude", 
                                                                                                       y="Latitude")
off_street_parkings # saved as off street parking density





vis_avg_duration_street <- melb2 + geom_point(data = avg_duration_street,
                                              aes(x=avg_lon, y=avg_lat,color = avg_duration),
                                              size=1.5) + scale_color_gradient(low="dodgerblue1", high="grey0")+ facet_wrap(~departure_days_cat,
                                                                                                                          nrow=2) + labs(title = "Melbourne Parking Bays by street and day",
                                                                                                                                         x= "Longitude", 
                                                                                                                                         y="Latitude")
vis_avg_duration_street # saved as street durations by day





vis_density_bay <- melb2 + stat_density2d(data = bay_locations,
                                          aes(x=lon, y=lat, fill=..level..,alpha=..level..),
                                          geom="polygon") + scale_fill_continuous(name="number of bays") + scale_alpha(guide=FALSE) + labs(title = "Density of parking bays",
                                                                                                                                           x= "Longitude", 
                                                                                                                                           y="Latitude")
vis_density_bay # saved as number of bays in street density





vis_duration_day <- ggplot(avg_duration_street, aes(x=departure_days_cat, y=avg_duration)) + geom_boxplot() + labs(title = "Distribution of durationMinutes by day",
                                                                                                                   x= "day", 
                                                                                                                   y="durationMinutes")
vis_duration_day # saved as avg duration by day boxplot





vis_avg_duration_bay <- melb2 + geom_point(data = sub,
                                           aes(x=lon, y=lat, color = DurationMinutes),
                                           size=1.5) + scale_color_gradient(low="blue", high="red")+ labs(title = "Melbourne Parking Bays by street and day",
                                                                                                          x= "Longitude", y="Latitude")
vis_avg_duration_bay # saved as avg duration bay NM





#Exploration and Summary Statistics





#ROUGH

nrow(avg_duration_street_nodays)
min(avg_duration_street$avg_duration)
max()

avg_duration_street %>% filter(avg_duration<0)

melb_parking_points_bins <- melb2 + stat_density2d(data = bay_locations,
                                                   aes(x=lon, y=lat, fill=..level..,alpha=..level..),
                                                   geom="polygon") + scale_fill_continuous(name="number of bays") + scale_alpha(guide=FALSE)
melb_parking_points_bins





