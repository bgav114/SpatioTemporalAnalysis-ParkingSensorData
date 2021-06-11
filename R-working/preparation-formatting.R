library(magrittr)
library(dplyr)

# Importations

jan_with_avg <- read.csv("~/Desktop/Applied Research Project/Parking Sensor Data/subsetted datasets/Jan_With_Avg.csv")
january <- read.csv("~/Desktop/Applied Research Project/Parking Sensor Data/subsetted datasets/January.csv")
parking_status <- read.csv("~/Desktop/Applied Research Project/Parking Sensor Data/parking status.csv")
jan <- january[!january$DurationMinutes < 0,!1]

# Date Formatting and Data Transformations

jan$ArrivalTime <- as.Date(jan$ArrivalTime, format="%d/%m/%y")
jan$DepartureTime <- as.Date(jan$DepartureTime, format="%d/%m/%y")





date_vector <- jan$DepartureTime #vector of all departure dates
jan$departure_days <- weekdays(date_vector) #converting vector into weekdays as a new column in jan
jan$departure_days_cat <- factor(jan$departure_days, 
                                 levels=c("Monday","Tuesday","Wednesday","Thursday","Friday","Saturday","Sunday"),
                                 labels=c("1","2","3","4","5","6","7")) #factoring the days (monday=1..sunday=7)



#jan adjustments

jan <- jan %>% select(-c(1)) #Drop column "x" which im assuming is ID column from subsetting original data
jan$DeviceId <- factor(jan$DeviceId, levels=c(unique(jan$DeviceId)))
jan$StreetId <- factor(jan$StreetId, levels=c(unique(jan$StreetId)))
jan$StreetName <- factor(jan$StreetName, levels=c(unique(jan$StreetName)))
jan$BayId <- factor(jan$BayId, levels=c(unique(jan$BayId)))





formatted_dataset <- jan %>% select(c(1,2,3,4,9,10,18,22)) 



#bay_locations

bay_locations <- parking_status %>% dplyr::select(c(1,4,5,6))
bay_locations <- bay_locations %>% rename(BayId = bay_id)
bay_locations$BayId <- factor(bay_locations$BayId, levels=c(unique(bay_locations$BayId)))
write.table(bay_locations, 
            file = "~/Desktop/Applied Research Project/Parking Sensor Data/subsetted datasets/bay_locations.csv",
            sep = ",", row.names = F)



#formatted_dataset_locations

formatted_dataset_locations <- merge(formatted_dataset, bay_locations, by="BayId") %>% 
  select(c("StreetId","StreetName","BayId","lat","lon","DurationMinutes","departure_days_cat"))



#avg_duration_street

avg_duration_street <- formatted_dataset_locations %>% group_by(StreetId, departure_days_cat) %>% 
  summarise(avg_duration = mean(DurationMinutes,na.rm=T),
            std_duration = sd(DurationMinutes,na.rm=T),
            avg_lon = mean(lon, na.rm=T),
            avg_lat = mean(lat, na.rm=T))
write.table(avg_duration_street, 
            file = "~/Desktop/Applied Research Project/Parking Sensor Data/subsetted datasets/avg_duration_street.csv",
            sep = ",", row.names = F)
avg_duration_street$StreetId <- factor(avg_duration_street$StreetId, 
                                       levels=c(unique(avg_duration_street$StreetId)))
avg_duration_street$departure_days_cat <- factor(avg_duration_street$departure_days_cat,
                                                 levels=c(unique(avg_duration_street$departure_days_cat)))
avg_duration_street <- avg_duration_street[!avg_duration_street$avg_duration < 0,]



#avg_duration_days

avg_duration_days <- avg_duration_street %>% group_by(departure_days_cat) %>% 
  summarise(avg_duration = mean(avg_duration, na.rm=T),
            std_duration = mean(std_duration, na.rm=T),
            n=n())
write.table(avg_duration_days, 
            file = "~/Desktop/Applied Research Project/Parking Sensor Data/subsetted datasets/avg_duration_days.csv",
            sep = ",", row.names = F)



#avg_duration_street_nodays

avg_duration_street_nodays <- avg_duration_street %>% group_by(StreetId) %>% 
  summarise(avg_duration = mean(avg_duration, na.rm=T),
            std_duration = mean(std_duration, na.rm=T),
            avg_lat=mean(avg_lat,na.rm=T),
            avg_lon=mean(avg_lon,na.rm=T))
write.table(avg_duration_street_nodays, 
            file = "~/Desktop/Applied Research Project/Parking Sensor Data/subsetted datasets/avg_duration_street_nodays.csv",
            sep = ",", row.names = F)



# bay_durations_locations

bay_avg_durations <- jan %>% select(c(4,18)) %>% group_by(BayId) %>% 
  summarise(avg_duration = mean(DurationMinutes, na.rm=T),
            std_duration = sd(DurationMinutes, na.rm=T))
bay_durations_locations <- merge(bay_avg_durations, bay_locations, by="BayId") %>% select(-c(4))
write.table(bay_durations_locations, 
            file = "~/Desktop/Applied Research Project/Parking Sensor Data/subsetted datasets/bay_durations_locations.csv",
            sep = ",", row.names = F)



# jan_with_coords

jan <- january[!january$DurationMinutes < 0,] 
jan <- jan %>% dplyr::select(-c(1))

jan$DeviceId <- factor(jan$DeviceId,levels=c(unique(jan$DeviceId)))
jan$DurationMinutes <- jan$DurationMinutes %>% as.numeric()
jan$BayId <- factor(jan$BayId, levels=c(unique(jan$BayId)))

jan$ArrivalTime <- strptime(jan$ArrivalTime,format="%d/%m/%Y %I:%M:%S %p")
jan$DepartureTime <- strptime(jan$DepartureTime,format="%d/%m/%Y %I:%M:%S %p")

jan_with_coords <- merge(jan, bay_locations, by="BayId")
jan_with_coords <- jan_with_coords %>% dplyr::select(1,4,5,22,23)



# sub

sub <- jan_with_coords[jan_with_coords$lat>=-37.815 & jan_with_coords$lat<=-37.80,]
sub <- sub[sub$lon>=144.94 & sub$lon<=144.96,]
sub <- sub[sub$DepartureTime>=strptime("01/01/2019 12:00:00 am",format="%d/%m/%Y %I:%M:%S %p") &
             sub$DepartureTime<=strptime("07/01/2019 11:59:00 pm",format="%d/%m/%Y %I:%M:%S %p") ,]
nrow(sub)
write.table(sub, 
            file = "~/Desktop/Applied Research Project/Parking Sensor Data/subsetted datasets/sub.csv",
            sep = ",", row.names = F)

# ROUGH

length(unique(jan$BayId))

min(january$DurationMinutes)

length(unique(avg_duration_street$StreetId))

min(avg_duration_street_nodays$avg_duration)
max(avg_duration_street_nodays$avg_duration)

#Descriptive Statistics and Understanding

table(day_of_week)
length(unique(jan$BayId))
length(unique(jan$StreetId))
length(unique(jan$StreetName))

