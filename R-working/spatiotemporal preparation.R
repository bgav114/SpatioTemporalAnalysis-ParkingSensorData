library(gstat)
library(sp)
library(spacetime)
library(ggplot2)
library(dplyr)
library(tidyr)
library(raster)
library(rgdal)
library(rgeos)
library(magrittr)
library(xts)
library(reshape2)
library(outliers)
library(devtools)
library(ggmap)
library(maptools)
library(broom)




#Importing ready made datasets----------------------------------------------------------------------------------------------------------




january <- read.csv("~/Desktop/Applied Research Project/Parking Sensor Data/subsetted datasets/January.csv")
bay_locations <- read.csv("~/Desktop/Applied Research Project/Parking Sensor Data/subsetted datasets/bay_locations.csv")

jan <- january[!january$DurationMinutes < 0,] 
jan <- jan %>% dplyr::select(-c(1))

january$BayId <- factor(january$BayId, levels=c(unique(january$BayId)))
jan$DeviceId <- factor(jan$DeviceId,levels=c(unique(jan$DeviceId)))
bay_locations$BayId <- factor(bay_locations$BayId, levels=c(unique(bay_locations$BayId)))

jan$DurationMinutes <- jan$DurationMinutes %>% as.numeric()

jan$ArrivalTime <- strptime(jan$ArrivalTime,format="%d/%m/%Y %I:%M:%S %p")
jan$DepartureTime <- strptime(jan$DepartureTime,format="%d/%m/%Y %I:%M:%S %p")


## jan_grouped_bays

jan_grouped_bays <- jan %>% group_by(BayId, DepartureTime) %>% summarise(duration = mean(DurationMinutes, na.rm=T))
write.table(jan_grouped_bays, 
            file = "~/Desktop/Applied Research Project/Parking Sensor Data/subsetted datasets/jan_grouped_bays.csv",
            sep = ",", row.names = F)


## jan_with_coords

jan_with_coords <- merge(jan_grouped_bays, bay_locations, by="BayId")
jan_with_coords <- jan_with_coords %>% dplyr::select(1,2,3,5,6) #subsetting BayId, durations, departure time, lat, lon
write.table(jan_with_coords, 
            file = "~/Desktop/Applied Research Project/Parking Sensor Data/subsetted datasets/jan_with_coords.csv",
            sep = ",", row.names = F)




# sub, sub_train, sub_test, sub_train_standardised, sub_test_standardised, sub_train_standardised_log, sub_test_standardised_log-----------------------------------------------------




sub <- jan_with_coords[jan_with_coords$lat>=-37.815 & jan_with_coords$lat<=-37.80,]
sub <- sub[sub$lon>=144.94 & sub$lon<=144.96,]
sub <- sub[sub$DepartureTime>=strptime("01/01/2019 12:00:00 am",format="%d/%m/%Y %I:%M:%S %p") &
             sub$DepartureTime<=strptime("07/01/2019 11:59:00 pm",format="%d/%m/%Y %I:%M:%S %p") ,]
nrow(sub)

write.table(sub, 
            file = "~/Desktop/Applied Research Project/Parking Sensor Data/subsetted datasets/sub.csv",
            sep = ",", row.names = F)

sub_full <- sort(sample(nrow(sub), nrow(sub)*0.75))
sub_train <- sub[sub_full, ]
sub_test <- sub[-sub_full, ]
nrow(sub_test)+nrow(sub_train)

write.table(sub_train, 
            file = "~/Desktop/Applied Research Project/Parking Sensor Data/subsetted datasets/sub_train.csv",
            sep = ",", row.names = F)
write.table(sub_test, 
            file = "~/Desktop/Applied Research Project/Parking Sensor Data/subsetted datasets/sub_test.csv",
            sep = ",", row.names = F)

sub_train_standardised <- sub_train
sub_train_standardised$duration <- as.numeric(scale(sub_train_standardised$duration))
sub_test_standardised <- sub_test
sub_test_standardised$duration <- as.numeric(scale(sub_test_standardised$duration))

write.table(sub_train_standardised, 
            file = "~/Desktop/Applied Research Project/Parking Sensor Data/subsetted datasets/sub_train_standardised.csv",
            sep = ",", row.names = F)
write.table(sub_test_standardised, 
            file = "~/Desktop/Applied Research Project/Parking Sensor Data/subsetted datasets/sub_test_standardised.csv",
            sep = ",", row.names = F)

sub_train_standardised_log <- sub_train_standardised
sub_train_standardised_log$duration <- (sub_train_standardised_log$duration+0.5481534)
sub_train_standardised_log$duration <- log(sub_train_standardised_log$duration)

sub_test_standardised_log <- sub_test_standardised
sub_test_standardised_log$duration <- (sub_test_standardised_log$duration+0.55)
sub_test_standardised_log$duration <- log(sub_test_standardised_log$duration)

write.table(sub_train_standardised_log, 
            file = "~/Desktop/Applied Research Project/Parking Sensor Data/subsetted datasets/sub_train_standardised_log.csv",
            sep = ",", row.names = F)
write.table(sub_test_standardised_log, 
            file = "~/Desktop/Applied Research Project/Parking Sensor Data/subsetted datasets/sub_test_standardised_log.csv",
            sep = ",", row.names = F)




# following workflow from: http://r-video-tutorial.blogspot.com/




# sub_STIDF creation---------------------------------------------------------------------------------------------------------------------------




## durations.UTM,    durationsSP, durationsTM, durationsDF,    sub_STIDF creation

x <- 10000 #respecify this sample size number and run the following chunk again for each new variogram trial
durations.UTM <- sample_n(sub_train_standardised, x)
coordinates(durations.UTM) = ~lon+lat
projection(durations.UTM)=CRS("+init=epsg:4326")
durations.UTM <- spTransform(durations.UTM, CRS("+init=epsg:3395"))

durationsSP <- SpatialPoints(durations.UTM@coords,CRS("+init=epsg:3395")) #coordinates dataset
duplicates <- zerodist(durationsSP) #no duplicate values allowed in the values and time dataset
durationsDF <- data.frame(durations = durations.UTM$duration)#[-duplicates[,2]]
durationsTM <- durations.UTM$DepartureTime#[-duplicates[,2]]

sub_STIDF <- STIDF(durationsSP, durationsTM, data=durationsDF)
stplot(sub_STIDF) #saved as stplot_10000_hours
summary(sub_STIDF)




# Empirical Variogram Construction----------------------------------------------------------------------------------------------------------




var_10000_hours <- variogramST(durations~1, data=sub_STIDF, tunit="hours", assumeRegular = F, na.omit = T)

plot(var_10000_hours, map=F) 
plot(var_10000_hours, map=T)
plot(var_10000_hours, wireframe=T, scale=list(arrows=F))
var_10000_hours

var_nonstandard
var_standardised <- var_10000_hours


# Variogram Modelling----------------------------------------------------------------------------------------------------------




## Separable

range.s = c()
range.t = c()
nugget.s = c()
nugget.t = c()
mse = c()
for (is in (seq(from=300, to=400, by = 25))){
  for (it in (seq(from=7, to=11, by = 1))){
    for (ns in (seq(from=0.1, to=0.6, by = 0.1))){
      for (nt in (seq(from=0.1, to=0.6, by = 0.1))){
        range.s <- append(range.s, is)
        range.t <- append(range.t, it)
        nugget.s <- append(nugget.s, ns)
        nugget.t <- append(nugget.t, nt)
        mse <- append(mse, attr(fit.StVariogram(var_standardised,method = "L-BFGS-B",
                                                vgmST("separable",
                                                      space = vgm(0.5, "Sph", range = is, nugget = ns),
                                                      time = vgm(0.5, "Sph", range = it, nugget = nt), 
                                                      sill = 1.2)), "MSE"))
        
        
      }
    }
  }
}
parameter_tuning_standardised_separable <- data.frame(range.s=range.s, range.t=range.t,
                                                      nugget.s=nugget.s, nugget.t=nugget.t,
                                                      mse=mse)

## ProductSum

range.s = c()
range.t = c()
nugget.s = c()
nugget.t = c()
mse = c()
for (is in (seq(from=300, to=400, by = 25))){
  for (it in (seq(from=7, to=11, by = 1))){
    for (ns in (seq(from=0, to=0.6, by = 0.1))){
      for (nt in (seq(from=0, to=0.6, by = 0.1))){
        range.s <- append(range.s, is)
        range.t <- append(range.t, it)
        nugget.s <- append(nugget.s, ns)
        nugget.t <- append(nugget.t, nt)
        mse <- append(mse, attr(fit.StVariogram(var_standardised,method = "L-BFGS-B", 
                                                vgmST("productSum",
                                                      space = vgm(0.5, "Sph", range = is, nugget = ns),
                                                      time = vgm(0.5, "Sph", range = it, nugget = nt), 
                                                      k=0.1)), "MSE"))
        
        
      }
    }
  }
}
parameter_tuning_standardised_prodsum <- data.frame(range.s=range.s, range.t=range.t,
                                                    nugget.s=nugget.s, nugget.t=nugget.t,
                                                    mse=mse)

## Metric

range.st = c()
nugget.st = c()
mse = c()
for (ij in (seq(from=300, to=400, by = 25))){
  for (nj in (seq(from=0, to=0.6, by = 0.1))){
    range.st <- append(range.st, ij)
    nugget.st <- append(nugget.st, nj)
    mse <- append(mse, attr(fit.StVariogram(var_standardised, method = "L-BFGS-B", 
                                            vgmST("metric",
                                                  joint = vgm(0.5, "Mat", range = ij, nugget = nj), 
                                                  sill=1.5, stAni=100)), "MSE"))
  }
}

parameter_tuning_standardised_metric <- data.frame(range.st=range.st,
                                                   nugget.st=nugget.st,
                                                   mse=mse)


## SumMetric

range.st=c()
nugget.st=c()
mse = c()

for (ij in (seq(from=300, to=425, by = 25))){
  for (nj in (seq(from=0, to=0.6, by = 0.1))){
    range.st <- append(range.st, ij)
    nugget.st <- append(nugget.st, nj)
    mse <- append(mse, attr(fit.StVariogram(var_standardised, method = "L-BFGS-B", 
                                            vgmST("sumMetric",
                                                  space = vgm(0.5, "Sph", range=325, nugget=0.4),
                                                  time = vgm(0.5, "Sph", range=8, nugget = 0.2),
                                                  joint = vgm(0.1,"Mat", range=ij, nugget=nj), stAni=100)), "MSE"))

  }
}
parameter_tuning_standardised_summetric <- data.frame(range.st=range.st, nugget.st=nugget.st,
                                                      mse=mse)

## SimpleSumMetric

range.st=c()
nugget.st=c()
mse = c()
for (ij in (seq(from=300, to=450, by = 25))){
  for (nj in (seq(from=0.1, to=0.6, by = 0.1))){
    range.st <- append(range.st, ij)
    nugget.st <- append(nugget.st, nj)
    mse <- append(mse, attr(fit.StVariogram(var_standardised, method = "L-BFGS-B",
                                            vgmST("simpleSumMetric",
                                                  space = vgm(0.5, "Sph", range=325, nugget=0.1),
                                                  time = vgm(0.5, "Sph", range=8, nugget = 0.1), 
                                                  joint = vgm(0.1,"Mat", range=ij, nugget=0.1),
                                                  nugget=nj,stAni=100)), "MSE"))
  }
}

parameter_tuning_standardised_simplesummetric <- data.frame(range.st=range.st, nugget.st=nugget.st,
                                                            mse=mse)

stani.list

# Hyperparameter Fine-tuning-----------------------------------------------------------------------------------------------------




## Separable

parameter_tuning_standardised_separable
parameter_tuning_standardised_separable %>% filter(mse==min(parameter_tuning_standardised_separable$mse))
separable <- vgmST("separable",
                   space = vgm(0.5, "Sph", range=325, nugget=0.4),
                   time = vgm(0.5, "Sph", range=8, nugget=0.2),sill=1.5)
plot(var_standardised,separable,map=F)

separable_Vgm <- fit.StVariogram(var_standardised, separable,method = "L-BFGS-B")
plot(var_standardised,separable_Vgm, wireframe=T)
plot(var_standardised,separable_Vgm, map=F)

attr(separable_Vgm, "MSE")

## ProdSum

parameter_tuning_standardised_prodsum
parameter_tuning_standardised_prodsum %>% filter(mse==min(parameter_tuning_standardised_prodsum$mse))
prodSumModel <- vgmST("productSum",
                      space = vgm(0.5, "Sph", range=325, nugget=0.4),
                      time = vgm(0.5, "Sph", range=8, nugget = 0.2), k=0.1)
plot(var_standardised,prodSumModel,map=F)

prodSumModel_Vgm <- fit.StVariogram(var_standardised, prodSumModel,method = "L-BFGS-B")
plot(var_standardised,prodSumModel_Vgm, wireframe=T)
plot(var_standardised,prodSumModel_Vgm, map=F)

## Metric

parameter_tuning_standardised_metric
parameter_tuning_standardised_metric %>% filter(mse==min(parameter_tuning_standardised_metric$mse))
metric <- vgmST("metric", 
                joint = vgm(0.5,"Mat", range=375, nugget=0.2), 
                sill=1.5, stAni=100)
plot(var_standardised,metric,map=F)

metric_Vgm <- fit.StVariogram(var_standardised, metric, method="L-BFGS-B")
plot(var_standardised,metric_Vgm, wireframe=T)
plot(var_standardised,metric_Vgm, map=F)

## SumMetric

parameter_tuning_standardised_summetric
parameter_tuning_standardised_summetric %>% filter(mse==min(parameter_tuning_standardised_summetric$mse))
sumMetric <- vgmST("sumMetric", 
                   space = vgm(0.5, "Sph", range=325, nugget=0.4),
                   time = vgm(0.5, "Sph", range=8, nugget = 0.2), 
                   joint = vgm(0.1,"Mat", range=375, nugget=0.1), stAni=100)
plot(var_standardised,sumMetric,map=F)


sumMetric_Vgm <- fit.StVariogram(var_standardised, sumMetric, method="L-BFGS-B",tunit="hours")
plot(var_standardised,sumMetric_Vgm, wireframe=T)
plot(var_standardised,sumMetric_Vgm, map=F)

## SimpleSumMetric

parameter_tuning_standardised_simplesummetric
parameter_tuning_standardised_simplesummetric %>% filter(mse==min(parameter_tuning_standardised_simplesummetric$mse))
SimplesumMetric <- vgmST("simpleSumMetric", 
                         space = vgm(0.5, "Sph", range=325, nugget=0),
                         time = vgm(0.5, "Sph", range=8, nugget = 0), 
                         joint = vgm(0.1,"Mat", range=325, nugget=0), nugget=0.4, stAni=100)
plot(var_standardised,SimplesumMetric,map=F)

SimplesumMetric_Vgm <- fit.StVariogram(var_standardised, SimplesumMetric,method = "L-BFGS-B")
plot(var_standardised,SimplesumMetric_Vgm, wireframe=T)
plot(var_standardised,SimplesumMetric_Vgm, map=F)

attr(SimplesumMetric_Vgm, "MSE")

## Comparing all models

best_models_standardised <- data.frame(model.type=c("separable","prodsum","metric","summetric","simplesummetric"),
                                       lowest.mse.achievable = c(unique(filter(parameter_tuning_standardised_separable,mse==min(parameter_tuning_standardised_separable$mse))[,"mse"]),
                                                                 unique(filter(parameter_tuning_standardised_prodsum,mse==min(parameter_tuning_standardised_prodsum$mse))[,"mse"]),
                                                                 unique(filter(parameter_tuning_standardised_metric,mse==min(parameter_tuning_standardised_metric$mse))[,"mse"]),
                                                                 unique(filter(parameter_tuning_standardised_summetric,mse==min(parameter_tuning_standardised_summetric$mse))[,"mse"]),
                                                                 unique(filter(parameter_tuning_standardised_simplesummetric,mse==min(parameter_tuning_standardised_simplesummetric$mse))[,"mse"])))
plot(var_standardised,list(separable_Vgm, prodSumModel_Vgm, metric_Vgm, sumMetric_Vgm, SimplesumMetric_Vgm),all=T,wireframe=T)




# sub_test_STIDF creation----------------------------------------------------------------------------------------------------------




## durations_test.UTM,    durations_testSP, durations_testTM, durations_testDF,    sub_test_STIDF creation

y <- 2500 #respecify this sample size number and run the following chunk again for each new variogram trial
durations_test.UTM <- sample_n(sub_test_standardised, y)
coordinates(durations_test.UTM) = ~lon+lat
projection(durations_test.UTM)=CRS("+init=epsg:4326")
durations_test.UTM <- spTransform(durations_test.UTM, CRS("+init=epsg:3395"))

durations_testSP <- SpatialPoints(durations_test.UTM@coords,CRS("+init=epsg:3395")) #coordinates dataset
duplicates_test <- zerodist(durations_testSP) #no duplicate values allowed in the values and time dataset
durations_testDF <- data.frame(durations = durations_test.UTM$duration)#[-duplicates[,2]]
durations_testTM <- durations_test.UTM$DepartureTime#[-duplicates[,2]]

sub_test_STIDF <- STIDF(durations_testSP, durations_testTM, data=durations_testDF)
stplot(sub_test_STIDF) 
summary(sub_test_STIDF)




# Kriging----------------------------------------------------------------------------------------------------------




## predictions
pred <- krigeST(durations~1, data=sub_STIDF, modelList=prodSumModel_Vgm, newdata=sub_test_STIDF)

## prediction_comparison_df creation
pred_df <- data.frame(pred)
sub_test_df <- data.frame(sub_test_STIDF)
prediction_comparison_df <- merge(pred_df, sub_test_df, by=c("sp.ID", "timeIndex"))[, c("lat.x", "lon.x",
                                                                                        "sp.ID","timeIndex",
                                                                                        "durations", "var1.pred")]

## prediction comparison density plots
original_vs_duration <- melt(prediction_comparison_df ,  
                             id.vars = c("sp.ID","timeIndex","lat.x", "lon.x"), 
                             variable.name = 'series')
prediction_comparison_plot <- ggplot(original_vs_duration, aes(value)) + geom_density(aes(colour = series))
prediction_comparison_plot

## stplot for pred and sub_test_df
stplot(pred)
stplot(sub_test_STIDF)

## squared deviations
prediction_comparison_df$sq_dev <- ((prediction_comparison_df$durations - prediction_comparison_df$var1.pred)^2)
prediction_comparison_df$dev <- ((prediction_comparison_df$durations - prediction_comparison_df$var1.pred))
summary(prediction_comparison_df)
hist(prediction_comparison_df$sq_dev)
hist(prediction_comparison_df$dev)

#density plots of all models in predicting
pred1 <- krigeST(durations~1, data=sub_STIDF, modelList=separable_Vgm, newdata=sub_test_STIDF)
pred2 <- krigeST(durations~1, data=sub_STIDF, modelList=prodSumModel_Vgm, newdata=sub_test_STIDF)
pred3 <- krigeST(durations~1, data=sub_STIDF, modelList=metric_Vgm, newdata=sub_test_STIDF)
pred4 <- krigeST(durations~1, data=sub_STIDF, modelList=sumMetric_Vgm, newdata=sub_test_STIDF)
pred5 <- krigeST(durations~1, data=sub_STIDF, modelList=SimplesumMetric_Vgm_Vgm, newdata=sub_test_STIDF)

pred_df_all <- data.frame(pred1, pred2, pred3, pred4, pred5)
prediction_comparison_df_all <- merge(pred_df_all, sub_test_df, by=c("sp.ID", "timeIndex"))[, c("lat.x", "lon.x",
                                                                                                "sp.ID","timeIndex",
                                                                                                "durations", "var1.pred")]
original_vs_duration_all <- melt(prediction_comparison_df_all ,  
                                 id.vars = c("sp.ID","timeIndex","lat.x", "lon.x"),
                                 variable.name = 'series')
prediction_comparison_plot_all <- ggplot(original_vs_duration_all, aes(value)) + geom_density(aes(colour = series))
prediction_comparison_plot_all

# Outlier Detection----------------------------------------------------------------------------------------------------------




## z-score method
outlier.index <- which(abs(prediction_comparison_df$dev)>3)
length(outlier.index)
deviation_outliers <- prediction_comparison_df[c(outlier.index),]

register_google(key="AIzaSyCYIMu3bBtnw0_QVoGGq0XXpTaZ_07DDps")

melbourne_google <- get_googlemap("Melbourne, Australia",zoom=14, maptype = "terrain")
melbourne_stamen <- get_map("Melbourne,Australia", zoom=14, source= "stamen", maptype = "terrain")
melb1 <- ggmap(melbourne_google)
melb2 <- ggmap(melbourne_stamen)

qplot(x=lon.x, y=lat.x, data=deviation_outliers, geom="point") + labs(title = "Outliers (in Actual-Predicted)",
                                                                      x= "Longitude", 
                                                                      y="Latitude")

melb_parking_outliers <- melb1 + geom_point(data = deviation_outliers,
                                            aes(x=lon.x, y=lat.x),
                                            size=1, colour = "red", alpha=0.75) + labs(title = "Melbourne Parking Bays",
                                                                                       x= "Longitude", 
                                                                                       y="Latitude")





















# SPATIAL VARIOGRAMS

spatial.UTM <- sample_n(sub_train_standardised, x)
coordinates(spatial.UTM) = ~lon+lat
projection(spatial.UTM)=CRS("+init=epsg:4326")
spatial.UTM <- spTransform(spatial.UTM, CRS("+init=epsg:3395"))

durationsSP_spatial <- SpatialPoints(spatial.UTM@coords,CRS("+init=epsg:3395")) #coordinates dataset
durationsDF_spatial <- data.frame(durations = spatial.UTM$duration)#[-duplicates[,2]]

sub_spatial_STIDF <- SpatialPointsDataFrame(durationsSP_spatial, data=durationsDF_spatial)
summary(sub_spatial_STIDF)

variogram_sub_spatial_STIDF <- variogram(durations~1, sub_spatial_STIDF)
plot(variogram_sub_spatial_STIDF)
variogram_spatial_fit <- fit.variogram(variogram_sub_spatial_STIDF, model=vgm("Gau", nugget=0.5, range=350, sill=1.1))
plot(variogram_sub_spatial_STIDF, variogram_spatial_fit, pch=4)

  
  
# ROUGH ROUGH ROUGH ROUGH ROUGH ROUGH ROUGH ROUGH ROUGH ROUGH ROUGH ROUGH ROUGH ROUGH ROUGH ROUGH ROUGH ROUGH ROUGH

variogram_spatial_isotropy <- variogram(duration~1, sub_spatial,alpha=c(0,45,90,135))
plot(variogram_spatial_isotropy)

variogram_spatial_az135 <- variogram(duration~1, sub_spatial, alpha=135)
plot(variogram_spatial_az135)
variogram_spatial_fit <- fit.variogram(variogram_spatial_az135, model=vgm(psill=1, "Sph", nugget=0.6, range=0.003))
plot(variogram_spatial_az135, variogram_spatial_fit, pch=4)


acf(sub_train_standardised$duration)


# ROUGH

length(durationsTM)
length(unique(jan$DeviceId))
projection(sub) = CRS("+init=epsg:4326")

durations.UTM <- spTransform(sub,CRS("+init=epsg:3395"))


unique(january$StreetName)
nrow(january[january$StreetName == c("SWANSTON STREET",""),])

145853-length(unique(duplicates[,2]))
dupicates


# VARIOGRAM_7000 (only 47% complete) xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx

var <- variogramST(durations~1, data=sub_STIDF, tunit="days", assumeRegular = F, na.omit = T)

plot(var, map=F) 
plot(var, map=T)
plot(var, wireframe=T)
var

# VARIOGRAM_3500 (only 47% complete) xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx

var_3500 <- variogramST(durations~1, data=sub_STIDF, tunit="days", assumeRegular = F, na.omit = T)

plot(var_3500, map=F) 
plot(var_3500, map=T)
plot(var_3500, wireframe=T)
var_3500

# VARIOGRAM_2000_hours (100% complete) xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx

var_2000 <- variogramST(durations~1, data=sub_STIDF, tunit="hours", assumeRegular = F, na.omit = T)

plot(var_2000, map=F) 
plot(var_2000, map=T)
plot(var_2000, wireframe=T)
var_2000

# VARIOGRAM_3500_hours (only x% complete) xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx

var_3500_hours <- variogramST(durations~1, data=sub_STIDF, tunit="hours", assumeRegular = F, na.omit = T)

plot(var_3500_hours, map=F) 
plot(var_3500_hours, map=T)
plot(var_3500_hours, wireframe=T)
var_3500_hours

# VARIOGRAM_7000_hours (only x% complete) xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx

var_7000_hours <- variogramST(durations~1, data=sub_STIDF, tunit="hours", assumeRegular = F, na.omit = T)

plot(var_7000_hours, map=F) 
plot(var_7000_hours, map=T)
plot(var_7000_hours, wireframe=T)
var_7000_hours


attr(fit.StVariogram(var_10000_hours,method = "L-BFGS-B", vgmST("productSum",
                                                                space = vgm(0.5, "Sph", range=350, nugget=0.2),
                                                                time = vgm(0.5, "Sph", range=11, nugget = 0.2), k=0.1)), "MSE")


list.append(b, attr(fit.StVariogram(var_10000_hours,method = "L-BFGS-B", vgmST("productSum",
                                                                               space = vgm(0.5, "Sph", range=350, nugget=0.2),
                                                                               time = vgm(0.5, "Sph", range=11, nugget = 0.2), k=0.1)), "MSE"))
b=c()
b <- append(b, 2)
b <- append(b, 3)
b[2] 



range.s = c()
range.t = c()
nugget.s = c()
nugget.t = c()
mse = c()
for (is in (seq(from=300, to=400, by = 25))){
  for (it in (seq(from=7, to=11, by = 1))){
    for (ns in (seq(from=0, to=0.6, by = 0.1))){
      for (nt in (seq(from=0, to=0.6, by = 0.1))){
        range.s <- append(range.s, is)
        range.t <- append(range.t, it)
        nugget.s <- append(nugget.s, ns)
        nugget.t <- append(nugget.t, nt)
        mse <- append(mse, attr(fit.StVariogram(var_10000_hours,method = "L-BFGS-B", 
                                                vgmST("productSum",
                                                      space = vgm(0.5, "Sph", range = is, nugget = ns),
                                                      time = vgm(0.5, "Sph", range = it, nugget = nt), 
                                                      k=0.1)), "MSE"))
        
        
      }
    }
  }
}
parameter_tuning_standardised_separable <- data.frame(range.s=range.s, range.t=range.t,
                                                      nugget.s=nugget.s, nugget.t=nugget.t,
                                                      mse=mse)
parameter_tuning_standardised_separable %>% filter(mse==min(parameter_tuning_standardised_separable$mse))




