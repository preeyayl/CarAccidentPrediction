[Yesterday 8:56 pm] Ratna Preeya Yen Leng Mahantheran

getwd()
setwd("C:/Users/bryan/OneDrive/Desktop/UM/Sem 1/WQD7003_Data Analytics/Assignment")
df <- read.csv('Kaagle_Upload.csv')

#Removed missing values
df <- df[complete.cases(df), ]

# Structure of data set
str(df)

# Removal of useless attributes
df <- subset(df, select=-c(accident_index, vehicle_reference, date, time, casualty_reference, local_authority_.district., local_authority_.highway., X1st_road_number, X2nd_road_number, lsoa_of_accident_location))

# Conversion of data set into correct form
df$vehicle_type <- as.factor(df$vehicle_type)
df$towing_and_articulation <- as.factor(df$towing_and_articulation)
df$vehicle_manoeuvre <- as.factor(df$vehicle_manoeuvre)
df$vehicle_location.restricted_lane <- as.factor(df$vehicle_location.restricted_lane)
df$junction_location <- as.factor(df$junction_location)
df$skidding_and_overturning <- as.factor(df$skidding_and_overturning)
df$hit_object_in_carriageway <- as.factor(df$hit_object_in_carriageway)
df$vehicle_leaving_carriageway <- as.factor(df$vehicle_leaving_carriageway)
df$hit_object_off_carriageway <- as.factor(df$hit_object_off_carriageway)
df$X1st_point_of_impact <- as.factor(df$X1st_point_of_impact)
df$was_vehicle_left_hand_drive. <- as.factor(df$was_vehicle_left_hand_drive.)
df$journey_purpose_of_driver <- as.factor(df$journey_purpose_of_driver)
df$sex_of_driver <- as.factor(df$sex_of_driver)
df$age_band_of_driver <- as.factor(df$age_band_of_driver)
df$propulsion_code <- as.factor(df$propulsion_code)
df$driver_imd_decile <- as.factor(df$driver_imd_decile)
df$driver_home_area_type <- as.factor(df$driver_home_area_type)
df$vehicle_imd_decile <- as.factor(df$vehicle_imd_decile)
df$police_force <- as.factor(df$police_force)
df$accident_severity <- as.factor(df$accident_severity)
df$police_force <- as.factor(df$police_force)
df$X1st_road_class <- as.factor(df$X1st_road_class)
df$road_type <- as.factor(df$road_type)
df$speed_limit <- as.factor(df$speed_limit)
df$junction_detail <- as.factor(df$junction_detail)
df$junction_control <- as.factor(df$junction_control)
df$X2nd_road_class <- as.factor(df$X2nd_road_class)
df$pedestrian_crossing.human_control <- as.factor(df$pedestrian_crossing.human_control)
df$pedestrian_crossing.physical_facilities <- as.factor(df$pedestrian_crossing.physical_facilities)
df$light_conditions <- as.factor(df$light_conditions)
df$weather_conditions <- as.factor(df$weather_conditions)
df$road_surface_conditions  <- as.factor(df$road_surface_conditions)
df$special_conditions_at_site  <- as.factor(df$special_conditions_at_site)
df$carriageway_hazards  <- as.factor(df$carriageway_hazards)
df$urban_or_rural_area  <- as.factor(df$urban_or_rural_area)
df$did_police_officer_attend_scene_of_accident  <- as.factor(df$did_police_officer_attend_scene_of_accident)
df$casualty_class  <- as.factor(df$casualty_class)
df$sex_of_casualty  <- as.factor(df$sex_of_casualty)
df$age_band_of_casualty  <- as.factor(df$age_band_of_casualty)
df$age_band_of_casualty  <- as.factor(df$age_band_of_casualty)
df$casualty_severity  <- as.factor(df$casualty_severity)
df$pedestrian_location <- as.factor(df$pedestrian_location)
df$pedestrian_movement  <- as.factor(df$pedestrian_movement)
df$car_passenger  <- as.factor(df$car_passenger)
df$bus_or_coach_passenger <- as.factor(df$bus_or_coach_passenger)
df$pedestrian_road_maintenance_worker <- as.factor(df$pedestrian_road_maintenance_worker)
df$casualty_type <- as.factor(df$casualty_type)
df$casualty_home_area_type <- as.factor(df$casualty_home_area_type)
df$casualty_imd_decile <- as.factor(df$casualty_imd_decile)


#Remove those factors with only 1 factor level
df <- subset(df, select=-c(driver_imd_decile, vehicle_imd_decile))
str(df)

#Variable Selection
library(leaps)
allsub1 <- regsubsets(accident_severity~., data=df, nvmax=58, method="forward")
summary(allsub1)
res.sum <- summary(allsub1)
which.max(res.sum$adjr2)



