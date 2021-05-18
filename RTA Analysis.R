df <- read.csv('Kaagle_Upload.csv')

#Attributes of the data set
names(df)

# Removal of useless attributes
df <- subset(df, select=-c(accident_index, vehicle_reference, age_of_driver, number_of_casualties, NUmber_of_Casualities_unique_to_accident_index, No_of_Vehicles_involved_unique_to_accident_index, location_easting_osgr, location_northing_osgr, longitude, latitude, police_force, number_of_casualties, date, day_of_week, time, local_authority_.district., local_authority_.highway., X1st_road_class, X1st_road_number, X2nd_road_class, X2nd_road_number, did_police_officer_attend_scene_of_accident, lsoa_of_accident_location))

#Remove those factors with only 1 factor level
df <- subset(df, select=-c(driver_imd_decile, vehicle_imd_decile))

#Remove those variables related to casualty since we are looking for the factors affecting the severity of car accidents
df <- subset(df, select=-c(casualty_reference, casualty_class, sex_of_casualty, age_of_casualty, age_band_of_casualty, casualty_severity, pedestrian_location, pedestrian_movement, car_passenger, bus_or_coach_passenger, pedestrian_road_maintenance_worker, casualty_type, casualty_home_area_type, casualty_imd_decile))

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
df$driver_home_area_type <- as.factor(df$driver_home_area_type)
df$accident_severity <- as.factor(df$accident_severity)
df$road_type <- as.factor(df$road_type)
df$speed_limit <- as.factor(df$speed_limit)
df$junction_detail <- as.factor(df$junction_detail)
df$junction_control <- as.factor(df$junction_control)
df$pedestrian_crossing.human_control <- as.factor(df$pedestrian_crossing.human_control)
df$pedestrian_crossing.physical_facilities <- as.factor(df$pedestrian_crossing.physical_facilities)
df$light_conditions <- as.factor(df$light_conditions)
df$weather_conditions <- as.factor(df$weather_conditions)
df$road_surface_conditions  <- as.factor(df$road_surface_conditions)
df$special_conditions_at_site  <- as.factor(df$special_conditions_at_site)
df$carriageway_hazards  <- as.factor(df$carriageway_hazards)
df$urban_or_rural_area  <- as.factor(df$urban_or_rural_area)

#age_of_vehicle change to age_band_of_vehicle
library(dplyr)
df <- df %>% mutate(age_band_of_vehicle = case_when(age_of_vehicle >= 0 & age_of_vehicle <= 10 ~ '10',
                                                    age_of_vehicle > 10 & age_of_vehicle <= 20 ~ '20',
                                                    age_of_vehicle > 20 & age_of_vehicle <= 30 ~ '30',
                                                    age_of_vehicle > 30 & age_of_vehicle <= 40 ~ '40',
                                                    age_of_vehicle > 40 & age_of_vehicle <= 50 ~ '50',
                                                    age_of_vehicle > 50 & age_of_vehicle <= 60 ~ '60',
                                                    age_of_vehicle > 60 & age_of_vehicle <= 70 ~ '70',
                                                    age_of_vehicle > 70 & age_of_vehicle <= 80 ~ '80',
                                                    age_of_vehicle > 80 & age_of_vehicle <= 90 ~ '90',
                                                    age_of_vehicle > 90 ~ '100',
                                                    age_of_vehicle == -1 ~ '-1'))

#remove age_of_vehicle & convert age_band_of_vehicle to factor
df <- subset(df, select=-c(age_of_vehicle))
df$age_band_of_vehicle <- as.factor(df$age_band_of_vehicle)
str(df)
summary(df)

#complete length of summary
summary(df, maxsum = max(lengths(lapply(df, unique))))

#convert -1 to NA in each column
df[df == -1] <- NA

#convert "data not known" of 3 in the columns of sex of driver & urban or rural area to NA
df$sex_of_driver[df$sex_of_driver == 3] <- NA
df$urban_or_rural_area[df$urban_or_rural_area == 3] <- NA
colSums(is.na(df))

#Drop those variables with lots of missing values that have little relation to the dependent variable (perhaps need to prove this with correlation heatmap?)
df <- subset(df, select=-c(propulsion_code, driver_home_area_type, engine_capacity_.cc.))

#Imputation of the missing values with -1 for the variables with high missing rate
df$junction_control[is.na(df$junction_control)] <- -1
df$age_band_of_vehicle[is.na(df$age_band_of_vehicle)] <- -1

#Removal of missing values
df <- df[complete.cases(df), ]
colSums(is.na(df))
n <-nrow(df)

#Separate into training and valid
new.order = sample.int(n) ### Shuffled numbers from 1 to n
size.train = floor(n*0.75) ### Number of observations in our training
### set. Use floor() to round down
ind.train = new.order[1:size.train] ### Indices of observations
### to put in training set
ind.valid = new.order[(size.train + 1):n] ### Indices of observations
### to put in validation set
data.train = df[ind.train, ] ### Keep only observations in ind.train
data.valid= df[ind.valid, ] ### Keep only observations in ind.valid
print(data.train)

#Variable Selection using Boruta
set.seed(111)
library(Boruta)
boruta <- Boruta(accident_severity~., data = data.train, doTrace = 2)
print(boruta)
fd <- boruta$finalDecision
confirmed_vars <- fd[which(fd == "Confirmed")] %>% names
print(confirmed_vars)
