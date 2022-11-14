#Set working directory
setwd("/users/13313/Documents/Data")

# Upload Divvy datasets (csv files) here
q2_2019 <- read_csv("Divvy_Trips_2019_Q2.csv")
q3_2019 <- read_csv("Divvy_Trips_2019_Q3.csv")
q4_2019 <- read_csv("Divvy_Trips_2019_Q4.csv")
q1_2020 <- read_csv("Divvy_Trips_2020_Q1.csv")

# Compare column names each of the files
colnames(q3_2019)
colnames(q4_2019)
colnames(q2_2019)
colnames(q1_2020)

# Rename columns  to make them consistent with q1_2020 (as this will be the supposed going-forward table design for Divvy)
(q4_2019 <- rename(q4_2019
                   ,ride_id = trip_id
                   ,rideable_type = bikeid 
                   ,started_at = start_time  
                   ,ended_at = end_time  
                   ,start_station_name = from_station_name 
                   ,start_station_id = from_station_id 
                   ,end_station_name = to_station_name 
                   ,end_station_id = to_station_id 
                   ,member_casual = usertype))

(q3_2019 <- rename(q3_2019
                   ,ride_id = trip_id
                   ,rideable_type = bikeid 
                   ,started_at = start_time  
                   ,ended_at = end_time  
                   ,start_station_name = from_station_name 
                   ,start_station_id = from_station_id 
                   ,end_station_name = to_station_name 
                   ,end_station_id = to_station_id 
                   ,member_casual = usertype))

(q2_2019 <- rename(q2_2019
                   ,ride_id = "01 - Rental Details Rental ID"
                   ,rideable_type = "01 - Rental Details Bike ID" 
                   ,started_at = "01 - Rental Details Local Start Time"  
                   ,ended_at = "01 - Rental Details Local End Time"  
                   ,start_station_name = "03 - Rental Start Station Name" 
                   ,start_station_id = "03 - Rental Start Station ID"
                   ,end_station_name = "02 - Rental End Station Name" 
                   ,end_station_id = "02 - Rental End Station ID"
                   ,member_casual = "User Type"))

# Inspect the dataframes and look for incongruences
str(q1_2020)
str(q4_2019)
str(q3_2019)
str(q2_2019)


# Convert ride_id and rideable_type to character so that they can stack correctly
q4_2019 <-  mutate(q4_2019, ride_id = as.character(ride_id)
                   ,rideable_type = as.character(rideable_type)) 
q3_2019 <-  mutate(q3_2019, ride_id = as.character(ride_id)
                   ,rideable_type = as.character(rideable_type)) 
q2_2019 <-  mutate(q2_2019, ride_id = as.character(ride_id)
                   ,rideable_type = as.character(rideable_type)) 

# Stack individual quarter's data frames into one big data frame
all_trips <- bind_rows(q2_2019, q3_2019, q4_2019, q1_2020)

# Remove lat, long, birthyear, and gender fields as this data was dropped beginning in 2020
all_trips <- all_trips %>%  
  select(-c(start_lat, start_lng, end_lat, end_lng, birthyear, gender, "01 - Rental Details Duration In Seconds Uncapped", "05 - Member Details Member Birthday Year", "Member Gender", "tripduration"))

# Inspect the new table
colnames(all_trips)
nrow(all_trips)
dim(all_trips)
head(all_trips)
str(all_trips)
summary(all_trips)

# In the "member_casual" column, replace "Subscriber" with "member" and "Customer" with "casual"
# Before 2020, Divvy used different labels for these two types of riders 
# Change dataframe to be consistent with their current nomenclature
all_trips <- all_trips %>% mutate(member_casual = 
                                    recode(member_casual 
                                           ,"Subscriber" = "member"
                                           , "Customer" = "casual"))
# Check to make sure the naming is consistent
table(all_trips$member_casual)

# Add columns that list the date, month, day, and year of each ride
# We can then aggregate ride data for each month, day, or year ... before completing these operations we could only aggregate at the ride level
all_trips$date <- as.Date(all_trips$started_at)#Default format is yyyy-mm-dd
all_trips$month <- format(as.Date(all_trips$date), "%m")
all_trips$day <- format(as.Date(all_trips$date), "%d")
all_trips$year <- format(as.Date(all_trips$date), "%Y")
all_trips$day_of_week <- format(as.Date(all_trips$date), "%A")

# Add a "ride_length" calculation to all_trips (in seconds)
all_trips$ride_length <- difftime(all_trips$ended_at,all_trips$started_at)

# Inspect the structure of the columns
str(all_trips)

# Convert "ride_length" from Factor to numeric so we can run calculations on the data
is.factor(all_trips$ride_length)
all_trips$ride_length <- as.numeric(as.character(all_trips$ride_length))
is.numeric(all_trips$ride_length)

#List invalid row.  The dataframe includes a few hundred entries when bikes were taken out of docks and checked for quality by Divvy or ride_length was negative
sqldf("SELECT * FROM all_trips WHERE ride_length < 0",  method = "name__class")

# Remove "bad" data
# We will create a new version of the dataframe (v2) since data is being removed
all_trips_v2 <- all_trips[!(all_trips$start_station_name == "HQ QR" | all_trips$ride_length<0),]

#Confirm that all "bad" data was removed
sqldf("SELECT * FROM all_trips_v2 WHERE ride_length < 0",  method = "name__class")

#Descriptive analysis on ride_length (all figures in seconds)
summary(all_trips_v2$ride_length)

# Compare members and casual users in new dataframe
agg <- aggregate(all_trips_v2$ride_length ~ all_trips_v2$member_casual, FUN = summary)

view(agg)
#Note that mean and median ride times are far higher in casual riders than members
#Working hypothesis - casuals feel more obligated to get the most out of their rides
#possible solution - reduce member prices so that it makes more financial sense to become a member

#Fix order of days of week
all_trips_v2$day_of_week <- ordered(all_trips_v2$day_of_week, levels = c("Sunday", "Monday", "Tuesday"
                                                                         ,"Wednesday", "Thursday", "Friday"
                                                                         ,"Saturday"))

# See the average ride time by each day for members vs casual users in new dataframe
agg2 <- aggregate(all_trips_v2$ride_length ~ all_trips_v2$member_casual + all_trips_v2$day_of_week, FUN = mean)

view(agg2)
#Same findings as above, nothing to dissuade working hypothesis
#Highest Casual ride length on Wednesdays and Fridays, lowest on Saturday and Monday
#Highest Member ride length on Saturday and Sunday, lowest on Wednesday and Thursday, with Friday as a close third
#Highest days are %15 greater average ride length for members, 12% for greater average ride length for casuals
#Casual riders have a higher variance ride time
#Highest casual days coincide with lowest member days.  
#Possible recommendation - increase marketing on high casual ride days to target these riders

# analyze ridership data by type and weekday in new dataframe
agg3 <- all_trips_v2 %>% 
  group_by(member_casual, day_of_week) %>%  #groups by usertype and weekday
  summarise(number_of_rides = n(),							#calculates the number of rides and average duration 
            average_duration = mean(ride_length)) %>% 		# calculates the average duration
  arrange(member_casual, day_of_week)		

view(agg3)
#Highest number of casual rides on Saturday, which also has the lowest ride length.
#Next highest casual ride day is sunday, which has a middling ride length
#Weekday casual ride total is significantly lower than weekends
#Weekday member ride total is significantly higher than weekends
#possible conclusion - members use bikes for commuting to work, casuals use them to go out on the weekends
#possible recommendation - same as above, targeting marketing towards weekend riders
#possible recommendation - gear marketing (getting new members) towards nightlife - going out on weekends
#possible message - membership being allowing more freedom to go out at night and being easier on their pocketbook

#visualize number of rides by rider type
ggplot(data = agg3, aes(x = day_of_week, y = number_of_rides, fill = member_casual)) +
  geom_col(position = "dodge")
#confirmed above observations about days of week


#visualize average duration
ggplot(data = agg3, aes(x = day_of_week, y = average_duration, fill = member_casual)) +
  geom_col(position = "dodge")
#Seeing it makes me pause on my above duration observations and possible conclusions
#The visual shows just how miniscule the variation is on ride duration throughout the week, both for members and casuals
#What is now much more clear is that ride duration is far greater for casuals than members
#This does support my hypothesis that casuals want to get more bang for their buck, whereas members don't worry about it
#Possible recommendation - gear marketing towards the hassle of dragging the bike around all day
#Cont - It's easier to put a bike back, go into a restaurant or bar for a while, then come back and get another if they were member
#It's much more efficient to use this system as a member.  
#Funny marketing idea - Short scene of a young person getting stopped at a club with a bike
#They tell the bouncer - "But I just have this single ride pass, I need this afterward."
#Then some other person right behind them sets the bike at the bike station outside the club and walks in unhindered

#Plot member_casual vs. Average duration to show overall ride trend
ggplot(data = agg3, aes(x = member_casual, y = average_duration, fill = member_casual)) +
  geom_col()
#Casuals take much longer rides than Members

#Plot member_casual vs. number_of_rides to show overall ride trend
ggplot(data = agg3, aes(x = member_casual, y = number_of_rides, fill = member_casual)) +
  geom_col(postion = "dodge")
#Members take much more rides than non-members

# Create a csv file that we will visualize in presentation software
counts <- aggregate(all_trips_v2$ride_length ~ all_trips_v2$member_casual + all_trips_v2$day_of_week, FUN = mean)
write.csv(counts, file = '/users/13313/Documents/Data/avg_ride_length.csv')



