# Data Camp - Working with Dates and Times in R
# https://campus.datacamp.com/courses/working-with-dates-and-times-in-r/dates-and-times-in-r
# 2020-10-03
install.packages("anytime")
install.packages("microbenchmark")
library(microbenchmark)
install.packages("fasttime")
library(fasttime)

library(anytime)
library(tidyverse)

#----- Ch 1: Dates and Times in R -----

releases <- read_csv("data/rversions.csv")
str(releases)

# Set the x axis to the date column
ggplot(releases, aes(x = date, y = type)) +
  geom_line(aes(group = 1, color = factor(major)))

# Limit the axis to between 2010-01-01 and 2014-01-01
ggplot(releases, aes(x = date, y = type)) +
  geom_line(aes(group = 1, color = factor(major))) +
  xlim(as.Date("2010-01-01"), as.Date("2014-01-01"))

# Specify breaks every ten years and labels with "%Y"
ggplot(releases, aes(x = date, y = type)) +
  geom_line(aes(group = 1, color = factor(major))) +
  scale_x_date(date_breaks = "10 years", date_labels = "%Y")


# Find the largest date
last_release_date <- max(releases$date)
# Filter row for last release
last_release <- filter(releases, date == last_release_date)
# Print last_release
last_release
# How long since last release?
Sys.Date() - last_release_date


# Use as.POSIXct to enter the datetime 
as.POSIXct("2010-10-01 12:12:00")
# Use as.POSIXct again but set the timezone to `"America/Los_Angeles"`
as.POSIXct("2010-10-01 12:12:00", tz = "America/Los_Angeles")
# Use read_csv to import rversions.csv
releases <- read_csv("data/rversions.csv")
# Examine structure of datetime column
str(releases$datetime)


# Import "cran-logs_2015-04-17.csv" with read_csv()
logs <- read_csv("data/cran-logs_2015-04-17.csv")
logs    # Print logs
# Store the release time as a POSIXct object
release_time <- as.POSIXct("2015-04-16 07:13:33", tz = "UTC")

# When is the first download of 3.2.0?
logs %>% 
  filter(datetime > release_time, r_version == "3.2.0")

# Examine histograms of downloads by version
ggplot(logs, aes(x = datetime)) +
  geom_histogram() +
  geom_vline(aes(xintercept = as.numeric(release_time)))+
  facet_wrap(~ r_version, ncol = 1)


#----- Ch 2: Parsing and Manipulating Dates and Times with lubridate -----
library(lubridate)

x <- "2010 September 20th" # 2010-09-20   # Parse x
ymd(x)

y <- "02.01.2010"  # 2010-01-02   # Parse y 
dmy(y)

z <- "Sep, 12th 2010 14:00"  # 2010-09-12T14:00   # Parse z 
mdy_hm(z)

?parse_date_time    # can be slow b/c it is forgiving and flexible.

x <- "Monday June 1st 2010 at 4pm"
parse_date_time(x, orders = "AmdyIp")  # Specify an order string to parse x

two_orders <- c("October 7, 2001", "October 13, 2002", "April 13, 2003", "17 April 2005", "23 April 2017")
parse_date_time(two_orders, orders = c("dmy", "mdy"))  # Specify order to include both "mdy" and "dmy"

short_dates <- c("11 December 1282", "May 1372", "1253")
parse_date_time(short_dates, orders = c("dOmY", "OmY", "Y"))  # Specify order to include "dOmY", "OmY" and "Y"


library(lubridate)
library(readr)
library(dplyr)
library(ggplot2)

akl_daily_raw <- read_csv("data/akl_weather_daily.csv")  # Import CSV with read_csv()
akl_daily_raw  # Print akl_daily_raw
akl_daily <- akl_daily_raw %>%  # Parse date 
  mutate(date = ymd(date))

ggplot(akl_daily, aes(x=date, y=max_temp)) +  # Plot to check work
  geom_line()



akl_hourly_raw <- read_csv("data/akl_weather_hourly_2016.csv")  # Import "akl_weather_hourly_2016.csv"
akl_hourly_raw   # Print akl_hourly_raw
akl_hourly  <- akl_hourly_raw  %>% 
  mutate(date = make_date(year = year, month = month, day = mday))  # Use make_date() to combine year, month and mday 
akl_hourly <- akl_hourly  %>% 
  mutate(
    datetimemon_string = paste(date, time, sep = "T"),  # Parse datetime_string 
    datetime = ymd_hms(datetime_string)
  )
akl_hourly %>% select(date, time, datetime)  # Print date, time and datetime columns of akl_hourly

# Plot to check work
ggplot(akl_hourly, aes(x = datetime, y = temperature)) +
  geom_line()

release_time <- releases$datetime
head(release_time)  # Examine the head() of release_time
head(month(release_time))  # Examine the head() of the months of release_time
month(release_time) %>% table()  # Extract the month of releases 
year(release_time) %>% table()  # Extract the year of releases
mean(hour(release_time) < 12)  # How often is the hour before 12 (noon)?
mean(am(release_time))  # How often is the release in am?



wday(releases$datetime) %>% table()  # Use wday() to tabulate release by day of the week
wday(releases$datetime, label = TRUE, abbr = FALSE) %>% table()  # Add label = TRUE to make table more readable
releases$wday <- wday(releases$datetime, label = TRUE, abbr = FALSE)  # Create column wday to hold labelled week days
# Plot barchart of weekday by type of release
ggplot(releases, aes(wday)) +
  geom_bar() +
  facet_wrap(~ type, ncol = 1, scale = "free_y")


install.packages("ggridges")
library(ggridges)
akl_daily <- akl_daily %>%  # Add columns for year, yday and month
  mutate(
    year = year(date),
    yday = yday(date),
    month = month(date, label = TRUE, abbr = FALSE))

ggplot(akl_daily, aes(x = yday, y = max_temp)) +  # Plot max_temp by yday for all years
  geom_line(aes(group = year), alpha = 0.5)

ggplot(akl_daily, aes(x = max_temp, y = month, height = ..density..)) +  # Examine distribution of max_temp by month
  geom_density_ridges(stat = "density")


# Create new columns hour, month and rainy
akl_hourly <- akl_hourly %>%
  mutate(
    hour = hour(datetime),
    month = month(datetime, label = TRUE),
    rainy = weather == "Precipitation"
  )

akl_day <- akl_hourly %>% 
  filter(hour >=8 , hour <= 22)  # Filter for hours between 8am and 10pm (inclusive)

rainy_days <- akl_day %>% 
  group_by(month, date) %>%
  summarise(any_rain = any(rainy))  # Summarise for each date if there is any rain

rainy_days %>% 
  summarise(days_rainy = sum(any_rain)) # Summarise for each month, the number of days with rain

# round_date() ceiling_date()  floor_date()  
# specify unit: "second", "minute", hour, day, week, month, bimonth, quarter, halfyear, year
# or multiples: "2 years"  "6 hours"

r_3_4_1 <- ymd_hms("2016-05-03 07:13:28 UTC")
floor_date(r_3_4_1, unit = "day")  # Round down to day
round_date(r_3_4_1, unit = "5 minutes")  # Round to nearest 5 minutes
ceiling_date(r_3_4_1, unit = "week")  # Round up to week 
r_3_4_1 - round_date(r_3_4_1, unit = "day")  # Subtract r_3_4_1 rounded down to day



akl_hourly <- akl_hourly %>%  # Create day_hour, datetime rounded down to hour
  mutate(
    day_hour = floor_date(datetime, unit = "hour")
  )

akl_hourly %>%   # Count observations per hour  
  count(day_hour) 

akl_hourly %>%   # Find day_hours with n != 2  
  count(day_hour) %>%
  filter(n != 2) %>% 
  arrange(desc(n))


#----- Ch 3: Arithmetic with Dates and Times -----
#difftime()  units = "secs" "mins" "hours" "days" or "weeks"
#now() and today()

date_landing <- mdy("July 20, 1969")
moment_step <- mdy_hms("July 20, 1969, 02:56:15", tz = "UTC")  # The date of landing and moment of step
difftime(today(), date_landing, units = "days")  # How many days since the first man on the moon?
difftime(now(), moment_step, units = "secs")  # How many seconds since the first man on the moon?

# Three dates
mar_11 <- ymd_hms("2017-03-11 12:00:00", tz = "America/Los_Angeles")
mar_12 <- ymd_hms("2017-03-12 12:00:00", tz = "America/Los_Angeles")
mar_13 <- ymd_hms("2017-03-13 12:00:00", tz = "America/Los_Angeles")
difftime(mar_13, mar_12, units = "secs")  # Difference between mar_13 and mar_12 in seconds
difftime(mar_12, mar_11, units = "secs")  # Difference between mar_12 and mar_11 in seconds

# Time spans in Lubridate
# period - human concept.  +1 day = same time tomorrow (ex: ignores DST +1hr/-1hr in spring/fall)
# duration - stopwatch concept  +1 day = + 86400 seconds
# creating timespan
# periods:  seconds()  minutes()  hours()  days()  weeks() months() years()
# duration: dseconds() dminutes() dhours() ddays() dweeks()   -    dyears()
# days(2)  "2d 0H 0M 0S"
# ddays(2)    # duration days


mon_2pm <- dmy_hm("27 Aug 2018 14:00")
mon_2pm + weeks(1)  # Add a period of one week to mon_2pm

tue_9am <- dmy_hm("28 Aug 2018 9:00")
tue_9am + hours(81)  # Add a duration of 81 hours to tue_9am

today() - years(-5)  # Subtract a period of five years from today()
today() - dyears(-5)  # Subtract a duration of five years from today()



eclipse_2017 <- ymd_hms("2017-08-21 18:26:40")  # Time of North American Eclipse 2017
synodic <- ddays(29) + dhours(12) + dminutes(44) + dseconds(3)  # Duration of 29 days, 12 hours, 44 mins and 3 secs
saros <- 223 * synodic  # 223 synodic months
eclipse_2017 + saros  # Add saros to eclipse_2017



today_8am <- today() + hours(8)  # Add a period of 8 hours to today
every_two_weeks <- 1:26 * weeks(2)  # Sequence of two weeks from 1 to 26
today_8am + every_two_weeks  # Create datetime for every two weeks for a year



jan_31 <- ymd("2018-01-31")
month_seq <- 1:12 * months(1)   # A sequence of 1 to 12 periods of 1 month
jan_31 + month_seq  # Add 1 to 12 months to jan_31    (months without a 31 are NA)
jan_31 %m+% month_seq  # Replace + with %m+%    (months without a 31 are end of that month)
jan_31 %m-% month_seq  # Replace + with %m-%

# creating intervals (has a startdatetime and an enddatetime)
# datetime1 %--% datetime 2   or   # interval(datetime1, datetime2)
# int_start() int_end int_length()


monarchs  # Print monarchs
monarchs <- monarchs %>%
  mutate(reign = from %--% to)   # Create an interval for reign

monarchs %>%
  mutate(length = int_length(reign)) %>%   # Find the length of reign, and arrange
  arrange(desc(length)) %>%
  select(name, length, dominion)


halleys  # Print halleys

halleys <- halleys %>% 
  mutate(visible = start_date %--% end_date)  # New column for interval from start to end date

halleys_1066 <- halleys[14, ]   # The visitation of 1066

monarchs %>% 
  filter(halleys_1066$perihelion_date %within% reign) %>%  # Monarchs in power on perihelion date
  select(name, from, to, dominion)

monarchs %>% 
  filter(int_overlaps(halleys_1066$visible, reign)) %>%  # Monarchs whose reign overlaps visible time
  select(name, from, to, dominion)


monarchs <- monarchs %>%
  mutate(
    duration = as.duration(reign),  # New columns for duration and period
    period = as.period(reign)) 

monarchs %>%
  select(name, duration, period)  # Examine results    


#----- Ch 4: Problems in practice -----
library(tidyverse)
library(lubridate)
rm(list = ls())
Sys.timezone()
OlsonNames()  #IANA list of time zones
force_tz()  # change the timezone w/o changing the clock.
with_tz() # view same instant in different timezone


game2 <- mdy_hm("June 11 2015 19:00")  # Game2: CAN vs NZL in Edmonton
game3 <- mdy_hm("June 15 2015 18:30")  # Game3: CHN vs NZL in Winnipeg

game2_local <- force_tz(game2, tzone = "America/Edmonton")  # Set the timezone to "America/Edmonton"
game2_local

game3_local <- force_tz(game3, tzone = "America/Winnipeg")  # Set the timezone to "America/Winnipeg"
game3_local

as.period(game2_local %--% game3_local)  # How long does the team have to rest?


with_tz(game2_local, tzone = "Pacific/Auckland")  # What time is game2_local in NZ?
with_tz(game2_local, tzone = "America/Los_Angeles")  # What time is game2_local in Corvallis, Oregon?
with_tz(game3_local, tzone = "Pacific/Auckland")  # What time is game3_local in NZ?


akl_hourly <- read_csv("data/akl_weather_hourly_2016.csv")
akl_hourly  <- akl_hourly  %>% 
  mutate(date = make_date(year = year, month = month, day = mday),  # Use make_date() to combine year, month and mday 
  datetime_string = paste(date, time, sep = "T"),  # Parse datetime_string 
  datetime = ymd_hms(datetime_string)
  )
head(akl_hourly$datetime)  # Examine datetime and date_utc columns
head(akl_hourly$date_utc)
akl_hourly <- akl_hourly %>%
  mutate(
    datetime = force_tz(datetime, tzone = "Pacific/Auckland"))  # Force datetime to Pacific/Auckland
head(akl_hourly$datetime)  # Reexamine datetime
table(akl_hourly$datetime - akl_hourly$date_utc)  # Are datetime and date_utc the same moments?  4 are different b/c of DST



akl_hourly <- read_csv("data/akl_weather_hourly_2016.csv")  # Import auckland hourly data 
str(akl_hourly$time)  # Examine structure of time column
head(akl_hourly$time)  # Examine head of time column


ggplot(akl_hourly, aes(x = time, y = temperature)) +  # A plot using just time
  geom_line(aes(group = make_date(year, month, mday)), alpha = 0.2)

#--- importing/exporting times ---
#parse_date_time() can be slow b/c it's forgiving and flexible
install.packages("fasttime")
library(fasttime)
x <- "2001-02-27"
parse_date_time(x, order = "ymd")
fasttime::fastPOSIXct(x, tz = "UTC")  # if date/times are in ISO 8601 format, this is faster.
fasttime::fastPOSIXct(x)
fast_strptime(x, format = "%Y-%m-%d")  # lubridate function.  Similar to parse_date_time, but is less forgiving.  
                                       # Format must be explicit and exact.  Ex here, the dashes must be included

# exporting "pretty"
mystamp <- lubridate::stamp("Saturday October 30 1999")
mystamp(lubridate::ymd("1999-10-30"))

lubridate::parse_date_time("2001-02-27", order = "ymd")  # parses OK
lubridate::parse_date_time("2001-02-27 00:00:00", order = "ymd")  #fails to parse


library(microbenchmark)
library(fasttime)

str(dates)  # Examine structure of dates
fastPOSIXct(dates) %>% str()  # Use fastPOSIXct() to parse dates

microbenchmark(
  ymd_hms = ymd_hms(dates),  # Compare speed of fastPOSIXct() to ymd_hms()
  fasttime = fastPOSIXct(dates),
  times = 20)

fast_strptime(dates, format = "%Y-%m-%dT%H:%M:%SZ") %>% str()  # Parse dates with fast_strptime

microbenchmark(                        # Comparse speed to ymd_hms() and fasttime
  ymd_hms = ymd_hms(dates),
  fasttime = fastPOSIXct(dates),
  fast_strptime = fast_strptime(dates, format = "%Y-%m-%dT%H:%M:%SZ"),
  times = 20)


date_stamp <- stamp("Saturday, Jan 1, 2000")  # Create a stamp based on "Saturday, Jan 1, 2000"
date_stamp  # Print date_stamp
date_stamp(today())  # Call date_stamp on today()
stamp("12/31/1999")(today())  # Create and call a stamp based on "12/31/1999"