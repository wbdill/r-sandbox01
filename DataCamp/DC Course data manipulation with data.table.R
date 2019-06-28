#---------------------
# Coursera Data Manipulation in r with data.table (2019-06-24)
# https://www.rdocumentation.org/packages/bikeshare14/versions/0.1.2
rm(list = ls())
library(data.table)
#install.packages("bikeshare14")
library(bikeshare14)

#----- Chapter 1 -----
# 3 ways to create data.table
# data.table() as.data.table() fread()
# data.table doesn't convert strings to factors and never sets or uses row names
y <- list(id = 1:2, name = c("a", "b"))
x <- as.data.table(y)

# DT[i, j, by]   i = which rows, j=what to do, by = group by
# .N = number of last row

head(batrips)

batrips <- as.data.table(batrips)
batrips[bike_id == 473]

indices(batrips)  # indexes are auto-created whena a column is filtered so future queries are faster

batrips[1:10]
batrips[start_station == "Ryland Park" & subscription_type != "Customer"]
batrips[end_station %like% "Market$"]
batrips[trip_id %in% c(588841, 139560, 139562)]
batrips[duration %between% c(5000,6000)]
batrips[start_station %chin% c("San Francisco City Hall", "Embarcadero at Sansome")]  # %chin% is %in% optimized for chars

#----- Chapter 2 -----
# .() is an alias for list()
batrips[, c("bike_id", "trip_id")]
batrips[, .(bike_id, trip_id)]

batrips[, mean(duration)]
batrips[start_station == "Japantown", .N] # how many rows started in Japantown?
batrips[end_station == "Market at 10th", median(duration)]

batrips[, difftime(end_date, start_date, units = "min")]  # ?difftime

batrips[, .(mean_durn = mean(duration), last_ride = max(end_date))]


#----- Chapter 3 -----
batrips[ , .N, by = "start_station"]
batrips[ , .N, by = .(start_station)]
batrips[, .(mean_duration = mean(duration)), by = "start_station"]
batrips[, .(mean_duration = mean(duration)), by = .(start_station, end_station)]

batrips[, .(mean_duration = mean(duration)), by = .(start_station, month(start_date))]
batrips[, .(mean_duration = mean(duration), total_trips = .N ), by = .(start_station, end_station)]
batrips[, .(min_duration = min(duration), max_duration = max(duration) ), by = .(start_station, end_station, month(start_date))]

# order by 
batrips[, .N, by = .(start_station, 
                     end_station)][order(-N)]

batrips[, .N, by = end_station][order(-N)][1:5]  # top 5 destinations

batrips[, .(start_date = start_date[c(1, .N)]), by = start_station]  # first and last ride for each start_station - this one seems weird.  I would pull them as 2 cols.

# .SD() is "Subset of Data"
batrips[, .SD[which.min(duration)], by = month(start_date), .SDcols = c("start_station", "end_station", "start_date", "end_date", "duration")]

batrips[, lapply(.SD, uniqueN), by = month(start_date), .SDcols = c("start_station", "zip_code")]  # total number of unique start stations and zip codes per month

#----- Chapter 4 -----
# := creates/updates by reference
ncol(batrips)
batrips[, duration_mean := mean(duration), by = .(start_station, end_station)]  # creates new column
ncol(batrips)

batrips[duration_mean %between% c(1000, 1020), unique(duration_mean), by = .(start_station, end_station)][order(-V1)]

batrips[, duration_mean, by = .(start_station, end_station)][order(duration_mean)]
batrips[, duration_mean := NULL] #delete the column

# in `:=`() functional form.  You have to wrap := in back-ticks since : is not a valid function name letter.
batrips[, `:=`(is_dur_gt_1hour = duration > 3600, week_day = wday(start_date))]


batrips[, trip_category := {
    med_dur = median(duration, na.rm = TRUE)
    if (med_dur < 600) "short"
    else if (med_dur >= 600 & med_dur <= 1800) "medium"
    else "long"
  },
  by = .(start_station, end_station)]
batrips[1:3, ]

# get min duration for each start/end station above 500
batrips[duration > 500, min_dur_gt_500 := min(duration), by = .(start_station, end_station)]
batrips[1:5]

# two ways to add multiple columns.  1st is `:=` functional form.  2nd is <char_vector> := <list>
ncol(batrips)
batrips[, `:=`(mean_duration = NULL, median_duration = NULL)]
batrips[, `:=`(mean_duration = as.integer(mean(duration)), median_duration = as.integer(median(duration))) , by = start_station]

#2nd way to add multiple cols
batrips[, c("mean_duration", "median_duration") := list(as.integer(mean(duration)), as.integer(median(duration))), by = start_station]


#----- Chapter 5 -----
# fread() is multi-threaded.  Dates and datetimes are read as character columns, but can be converted later (fasttime or anytime packages)
# fread() can import from local file, URL, or string
# skip = 9
# nrows = 999 to limit number of rows imported
# select = c("a", "b")  to limit which columns are imported.  If you need all but a few cols, use drop = c("a", "b") to get all but those cols.
# na.strings = c("#N/A", "-", "")  specify what values should be translated to NA
?fread
d <- fread("a,b\n1,2\n3,4", verbose = TRUE)
d

c("foo", "bar", "ray")
list(foo, bar, ray)
.(foo, bar, ray)

comics <- fread("C:/GitHub/r-sandbox01/DataCamp/data/comics.csv")
saveRDS(comics, "C:/GitHub/r-sandbox01/DataCamp/data/comics.rds")
comics[appearances > 1000, .(name, id, align, appearances, first_appear, publisher)][order(-appearances)]


?fwrite
# dateTimeAs "ISO", "squash", "epoch".  Squash is ISO with hyphens/colons removed.  Epoch is days/seconds since 1970-01-01


