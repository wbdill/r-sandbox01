# DataCamp: Joining data with data.table
# https://campus.datacamp.com/courses/joining-data-in-r-with-datatable/
library(data.table)
rm(list = ls())
setwd("C:/GitHub/r-sandbox01/DataCamp/data/joining")
netflix <- fread("netflix_2017.csv")
imdb <- fread("imdb_ratings.csv")
#----- Chapter 1: JOINS using merge()  -----

tables()  # shows tables in current session.  # rows, # cols and space MB and col names

# merge()  all = TRUE makes a full outer JOIN
merge(netflix, imdb, by = "title")                        # inner join
merge(netflix, imdb, by = "title", all = TRUE)            # full outer join
merge(x = netflix, y = imdb, by = "title", all.x = TRUE)  # left outer join


area <- fread("australia_area.csv")
capitals <- fread("australia_capitals.csv")
population <- fread("australia_cities_top20.csv")
tables()

#----- Chapter 2: JOINS using data.table syntax -----

# T1[T2, on = .(colname)]  is T1 right outer join T2 on colname
# T1[T2, on = .(colname), nomatch = 0]  performs inner join
# can't do full join with this syntax.  Must use merge()
# T1[!T2, on = .(colname)] #antijoin.  All of T1 that is NOT in T2

capitals[population, on = .(city)]                       # right join pop to cap using data.tables syntax
merge(capitals, population, by = "city", all.y = TRUE)   # right join pop to cap using merge()
capitals[population, on = "city", nomatch = 0]           # Inner join with the data.table syntax
population[!capitals, on = "city"]                       # Anti-join capitals to population

tblA[tblB, on = c("tblA_keycol","tblB_keycol")]          # tblA RIGHT OUTER JOIN tblB on cols with diff names in each
tblA[tblB, on = .(tblA_keycol,tblB_keycol)]              # tblA RIGHT OUTER JOIN tblB on cols with diff names in each

# setkey(DT, key1, key2, ....)  setkey lets you join without having to specify they key(s)
tables()

setkey(netflix, title)             # Set the keys
setkey(imdb, title)
key(imdb)
netflix[imdb, nomatch = 0]      # Inner join

#----- Chapter 3: Diagnosing and fixing common join problems -----

?merge

locations[, semester := as.integer(semester)]

# Right join on two columns (same name in each table)
subjects[semester, on = .(semester, subject)]

# use merge() suffixes argument to uniquely name columns in tables so you don't get funky suffixes when both tables have non-key cols with same name
merge(capital_pop, area, by = "state", all.x = TRUE, suffixes = c(".pop", ".area"))
# rather than percent.x and percent.y, you get percent.pop and percent.area

# can also use setnames() to rename cols before or after the join

# use as.data.table(DF, keep.rownames = TRUE) to convert dataframe DF to data.table


# Keep only the last probe for each gene
heart_3 <- unique(heart_2, by = "ilmn_probe", fromLast = TRUE)
cardio_3 <- unique(cardio_2, by = "affy_probe", fromLast = TRUE)

# Inner join
reproducible <- merge(heart_3, cardio_3, by = "gene", suffixes = c(".heart", ".cardio"))
reproducible

#----- Chapter 4: Concatenating and reshaping data.tables -----

# concatenate multiple tables similar to UNION with rbind()
rbind("2015" = sales_2015, "2016" = sales_2016, idcol = "year")
# fill = TRUE allows you to rbind() two tables where one has more cols than the other

# all in one import without intermediate variables:
table_files <- c("sales_2015.csv", "sales_2016.csv")
list_of_tables <- lapply(table_files, fread)
rbindlist(list_of_tables)

# use.names = TRUE lines the cols up with name (useful if colorder isn't identical in source tables)
# use.names = FALSE lines the cols up in order (useful if cols are aligned in source tables, but have different names)



# Get all countries in either Asia or Europe (gdp is a list of data.tables - one for each continent)
funion(gdp$asia, gdp$europe)
# Concatenate all data tables
gdp_all <- rbindlist(gdp)
# Print all unique countries
unique(gdp_all)

# melt() is similar to tidyverse's gather()
melt(sales_wide, measure.vars = c("2015", "2016"), variable.name = "year", value.name = "amount")
melt(sales_wide, id.vars = "quarter", variable.name = "year", value.anme = "amount")
# you can use measure.vars and id.vars at the same time.  Any columns not in either are dropped in the result.

# dcast is similar to tidyverse's spread()
dcast(DT, ids ~ group, value.var = "values")
dcast(sales_long, quarter ~ year, value.var = "amount")

# can split multiple value columns
dcast(profit_long, quarter ~ year, value.var = c("revenue", "profit"))
# returns quarter, revenue_2015, revenue_2016, profit_2015, profit_2016

# can keep multiple cols as row identifiers with +
dcast(sales_long, quarter + season ~ year, value.var = "amount")

# can split on multiple group columns
dcast(sales_long, quarter ~ department + year, value.var = "amount")
# returns quarter, retail_2015, retail_2016, consulting_2015, consulting_2016