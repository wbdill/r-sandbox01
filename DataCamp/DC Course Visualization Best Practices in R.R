# https://campus.datacamp.com/courses/visualization-best-practices-in-r/proportions-of-a-whole?ex=1
install.packages("waffle")
library(data.table)
library(tidyverse)
library(waffle)

who_disease <- fread("C:/GitHub/r-sandbox01/DataCamp/data/who_disease.csv")
glimpse(who_disease)
#========== Chapter 1: Proportions of a whole ==========
ggplot(who_disease, aes(x=region)) +
  geom_bar()

# filter data to AMR region. 
amr_region <- who_disease %>%
  filter(region == "AMR")

# map x to year and y to cases.  # lower alpha to 0.5 to see overlap. 
ggplot(amr_region, aes(x=year, y=cases)) + 
  geom_point(alpha = 0.5)


# pie chart
disease_counts <- who_disease %>%
  mutate(disease = ifelse(disease %in% c('measles', 'mumps'), disease, 'other')) %>%
  group_by(disease) %>%
  summarise(total_cases = sum(cases))

ggplot(disease_counts, aes(x = 1, y = total_cases, fill = disease)) +
  geom_col() +
  theme_void() +
  coord_polar(theta = 'y') +
  labs(title = "Proportions of diseases")

# waffle chart (waffle library)
disease_counts <- who_disease %>%
  group_by(disease) %>%
  summarise(total_cases = sum(cases)) %>% 
  mutate(percent = round(total_cases/sum(total_cases)*100))

case_counts <- disease_counts$percent
names(case_counts) <- disease_counts$disease
waffle(case_counts)


disease_counts <- who_disease %>%
  mutate(disease = ifelse(disease %in% c('measles', 'mumps'), disease, 'other')) %>%
  group_by(disease, year) %>% # note the addition of year to the grouping.
  summarise(total_cases = sum(cases))
ggplot(disease_counts, aes(x = year, y = total_cases, fill = disease)) +
  geom_col(position = "fill")

disease_counts <- who_disease %>%
  mutate(
    disease = ifelse(disease %in% c('measles', 'mumps'), disease, 'other') %>% 
      factor(levels = c('measles', 'other', 'mumps')) 
  ) %>%
  group_by(disease, year) %>%
  summarise(total_cases = sum(cases)) 

# plot
ggplot(disease_counts, aes(x = year, y = total_cases, fill = disease)) +
  geom_col(position = 'fill')


disease_counts <- who_disease %>%
  filter(year >= 1999) %>%  # Filter to on or later than 1999
  mutate(disease = ifelse(disease %in% c('measles', 'mumps'), disease, 'other')) %>%
  group_by(disease, region) %>%    
  summarise(total_cases = sum(cases))

# Set aesthetics so disease is the stacking variable, region is the x-axis and counts are the y
ggplot(disease_counts, aes(x = region, y = total_cases, fill = disease)) +
  geom_col(position = "fill")

#========== Chapter 2: Point data ==========
who_disease %>% 
  filter(country == 'India', year == 1980) %>% 
  ggplot(aes(x = disease, y = cases)) +
  geom_col()  #geom_col expects a y-axis, but geom_bar() does not.

who_disease %>%
    filter(cases > 1000) %>%
    ggplot(aes(x = region)) +
    geom_bar()  # add a geom_bar call

interestingCountries <- c("NGA", "SDN", "FRA", "NPL", "MYS", "TZA", "YEM", "UKR", "BGD", "VNM")
who_subset <- who_disease %>%
  filter(countryCode %in% interestingCountries,
         disease == 'measles',
         year %in% c(1992, 2002)
         ) %>%
  mutate(year = paste0('cases_', year)) %>%
  spread(year, cases)
ggplot(who_subset, aes(x = log10(cases_1992), y = reorder(country, cases_1992))) +
  geom_point()

who_subset %>% 
  mutate(logFoldChange = log2(cases_2002/cases_1992)) %>% 
  ggplot(aes(x = logFoldChange, y = reorder(country, logFoldChange))) +
     geom_point() +
     geom_vline(xintercept = 0) +
     xlim(-6,6) +
     facet_grid(region~., scale='free_y')

#tuning the charts
amr_pertussis <- who_disease %>% 
  filter(   # filter data to our desired subset
    region == 'AMR', 
    year == 1980, 
    disease == 'pertussis'
  )
# Set x axis as country ordered with respect to cases. 
ggplot(amr_pertussis, aes(x = reorder(country, cases), y = cases)) +
  geom_col() +
  coord_flip()

# filter out zero cases and get rid of major gridlines w/ theme
amr_pertussis %>%
  filter(cases > 0) %>%
ggplot(aes(x = reorder(country, cases), y = cases)) +
  geom_col() +
  coord_flip() + 
  theme(
    panel.grid.major.y = element_blank()
  )

amr_pertussis %>% filter(cases > 0) %>% 
  ggplot(aes(x = reorder(country, cases), y = cases)) + 
  geom_point(size=2) + 
  scale_y_log10() +  # change y-axis to log10. 
  theme_minimal() +  # add theme_minimal()
  coord_flip()

#========== Chapter 3: Single Distributions ==========
md_speeding <- fread("D:/Downloads/Traffic_Violations.csv")

md_speeding <- md_speeding %>%
  filter(str_detect(md_speeding$`Date Of Stop`, "2017$") > 0) 

# not the same data as in the video, so these won't run.
md_speeding %>%
  select('Date Of Stop', `Work Zone`, VehicleType, Color, Race, Gender, `Driver State`) %>%
  head()
glimpse(md_speeding)

ggplot(md_speeding) + 
  geom_histogram(
      aes(x = speed_over),
      alpha = 0.7
  ) +
  theme_minimal()

ggplot(md_speeding) +
  geom_histogram(
    aes(x = hour_of_day, y = stat(density)),
    alpha = 0.8
  )


ggplot(md_speeding) +
  geom_histogram(
    aes(x = percentage_over_limit),
    bins = 100,     # set bin number to 40
    fill = 'steelblue',
    alpha = 0.8)    # reduce alpha to 0.8

ggplot(md_speeding,aes(x = hour_of_day)) +
  geom_histogram(
    binwidth = 1,  # set binwidth to 1
    center = 0.5  # Center bins at the half (0.5) hour
  ) +
  scale_x_continuous(breaks = 0:24)
#========== Chapter 4: Comparing Distributions ==========

