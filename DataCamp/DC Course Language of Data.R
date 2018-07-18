# Data Camp - Language of Data course
# https://campus.datacamp.com/courses/introduction-to-data/language-of-data?ex=1

rm(list = ls())

# Types of varaibles
#  Numerical (quantitavie)
#  - Continuous - infinite values (often measured)
#  _ Discrete: specific set of numeric values (often counted)
#  Categorical (qualitative) limited number of distinct categories
#  - Ordinal: finite # of values w/in a given range (often measured) with an inherent order.
#             Ex: low, medium, high
#  - Categorical: non-orderd.  Ex: race, sex, etc.


tbl$col <- droplevels(tbl$col) #drop unused levels - prevents problems in visualizations

# wrap parens around an assignment to both 1) Do the assignment 2) see the value
(avg_score <- mean(evals$score))

evals <- evals %>%
  mutate(score_cat = ifelse(score < avg_score, "below average", "at or above average"))

#*****************************************************************************
#----- UCB admit exercises-----
load("C:/data/R/DataCamp/data/ucb_admit.RData")
glimpse(ucb_admit)

table(ucb_admit$Gender)
table(ucb_admit$Dept)
table(ucb_admit[,2:3])
table(ucb_admit[,1:2])

ucb_admission_counts <- ucb_admit %>% count(Gender, Admit)

ucb_admission_counts %>%
  ggplot(aes(Admit, n, fill = Gender)) +
  geom_col()

ucb_admission_counts %>%
  group_by(Gender) %>%
  mutate(prop = n / sum(n)) %>%
  filter(Admit == "Admitted")

ucb_admission_counts <- ucb_admit %>% count(Gender, Admit, Dept)

ucb_admission_counts %>%
  group_by(Dept, Gender) %>%
  mutate(prop = n / sum(n)) %>%
  filter(Gender == "Male", Admit == "Admitted")

ucb_admission_counts %>%
  ggplot(aes(Dept, n, color = Gender, fill = Admit)) +
  geom_col(position = "dodge", alpha = 0.9)



#*****************************************************************************
#----- Sampling -----
install.packages("openintro")
library(openintro)
library(dplyr)
data("county")
glimpse(county)

tn_counties <- county %>%
  filter(state == "Tennessee") %>%
  select(name, pop2010, poverty, income, med_income) %>%
  arrange(desc(med_income)) %>%
  mutate(rownum = row_number())

ggplot(tn_counties, aes(rownum, med_income)) +
  geom_line() +
  scale_x_continuous(breaks = scales::pretty_breaks(n = 10)) +
  ylim(0,max(tn_counties$med_income)) +
  labs(x = "County Rank", y = "Median Income", title = "TN Median Income by County")

#*****************************************************************************
#----- Regions -----
load("C:/data/R/DataCamp/data/us_regions.RData")
glimpse(us_regions)

# random sample
states_srs <- us_regions %>% sample_n(8)
states_srs %>% count(region)

# stratefied sample
states_str <- us_regions %>%
  group_by(region) %>%
  sample_n(2)

states_str %>%
  count(region)

#*****************************************************************************
# Principles of experimental design
# Control: compare treatments of interest to a control group.
# Randomize: randomly assing subjects to treatments
# Replicate: collect a sufficiently large sample w/in a study, or replicate entire study
# Block: account for potential effect of confounding variables
#    - Group subjects into blocks based on these variables
#    - Randomize within each block to treatment groups

#*****************************************************************************
#----- Beauty in the classroom -----
load("C:/data/R/DataCamp/data/evals.RData")
glimpse(evals)

cat_vars <- c("rank", "ethnicity", "gender", "language", 
              "cls_level", "cls_profs", "cls_credits",
              "pic_outfit", "pic_color")
# Recode cls_students as cls_type
evals <- evals %>%
  mutate(cls_type = case_when(
    cls_students <= 18 ~ "small",
    cls_students <= 59 ~ "midsize",
    cls_students >= 60 ~ "large"
    )
  )
ggplot(evals, aes(bty_avg, score)) +
  geom_jitter()

ggplot(evals, aes(bty_avg, score, col = cls_type)) +
  geom_jitter() +
  geom_smooth(se = FALSE)
