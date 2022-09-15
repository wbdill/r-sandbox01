# 2022 Wyoming House Republican Primary results by county
# Comparing counties
library(tidyverse)
library(tidycensus)

#vars <- load_variables(2020, "acs5/subject", cache = TRUE)
View(vars)

#----- Census educational attainment data ---
edu <- get_acs(geography = "county",
        state = "WY",
        variables = c(bachelor_up_25_up = "S1501_C01_015",  # bachelor or higher age 25 or higher
                      hs_up_25_up       = "S1501_C01_014",
                      pop_25_up         = "S1501_C01_006",  # population age 25 or higher
                      ), 
        
                
        survey = "acs5",
        year = 2020,
        output = "wide",
        cache_table = TRUE)

edu <- edu %>% 
  mutate(pct_bach_up = bachelor_up_25_upE  * 100 / pop_25_upE,
         county = str_remove(NAME, " County, Wyoming"))
edu

#--- WY 2022 Republican House primary election results by county
# https://sos.wyo.gov/Elections/Docs/2022/Results/Primary/2022_Statewide_Candidates_Summary.pdf
wy <- tibble::tribble(
              ~county, ~Robyn.M..Belinskey, ~Anthony.Bouchard, ~Liz.Cheney, ~Harriet.Hageman, ~Denton.Knapp, ~`Write-Ins`, ~Overvotes, ~Undervotes,
             "Albany",                 40L,              109L,       4218L,            3967L,           65L,           9L,        13L,         32L,
           "Big Horn",                 78L,              123L,        732L,            3123L,           48L,           5L,        13L,         28L,
           "Campbell",                 71L,              346L,       1633L,            9164L,          789L,           5L,        65L,         76L,
             "Carbon",                 38L,              173L,        819L,            2852L,           49L,           6L,        23L,         33L,
           "Converse",                 45L,              164L,        822L,            3829L,           45L,           1L,        23L,         23L,
              "Crook",                 31L,               98L,        403L,            2370L,           55L,           2L,        20L,         22L,
            "Fremont",                 46L,              260L,       3458L,            7380L,           77L,           9L,        23L,         41L,
             "Goshen",                 17L,               96L,        958L,            3356L,           29L,           1L,         3L,         12L,
        "Hot Springs",                 10L,               58L,        383L,            1440L,           15L,           1L,         7L,          9L,
            "Johnson",                 45L,              123L,        890L,            2738L,           38L,           3L,        16L,         28L,
            "Laramie",                179L,              772L,       9757L,           14424L,          208L,          44L,        22L,         99L,
            "Lincoln",                 75L,              212L,       1221L,            4886L,           62L,           5L,        27L,         32L,
            "Natrona",                113L,              393L,       6511L,           14200L,          157L,          13L,        46L,         56L,
           "Niobrara",                  9L,               25L,        113L,             908L,           10L,           0L,         4L,          6L,
               "Park",                 71L,              197L,       2821L,            8672L,          101L,          16L,        34L,         39L,
             "Platte",                 19L,              104L,        535L,            2777L,           25L,           3L,         9L,         14L,
           "Sheridan",                118L,              228L,       3057L,            7366L,          113L,          13L,        42L,         59L,
           "Sublette",                 18L,               95L,        849L,            2538L,           32L,           8L,        15L,         28L,
         "Sweetwater",                132L,              449L,       2162L,            6722L,          176L,          12L,        51L,        103L,
              "Teton",                 15L,               34L,       5955L,            1928L,           13L,           4L,         9L,         20L,
              "Uinta",                 72L,              215L,       1156L,            4030L,           84L,           7L,        36L,         55L,
           "Washakie",                 30L,              114L,        531L,            2213L,           31L,           8L,        18L,         15L,
             "Weston",                 33L,              117L,        332L,            2142L,           36L,           0L,        16L,         15L
        )

wy <- janitor::clean_names(wy)

wy <- wy %>% 
  mutate(tot_votes = rowSums(select(., -1)),  # sum of row values
         pct_cheney = liz_cheney * 100 / tot_votes,
         pct_hageman = harriet_hageman * 100 / tot_votes,
         winner = case_when (pct_cheney > pct_hageman ~ "Cheney",
                             pct_cheney < pct_hageman ~ "Hageman"))  

joined <- wy %>% 
  inner_join(edu) %>% 
  select(county, tot_votes, pct_cheney, pct_hageman, winner, pct_bach_up, pop_25_up = pop_25_upE)

joined %>% 
  ggplot(aes(x = pct_bach_up, y = pct_cheney, label = county)) +
  geom_point(aes(size = tot_votes), alpha = 0.5) +
  geom_smooth(method = "lm") +
  geom_text(check_overlap = TRUE, nudge_x = .8, nudge_y = -1.1, size = 3) +
  labs(title = "2022 WY Republican Primary",
       subtitle = "Cheney won Teton and Albany counties",
       x = "% of county (age 25 or up) with a bachelor's degree or higher",
       y = "% of county that voted for Liz Cheney",
       size = "Total Votes",
       caption = "Chart: @bdill\nEducation Data: US Census Burear ACS 5 year\nElection Data: sos.wyo.gov")


write_csv(joined, "C:/users/bdill/Desktop/2022_WY_Republican_primary_data.csv")

joined %>% arrange(desc(tot_votes))
#--- 
ggplot(joined, aes(x = pop_25_up, y = pct_bach_up)) +
  geom_point()

#--- linear model

mod <- lm(pct_cheney ~ pct_bach_up, data = joined)
summary(mod)

#-----
wy_long <- pivot_longer(wy,cols = 2:9, names_to = "candidate", values_to = "votes")


# "liz_cheney", "harriet_hageman"
wy_long %>% 
  filter(candidate %in% c("liz_cheney", "harriet_hageman")) %>% 
  ggplot(aes(x = county, y = votes, group = candidate, color = candidate)) +
  geom_bar(aes(fill = candidate),position = 'dodge',stat='identity') +
  theme(axis.text.x = element_text(angle = 90, vjust = .2)) +
  labs(title = "2022 WY Republican Primary",
       subtitle = "Cheney won Teton and Albany counties",
       caption = "Chart: @bdill\nElection Data: sos.wyo.gov")



