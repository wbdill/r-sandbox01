

library(tidyverse)
rm(list=ls())
# https://data.census.gov/cedsci/table?q=school%20enrollment&g=0100000US.050000&tid=ACSDT1Y2019.B14001&hidePreview=true&moe=false&tp=true
sch <- read_csv("D:/opendata/census.gov/school_pop_2019/ACSDT5Y2019.B14001_data_with_overlays_2021-07-27T235831.csv")

sch <- sch[2:nrow(sch),]  # chop off 1st data row

names(sch) <-  c("geoid"
              , "county"
              , "total"
              , "total_moe"
              , "enr_in_school"
              , "enr_in_school_moe"
              , "enr_PK"
              , "enr_PK_moe"
              , "enr_KG"
              , "enr_KG_moe"
              , "enr_G1G4"
              , "enr_G1G4_moe"
              , "enr_G5G8"
              , "enr_G5G8_moe"
              , "enr_G9G12"
              , "enr_G9G12_moe"
              , "enr_college"
              , "enr_college_moe"
              , "enr_grad_sch"
              , "enr_grad_sch_moe"
              , "not_enr"
              , "not_enr_moe")

sch$county <- str_replace(sch$county, " County", "")


# change most columns to numeric
sch <- sch  %>% mutate_at(vars(matches("enr")), as.numeric) %>% 
  mutate(total = as.numeric(total))

sch <- sch %>% separate(col = county, into = c("county", "state"), sep = ", ") %>% 
  mutate(fips_county = substr(geoid, 10, 99),
         fips_state = substr(fips_county, 1, 2)) %>% 
  select(geoid, fips_state, fips_county, state, county, total
          , "enr_in_school"
          , "not_enr" 
          , "enr_PK"
          , "enr_KG"
          , "enr_G1G4"
          , "enr_G5G8"
          , "enr_G9G12"
          , "enr_college"
          , "enr_grad_sch"
          , "enr_in_school_moe"
          , "not_enr_moe" 
          , "enr_PK_moe"
          , "enr_KG_moe"
          , "enr_G1G4_moe"
          , "enr_G5G8_moe"
          , "enr_G9G12_moe"
          , "enr_college_moe"
          , "enr_grad_sch_moe")
write_csv(sch, "D:/opendata/census.gov/school_pop_2019/census_school_pop_2019.csv")


#----- Stacked bar chart with controlled stack order and x-axis order
plot_state <- function(x) {
  state1 <- x$state
  if(sch %>% filter(state == state1) %>% count() > 0 ) {
    

    sch %>% filter(state == state1) %>% 
      mutate(pct_enroll = enr_in_school * 100 / total  # create in specific order to use fct_inorder later
             , pct_PK = enr_PK * 100 / total
             , pct_KG = enr_KG * 100 / total
             , pct_G1_4 = enr_G1G4 * 100 / total
             , pct_G5_8 = enr_G5G8 * 100 / total
             , pct_G9_12 = enr_G9G12 * 100 / total
             , pct_College = enr_college * 100 / total
             , pct_Graduate = enr_grad_sch * 100 / total
             ) %>% 
      select(county, starts_with("pct")) %>% 
      mutate(county = forcats::fct_reorder(county, desc(pct_enroll))) %>%                     # control x-axis plot order
      select(-pct_enroll) %>% 
      pivot_longer(starts_with("pct"), names_to = "Group", values_to = "pct", names_prefix = "pct_") %>% 
      ggplot(aes(x = county, y = pct, group = fct_rev(fct_inorder(Group)))) +                 # control order of legend
      geom_bar(aes(fill = fct_rev(fct_inorder(Group))), position="stack", stat="identity") +  # control order of stack
      theme(axis.text.x = element_text(angle = 90)) +
      labs(title ="School Enrollment by County",
           subtitle = state1,
           x = "County",
           y = "Enrolled in School %",
           fill = "Edu Cohort",
           caption = "Data: Census table B14001 (2019)\nChart: @bdill"
           )
    filename <- paste0("D:/opendata/census.gov/school_pop_2019/output/sch_enroll_", state1, ".png")   
    ggsave(filename = filename, width = 24, height = 14, units = "cm", dpi=200)
  }
}

states <- read_csv("D:/opendata/FIPS_state_usda.csv")
by(states, 1:nrow(states), plot_state)

