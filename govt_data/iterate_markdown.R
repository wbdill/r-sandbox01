

params_table <- tribble(
  ~top_n,
  5,
  10,
  13,
  20
  )
params_table
for (x in params_table$top_n) {
  rmarkdown::render('C:/GitHub/r-sandbox01/govt_data/census_2020_race.Rmd', 
                    output_file = paste0("C:/GitHub/r-sandbox01/govt_data/output/census2020_", x, ".docx"),
                    params = list(top_n = x)
                    )
}



library(tidyverse)
reports <- tibble(
  tp = c(5,10,15),
  bar = c(90, 95, 100),
  filename = paste0("C:/GitHub/r-sandbox01/govt_data/output/census2020_", tp, ".docx"),
  params = map(tp,~ list(top_n = .x))
  #params = map( tp, bar, ~ list(top_n = .x, foo = .y))
)

reports %>% 
  select(output_file = filename, params) %>% 
  pwalk(rmarkdown::render, input = "C:/GitHub/r-sandbox01/govt_data/census_2020_race.Rmd")

?map
