

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
