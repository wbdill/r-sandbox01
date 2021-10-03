

f <- tribble(
  ~top_n,
  5,
  8,
  10
  )

for (x in f$top_n) {
  rmarkdown::render('C:/GitHub/r-sandbox01/govt_data/census_2020_race.Rmd', 
                    output_file = paste0("C:/GitHub/r-sandbox01/govt_data/output/census2020_", x, ".docx"),
                    params = list(top_n = x)
                    )
}
