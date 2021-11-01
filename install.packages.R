# https://www.tidyverse.org/
install.packages("tidyverse")
install.packages("stringi")

# https://www.tidytextmining.com/
install.packages("tidytext")    # text mining (has built-in lexicon dictionaries)

# https://cran.r-project.org/web/packages/syuzhet/index.html
install.packages("syuzhet")     # sentiment lexicons

# https://cran.r-project.org/web/packages/mosaic/index.html
install.packages("mosaic")      # stats pkg

# https://cran.r-project.org/web/packages/gutenbergr/index.html
install.packages("gutenbergr")  # project Gutenberg book downloads


# https://rmarkdown.rstudio.com/lesson-1.html
install.packages("formatR", repos = "http://cran.rstudio.com")


# https://www.stat.pitt.edu/stoffer/tsa4/xChanges.htm
# https://www.stat.pitt.edu/stoffer/tsa4/
# https://www.rdocumentation.org/packages/astsa/versions/1.8/topics/astsa-package
install.packages("devtools")     # only need to do this once
devtools::install_github("nickpoison/astsa")  # time series analysis


install.packages("ggThemeAssist") #addin
install.packages("esquisse") #addin
install.packages("plotly")  # interactive graphs

install.packages("kableExtra") # extension to knitr's kable https://cran.r-project.org/web/packages/kableExtra/vignettes/awesome_table_in_html.html

#https://www.tidyverse.org/blog/2019/05/vroom-1-0-0
install.packages("vroom")
