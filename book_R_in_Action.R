rm(list=ls())

#----- Ch 1 -----
demo(colors)
demo(image)
demo(plotmath)

demo()
data()
vignette()

library(ggplot2)
vignette("ggplot2-specs")

help.start()

# running script via command line batch
#  "C:\Program Files\R\R-3.1.0\bin\R.exe" CMD BATCH --vanilla --slave "c:\my projects\myscript.R"

install.packages("vcd")
library(vcd)
help(package="vcd")
data()
help(Arthritis)
Arthritis
example("Arthritis")

data("Arthritis")
art <- xtabs(~ Treatment + Improved, data = Arthritis, subset = Sex == "Female")
art

mosaic(art, gp = shading_max)

