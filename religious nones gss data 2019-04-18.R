#----- Religious Nones - GSS data 1982 - 2018
# https://t.co/3tBqjPBXeb  dta download url
# D:\Downloads\gss_full.dta
#rm(list=ls())

library(tidyverse)

gss = read_dta("D:/Downloads/gss_full.dta")

saveRDS(gss, "C:/Data/R/religous_nones_gss_data.rds")  # CYA save
# FYI, the *.dta file was 445 MB while the *.rds file was a mere 33 MB.  R compression FTW!

gss = readRDS("C:/Data/R/religous_nones_gss_data.rds")

#----- Get raw data of desired columns -----
datraw <- select(gss, year, evangelical, mainline, blackprot, catholic, jewish, otherfaith, nofaith, reltrad, oversamp, wtssall)
datraw %>% filter(year %in% c(1982, 1987) & blackprot == 1)
datraw %>% filter(!(year %in% c(1982, 1987) & blackprot == 1))
datraw %>% filter(!year %in% c(1982, 1987) & !blackprot == 1)

#----- counts by year -----
datcount <- gss %>%
  filter(!(year %in% c(1982, 1987) & blackprot == 1)) %>%
  group_by(year) %>%
  summarize(
    total = sum(evangelical + mainline + blackprot + catholic + jewish + otherfaith + nofaith),
    evangelical = sum(evangelical),
    mainline = sum(mainline) ,
    blackprot = sum(blackprot),
    catholic = sum(catholic) ,
    jewish = sum(jewish) ,
    otherfaith = sum(otherfaith),
    nofaith = sum(nofaith)
  )

#----- percents by year -----
datpercent <- gss %>%
  filter(!(year %in% c(1982, 1987) & blackprot == 1)) %>%
  group_by(year) %>%
  summarize(
    total = sum(evangelical + mainline + blackprot + catholic + jewish + otherfaith + nofaith),        
    evangelical = sum(evangelical) / total,
            mainline = sum(mainline) / total,
            blackprot = sum(blackprot) / total,
            catholic = sum(catholic) / total,
            jewish = sum(jewish) / total,
            otherfaith = sum(otherfaith) / total,
            nofaith = sum(nofaith) / total
         )

#----- save off data as TSV files -----
write.table(datraw, file = "C:/Data/R/religous_nones_gss_data_raw.tsv", sep = "\t", row.names = FALSE, quote = FALSE)
write.table(datcount, file = "C:/Data/R/religous_nones_gss_data_count.tsv", sep = "\t", row.names = FALSE, quote = FALSE)
write.table(datpercent, file = "C:/Data/R/religous_nones_gss_data_percent.tsv", sep = "\t", row.names = FALSE, quote = FALSE)

# tidy up the dataset for ggplot
tidydat <- gather(datpercent, key="denomination", value="count",  -year, -total)

ggplot(tidydat, aes(x = year, y = count)) +
  geom_line(aes(color=denomination))


#--------------------------------------------------
#----- Gist from Ryan Burge -----
#--------------------------------------------------
install.packages("devtools")
devtools::install_github("ryanburge/socsci")
library(devtools)
library(socsci)
over <- gss %>% 
  filter(year == 1982 | year == 1987) %>% 
  group_by(year) %>% 
  ct(reltrad, wt = oversamp)

## This is the weight for the rest of the sample ####
wtss <- gss %>%
  group_by(year) %>%
  ct(reltrad, wt = wtssall)

#Removing the two years that used the overweight ####
wtss <- wtss %>%
  filter(year != 1982) %>%
  filter(year != 1987)

## Bind them both together ####
graph <- bind_rows(over, wtss)

## Graph ####
graph %>%
  ggplot(., aes(x = year, y = pct, color = factor(reltrad))) +
  geom_point() +
  geom_line() +
  scale_y_continuous(labels = percent) +
  #theme_gg("Abel") +
  #scale_color_npg() +
  labs(x = "", y = "Percent of Population", title = "America's Changing Religious Landscape", caption = "Data: GSS 1972-2018") +
  annotate("text", x=1996, y = .30, label = "Evangelical", size = 4, family = "font") +
  annotate("text", x=1995.75, y = .225, label = "Catholic", size = 4, family = "font") +
  annotate("text", x=2012, y = .119, label = "Mainline", size = 4, family = "font") +
  annotate("text", x=2013, y = .175, label = "No Religion", size = 4, family = "font") +
  annotate("text", x=2006, y = .092, label = "Black Protestant", size = 4, family = "font") +
  annotate("text", x=1999.5, y = .068, label = "Other Faith", size = 4, family = "font") +
  annotate("text", x=2005, y = .0275, label = "Jewish", size = 4, family = "font") 
#  ggsave("D://cces/images/full_reltrad_gss.png", width = 7)
