# US Congress Bioguide Profiles
# @bdill 2022-10-06
library(tidyverse)
library(fs)


#----- single member pull from congress.gov) -----
# member <- jsonlite::fromJSON("https://bioguide.congress.gov/search/bio/B001243.json")
# member <- member[[1]]
# names(member)
# member$usCongressBioId
# member$familyName
# member$givenName
# member$birthDate
# member$deathDate
# member$jobPositions
# jobs <- member$jobPositions
# str(jobs)
# 
# dt <- tibble(
#     bioid = member$usCongressBioId,
#     last_name = member$unaccentedFamilyName,
#     first_name = member$unaccentedGivenName,
#     dob = member$birthDate,
#     job = jobs$job$name,
#     congress = jobs$congressAffiliation$congress$congressNumber,
#     party = data.table::as.data.table(jobs$congressAffiliation$partyAffiliation) |> pivot_longer(cols = starts_with("party")) |> pull(value),
#     #caucus = data.table::as.data.table(jobs$congressAffiliation$caucusAffiliation) |> pivot_longer(cols = starts_with("party")) |> pull(value),
#     state = jobs$congressAffiliation$represents$regionCode
#   )
# str(dt)
# dt |> arrange(bioid, congress)


#----- US Congresses -----
# congs <- read_csv("D:/opendata/USCongress/us_congresses.csv")
# congs <- congs |> 
#   mutate(began = lubridate::mdy(CongressBegan),
#          ended = lubridate::mdy(CongressEnded))
# congs |> select(congress_number = CongressNumber, began, ended) |> 
#   write_csv("D:/opendata/USCongress/us_congresses_iso.csv")
#
#congs <- read_csv("D:/opendata/USCongress/us_congresses_iso.csv")


#----- Loop through local json files -----
library(fs)

# https://bioguide.congress.gov/search (Download -> DOWNLOAD BULK DATA IN JSON FORMAT) zip of 12k+ json files
basepath <- "D:/opendata/USCongress/BioguideProfiles/"  # need trailing slash.  
file_paths <- fs::dir_ls(basepath, type = "file", regexp = ".json$") # only .json files
file_contents <- list()  # init a blank list

for (i in seq_along(file_paths)) {
  member <- jsonlite::fromJSON(file_paths[[i]])
  #member <- jsonlite::fromJSON("D:/opendata/USCongress/BioguideProfiles/A000029.json") # A000029.json has problematic jobPositions
  jobs <- member$jobPositions
  party <- ""

    tryCatch(               
      expr = {                     
        party = data.table::as.data.table(jobs$congressAffiliation$partyAffiliation) |> pivot_longer(cols = starts_with("party")) |> 
          pull(value)
        if( length(jobs$congressAffiliation$partyAffiliation) != length(party)) {
          party = ""  # if the party array != partyAffiliation array, then set it to blank.  Otherwise it won't build the dt
        }
      },
      error = function(e){         
        print(paste("There was an error message with:", member$usCongressBioId))
        #print(member$usCongressBioId)
      },
      warning = function(w){      
        print("There was a warning message.")
      },
      finally = {
      }
  )

  dt <- tibble(
    bioid = member$usCongressBioId,
    last_name = member$unaccentedFamilyName,
    first_name = member$unaccentedGivenName,
    dob = member$birthDate,
    dod = member$deathDate,
    job = jobs$job$name,
    congress = jobs$congressAffiliation$congress$congressNumber,
    party = party,
    #caucus = data.table::as.data.table(jobs$congressAffiliation$caucusAffiliation) |> pivot_longer(cols = starts_with("party")) |> pull(value),
    state = jobs$congressAffiliation$represents$regionCode
  )
  file_contents[[i]] <- dt
} # end for loop

members <- data.table::rbindlist(file_contents, fill = TRUE)
#members |> filter(bioid %in% c("A000029","A000045","A000049"))

congs <- read_csv("D:/opendata/USCongress/US_Congresses_iso.csv")
members <- members |> left_join(congs, by = c("congress" = "congress_number")) |> 
  mutate(approx_age = lubridate::year(began) - as.integer(substr(dob, 1, 4))) 

write_csv(members, "D:/opendata/USCongress/US_Congress_bio_profiles.csv", na = "")


#----- EDA
members |> filter(bioid == "P000197") |> arrange(congress)

cong117 <- members |> filter(congress == 117 & is.na(dod) & job %in% c("Representative", "Senator", "Delegate") ) |> select(bioid)
members |> 
  inner_join(cong117) |> 
  filter(job %in% c("Representative", "Senator", "Delegate") & is.na(dod)) |> 
  group_by(bioid, last_name, first_name, dob, job, party, state) |> 
  summarize(first_cong = min(congress)
            , last_cong = max(congress)
            , terms_served = n(),
            , years_served = 2 * (last_cong - first_cong + 1)
            , approx_age = (2022 - as.integer(substr( max(dob), 1, 4))) 
            ) |> 
  View()
  
  select( bioid, last_name, first_name, dob, dod, job, congress, party, state, began, approx_age) |> 
  head(20)
