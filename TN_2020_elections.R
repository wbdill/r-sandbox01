# TN 2020 election
rm(list = ls())
install.packages("rvest")  # web scraping package
install.packages("xml2")
library(xml2)
library(rvest)
library(tidyverse)

test_url <- "https://www.elections.tn.gov/county-results.php?OfficeByCounty=United%20States%20President"
test_xml <- read_html(test_url)  # Read the URL stored as "test_url" with read_html()
test_xml  # Print test_xml


counties_html <- html_nodes(test_xml, css = ".election-result-table .results-header-row h3")
counties <- as.data.frame(html_text(counties_html))
names(counties)  <-  "county"

results_html <- html_nodes(test_xml, css = ".election-result-table tbody")


get_county <- function(county_name, node) {
  df1 <- as.data.frame(html_text(html_nodes(node, css = "th")))
  stxt <- html_text(html_nodes(node, css = "td"))
  
  df2 <- as.data.frame(matrix(stxt, ncol = 3 , byrow = TRUE))
  df2[,3] <- as.numeric( str_trim(str_replace(str_replace(df2[,3], "\n", ""), "%", "")) )   # strip % and remove whitespace
  df2[,2] <- as.numeric( str_trim(str_replace_all(df2[,2], ",", "")) )                      # strip comma
  
  df3 <- cbind(county_name, df1, df2)
  names(df3) <- c("county", "candidate", "party", "votes", "percent")
  df3
}

#get_county("ff", results_html[96])

df_master <- data.frame(row.names = c("county", "candidate", "votes", "percent"))
for(i in 1:95) {
  df_master <- rbind(df_master, get_county(counties[i,1], results_html[i]))
  #get_county(counties[i,1], results_html[i])
}

df_master %>% 
  group_by(candidate) %>% 
  summarise(votes = sum(votes)) %>% 
  arrange(desc(votes))

df_master %>% 
  filter(candidate == "Donald J. Trump") %>% 
  arrange(desc(percent)) %>% 
  select(county, votes, percent)



county_pop <- read_csv("https://pastebin.com/raw/jQYXHM8e")
county_pop_tn <- county_pop %>% 
  filter(state == "Tennessee") %>% 
  select(fips_code, county, pop_2018) %>% 
  mutate(county = str_remove(county, " County"))

df_vote_pct <- df_master %>% 
  select(county, party, votes) %>% 
  group_by(county, party) %>% 
  summarize(votes = sum(votes)) %>% 
  pivot_wider(names_from = party, values_from = votes) %>% 
  mutate(total_votes = Democratic + Independent + Republican,
         d_pct = round(Democratic * 100 / total_votes, 3),
         r_pct = round(Republican * 100 / total_votes, 3) ,
         i_pct = round(Independent * 100 / total_votes, 3) ) %>% 
  #arrange(desc(i_pct)) %>% 
  inner_join(county_pop_tn, by = "county") %>% 
  mutate(pct_that_voted = round(total_votes* 100 / pop_2018, 3) ) %>% 
  arrange(county)

write_csv(df_vote_pct, "C:/GitHub/r-sandbox01/output/tn_2020_presidential_election.csv")

df_vote_pct %>% 
  filter(county %in% c("Shelby", "Davidson", "Williamson", "Rutheford", "Maury", "Knox", "Wilson")) %>% 
  arrange(desc(d_pct)) %>% 
  pivot_longer(cols = c(d_pct, i_pct, r_pct), values_to = "val" ) %>% 
  select(county, name, val) %>% 
  ggplot(aes(x = county, y = val, fill = name)) +
  geom_col(position = "stack") +
  scale_fill_manual(values = c("#6666ff", "#66cc66", "#ff6666"))
  
# Kanye vote
df_master %>% 
  filter(candidate == "Kanye West") %>% 
  arrange(desc(votes)) %>% 
  inner_join(county_pop_tn, by = "county") %>% 
  top_n(20, votes) %>% 
  select(county, votes, pct_of_vote = percent, pop_2018) %>% 
  mutate(pct_of_pop = round(votes * 100 / pop_2018, 3))


df_vote_pct %>% 
  ggplot(aes(x = pop_2018, y = r_pct))+
  geom_point() +
  geom_smooth(method = "loess", se = F)+
  labs(title = "County Population vs Republican vote %",
       x = "County Population (2018)",
       y = "Republican Vote %")


#----- Ch 4 Web scraping with XPATHs -----
install.packages("rvest")  # web scraping package
install.packages("xml2")
library(xml2)
library(rvest)
# html_text()  html_attr() html_name() html_node() 
# html_table() extract html table to data frame
# data.frame() with vectors of text to put them into data frames

test_url <- "https://en.wikipedia.org/wiki/Hadley_Wickham"  # Hadley Wickham's Wikipedia page
test_url <- "https://www.elections.tn.gov/county-results.php?OfficeByCounty=United%20States%20President"
test_xml <- read_html(test_url)  # Read the URL stored as "test_url" with read_html()
test_xml  # Print test_xml

test_node_xpath <- "//*[contains(concat( \" \", @class, \" \" ), concat( \" \", \"vcard\", \" \" ))]"
test_node_xpath <- ""
node <- html_node(x = test_xml, xpath = test_node_xpath)  # Use html_node() to grab the node with the XPATH stored as `test_node_xpath`
node[1]  # Print the first element of the result

element_name <- html_name(x = node)  # Extract the name of table_element
element_name  # Print the name

table_element <- node
second_xpath_val <- "//*[contains(concat( \" \", @class, \" \" ), concat( \" \", \"fn\", \" \" ))]"
page_name <- html_node(x = table_element, xpath = second_xpath_val)  # Extract the element of table_element referred to by second_xpath_val and store it as page_name
page_title <- html_text(page_name)  # Extract the text from page_name
page_title  # Print page_title

wiki_table <- html_table(table_element)  # Turn table_element into a data frame and assign it to wiki_table
wiki_table  # Print wiki_table

colnames(wiki_table) <- c("key", "value")  # Rename the columns of wiki_table
cleaned_table <- subset(wiki_table, !key == "")  # Remove the empty row from wiki_table
cleaned_table  # Print cleaned_table


#----- Ch 5 CSS Web Scraping and Final Case Study -----
# use html_nodes() but with css argument

html_nodes(test_xml, css = "table")  # Select the table elements
html_nodes(test_xml, css = ".infobox")  # Select elements with class = "infobox"
html_nodes(test_xml, css = "#firstHeading")  # Select elements with id = "firstHeading"

infobox_element <- html_nodes(test_xml, css = ".infobox")  # Extract element with class infobox
element_name <- html_name(infobox_element)  # Get tag name of infobox_element
element_name  # Print element_name

page_name <- html_node(x = infobox_element, css = ".fn")  # Extract element with class fn
page_title <- html_text(page_name)  # Get contents of page_name
page_title  # Print page_title

#--- final exercise
library(httr)
base_url <- "https://en.wikipedia.org/w/api.php"  # The API url
query_params <- list(action = "parse",   # Set query parameters
                     page = "Hadley Wickham", 
                     format = "xml")
resp <- GET(url = base_url, query = query_params)  # Get data from API
resp_xml <- content(resp)  # Parse response


library(rvest)  # Load rvest
page_html <- read_html(xml_text(resp_xml))  # Read page contents as HTML
infobox_element <- html_node(page_html, css = ".infobox")  # Extract infobox element
page_name <- html_node(infobox_element, css = ".fn")  # Extract page name element from infobox
page_title <- html_text(page_name)  # Extract page name as text


# Your code from earlier exercises
wiki_table <- html_table(infobox_element)
colnames(wiki_table) <- c("key", "value")
cleaned_table <- subset(wiki_table, !key == "")

name_df <- data.frame(key = "Full name", value = page_title)  # Create a dataframe for full name
wiki_table2 <- rbind(name_df, cleaned_table)  # Combine name_df with cleaned_table
wiki_table2  # Print wiki_table



#-- functionize the code
library(httr)
library(rvest)
library(xml2)

get_infobox <- function(title){
  base_url <- "https://en.wikipedia.org/w/api.php"
  
  query_params <- list(action = "parse", 
                       page = title,    # title param
                       format = "xml")
  resp <- GET(url = base_url, query = query_params)
  resp_xml <- content(resp)
  
  page_html <- read_html(xml_text(resp_xml))
  infobox_element <- html_node(x = page_html, css =".infobox")
  page_name <- html_node(x = infobox_element, css = ".fn")
  page_title <- html_text(page_name)
  
  wiki_table <- html_table(infobox_element)
  colnames(wiki_table) <- c("key", "value")
  cleaned_table <- subset(wiki_table, !wiki_table$key == "")
  name_df <- data.frame(key = "Full name", value = page_title)
  wiki_table <- rbind(name_df, cleaned_table)
  
  wiki_table
}

# Test get_infobox with "Hadley Wickham"
get_infobox(title = "Hadley Wickham")

# Try get_infobox with "Ross Ihaka"
get_infobox(title = "Ross Ihaka")

# Try get_infobox with "Grace Hopper"
get_infobox(title = "Grace Hopper")
