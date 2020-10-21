# DC Course Working with Web Data in R
# https://learn.datacamp.com/courses/working-with-web-data-in-r
library(httr)

?download.file(url = "http://foo.com/remote.csv", destfile = "local.csv")

#----- Ch 1 Downloading Files and Using API Clients -----

# Here are the URLs! As you can see they're just normal strings
csv_url <- "http://s3.amazonaws.com/assets.datacamp.com/production/course_1561/datasets/chickwts.csv"
tsv_url <- "http://s3.amazonaws.com/assets.datacamp.com/production/course_3026/datasets/tsv_data.tsv"

csv_data <- read.csv(csv_url)  # Read a file in from the CSV URL and assign it to csv_data
tsv_data <- read.delim(tsv_url)  # Read a file in from the TSV URL and assign it to tsv_data


head(csv_data)  # Examine the objects with head()
head(tsv_data)

download.file(url = csv_url, destfile = "data/feed_data.csv")
csv_data <- read.csv("data/feed_data.csv")

csv_data$square_weight <- csv_data$weight ^2  # Add a new column: square_weight
saveRDS(object = csv_data, file = "data/modified_feed_data.rds")  # Save it to disk with saveRDS()
modified_feed_data <- readRDS(file = "data/modified_feed_data.rds")  # Read it back in with readRDS()
str(modified_feed_data)  # Examine modified_feed_data

install.packages("pageviews")
library(pageviews)
pv <- article_pageviews(article = "R_(programming_language)")

install.packages("birdnik")

#----- Ch 2 Using httr to interact with APIs directly -----
library(httr)
get_result <- GET(url = "http://httpbin.org/get")  # Make a GET request to http://httpbin.org/get
get_result  # Print it to inspect it

# Make a POST request to http://httpbin.org/post with the body "this is a test"
post_result <- POST(url = "http://httpbin.org/post", body = "this is a test")
post_result  # Print it to inspect it

url <- "https://wikimedia.org/api/rest_v1/metrics/pageviews/per-article/en.wikipedia.org/all-access/all-agents/Hadley_Wickham/daily/20170101/20170102"
pageview_response <- GET(url)  # Make a GET request to url and save the results
pageview_data <- content(pageview_response)  # Call content() to retrieve the data the server sent back
str(pageview_data)  # Examine the results with str()

fake_url <- "http://google.com/fakepagethatdoesnotexist"
request_result <- GET(fake_url)  # Make the GET request
if(http_error(request_result)){  # Check request_result
  warning("The request failed")
} else {
  content(request_result)
}

# Construct a directory-based API URL to `http://swapi.co/api`, looking for person `1` in `people`
directory_url <- paste("http://swapi.co/api", "people", 1, sep = "/")
result <- GET(directory_url)  # Make a GET call with it
result


# Create list with nationality and country elements
query_params <- list(nationality = "americans", country = "antigua")
parameter_response <- GET("https://httpbin.org/get", query = query_params)  # Make parameter-based call to httpbin, with query_params
parameter_response  # Print parameter_response

# Do not change the url
url <- "https://wikimedia.org/api/rest_v1/metrics/pageviews/per-article/en.wikipedia/all-access/all-agents/Aaron_Halfaker/daily/2015100100/2015103100"
# Add the email address and the test sentence inside user_agent()
server_response <- GET(url, user_agent("my@email.address this is a test"))


urls <- c("http://httpbin.org/status/404", "http://httpbin.org/status/301")  # Construct a vector of 2 URLs
for(url in urls){
  result <- GET(url)  # Send a GET request to url
  Sys.sleep(5) # Delay for 5 seconds between requests
}

#--- tying it all together
get_pageviews <- function(article_title){
  url <- paste(
    "https://wikimedia.org/api/rest_v1/metrics/pageviews/per-article/en.wikipedia/all-access/all-agents", 
    article_title, # Include article title 
    "daily/2015100100/2015103100", 
    sep = "/"
  ) 
  response <- GET(url, config = user_agent("my@email.com this is a test"))   # Get the webpage  
  if(http_error(response)){   # Is there an HTTP error?
    stop("the request failed")   # Throw an R error
  }
  content(response)
}

#----- Ch 3 Handling JSON and XML -----
# JSON is stored as lists in R because they can nest just like JSON

#--- manipulating parsed JSON with rlist pkg
resp_json <- GET("https://en.wikipedia.org/w/api.php?action=query&titles=Hadley%20Wickham&prop=revisions&rvprop=timestamp%7Cuser%7Ccomment%7Ccontent&rvlimit=5&format=json&rvdir=newer&rvstart=2015-01-14T17%3A12%3A45Z&rvsection=0")
content(resp_json)
install.packages("rlist")  
library(rlist)
str(content(resp_json), max.level = 4)  # Examine output of this code
revs <- content(resp_json)$query$pages$`41916270`$revisions  # Store revision list
user_time <- list.select(revs, user, timestamp)  # Extract the user element
user_time  # Print user_time
list.stack(user_time)  # Stack to turn into a data frame

library(dplyr)
revs <- content(resp_json)$query$pages$`41916270`$revisions  # Pull out revision list
revs %>%  # Extract user and timestamp
  bind_rows() %>%           
  select(user, timestamp)

#--- XML
library(xml2) # Load xml2
resp_xml <- rev_history("Hadley Wickham", format = "xml")  # Get XML revision history
http_type(resp_xml)  # Check response is XML 
rev_text <- content(resp_xml, as = "text")  # Examine returned text with content()
rev_text
rev_xml <- read_xml(rev_text)  # Turn rev_text into an XML document
xml_structure(rev_xml)  # Examine the structure of rev_xml

# XPATH   Meaning
# /node   elements with tag "node" at this level
# //node  elements with tag "node" at or below this level
# @attr   attribute with name "attr"
#-- get nodes with xml_find_all()
#-- extract contents with xml_text(), xml_double(), xml_integer() or as_list()

# Find all nodes using XPATH "/api/query/pages/page/revisions/rev"
xml_find_all(rev_xml, "/api/query/pages/page/revisions/rev")
rev_nodes <- xml_find_all(rev_xml, "//rev")  # Find all rev nodes anywhere in document
xml_text(rev_nodes)  # Use xml_text() to get text from rev_nodes

rev_nodes <- xml_find_all(rev_xml, "//rev")  # All rev nodes
first_rev_node <- xml_find_first(rev_xml, "//rev")  # The first rev node
xml_attrs(first_rev_node)  # Find all attributes with xml_attrs()
xml_attr(first_rev_node, "user")  # Find user attribute with xml_attr()
xml_attr(rev_nodes, "user")  # Find user attribute for all rev nodes
xml_attr(rev_nodes, "anon")  # Find anon attribute for all rev nodes

get_revision_history <- function(article_title){
  rev_resp <- rev_history(article_title, format = "xml") # Get raw revision response
  rev_xml <- read_xml(content(rev_resp, "text"))  # Turn the content() of rev_resp into XML
  rev_nodes <- xml_find_all(rev_xml, "//rev")  # Find revision nodes
  user <- xml_attr(rev_nodes, "user")  # Parse out usernames
  timestamp <- readr::parse_datetime(xml_attr(rev_nodes, "timestamp"))  # Parse out timestamps
  content <- xml_text(rev_nodes)  # Parse out content
  data.frame(user = user,    # Return data frame 
             timestamp = timestamp,
             content = substr(content, 1, 40))
}

#----- Ch 4 Web scraping with XPATHs -----
install.packages("rvest")  # web scraping package
library(rvest)
# html_text()  html_attr() html_name() html_node() 
# html_table() extract html table to data frame
# data.frame() with vectors of text to put them into data frames

test_url <- "https://en.wikipedia.org/wiki/Hadley_Wickham"  # Hadley Wickham's Wikipedia page
test_xml <- read_html(test_url)  # Read the URL stored as "test_url" with read_html()
test_xml  # Print test_xml

test_node_xpath <- "//*[contains(concat( \" \", @class, \" \" ), concat( \" \", \"vcard\", \" \" ))]"
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
