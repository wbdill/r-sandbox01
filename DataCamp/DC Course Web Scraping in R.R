
#https://campus.datacamp.com/courses/web-scraping-in-r/introduction-to-html-and-web-scraping
# 2022-05-14
library(tidyverse)
library(rvest)

#----- 1 Introduction to HTML and web scraping -----
html_excerpt_raw <- '
<html> 
  <body> 
    <h1>Web scraping is cool</h1>
    <p>It involves writing code – be it R or Python.</p>
    <p><a href="https://datacamp.com">DataCamp</a> 
		has courses on it.</p>
  </body> 
</html>'

html_excerpt <- read_html(html_excerpt_raw)  # Turn the raw excerpt into an HTML document R understands
html_excerpt
xml_structure(html_excerpt)  # Print the HTML excerpt with the xml_structure() function


list_raw_html <- "\n<html>\n  <body>\n    <ol>\n      <li>Learn HTML</li>\n      <li>Learn CSS</li>\n      <li>Learn R</li>\n      <li>Scrape everything!*</li>\n    </ol>\n    <small>*Do it responsibly!</small>\n  </body>\n</html>"
list_html <- read_html(list_raw_html)
ol_node <- list_html %>% 
 rvest::html_node('ol')
ol_node %>% rvest::html_children()


hyperlink_raw_html <- "\n<html>\n<body>\n<h3>Helpful links</h3>\n<ul>
<li><a href=\"https://wikipedia.org\">Wikipedia</a></li>
<li><a href=\"https://dictionary.com\">Dictionary</a></li>
<li><a href=\"https://duckduckgo.com\">Search Engine</a></li>\n</ul>\n<small>
Compiled with help from <a href=\"https://google.com\">Google</a>.\n</small>\n</body>\n</html>"
links <- hyperlink_raw_html %>% 
  read_html() %>% 
  html_nodes('li a')  # Extract all the a nodes from the bulleted list

domain_value = links %>% html_attr("href")  # Extract the needed values for the data frame
name_value = links %>% html_text()

link_df <- tibble(  # Construct a data frame
  domain = domain_value,
  name = name_value
)
link_df

html <- rvest::read_html("http://briandill.com/data/html_table.htm")
html <- xml2::read_html("http://briandill.com/data/html_table.htm")

tbl_clean <- html %>% html_node("table#clean") %>% html_table()
tbl_dirty <- html %>% html_node("table#dirty") %>% html_table(header = TRUE)  # header=T uses first row as header even though it's not <th>
str(tbl_clean)
str(tbl_dirty)

html %>% html_nodes("table")
html %>% html_nodes("table:first-child")  # :first-child not supported for tables?
html %>% html_nodes("table:nth-child(2)") # :nth-child(2) not supported for tables?
html %>% html_nodes("table:last-child") %>% html_table(header=T)

lst <- html %>% html_nodes("table") %>% html_table(header = T, fill=T)
lst[[1]]
lst[[2]]


#----- 2 Navigation and Selection with CSS ----------------------------------------------------------------------
languages_raw_html <- "\n<html> \n  <body> \n
<div>Python is perfect for programming.</div>\n
<p>Still, R might be better suited for data analysis.</p>\n
<small>(And has prettier charts, too.)</small>\n  </body> \n</html>"

languages_html <- html(languages_raw_html)
languages_html <- xml2::read_html(languages_raw_html)
languages_html %>% html_nodes("div, p") %>% html_text()

structured_html <- "<html><body>
<div id = 'first'>  <h1 class = 'big'>Joe Biden</h1>  <p class = 'first blue'>Democrat</p>  <p class = 'second blue'>Male</p></div>
<div id = 'second'>...</div>
<div id = 'third'>  <h1 class = 'big'>Donald Trump</h1>  <p class = 'first red'>Republican</p>  <p class = 'second red'>Male</p></div>
</body></html>"
html <- read_html(structured_html)
html %>% html_nodes("div:first-child")
html %>% html_nodes("div:nth-child(3)")

raw <- "<html><body>
<ul id = 'languages'>
    <li>SQL</li>
    <ul>    
      <li>Databases</li>
      <li>Query Language</li>
    </ul>
    <li>R</li>
    <ul>
      <li>Collection</li>
      <li>Analysis</li>
      <li>Visualization</li>
    </ul>
    <li>Python</li>
  </ul></body></html>"
html <- read_html(raw)
html %>% html_nodes("li") %>% html_text()

html %>% html_nodes("ul#languages > li") %>% html_text()


raw <- '<html><body>
<div class="first section">
  Some text with a <a href="#">link</a>.
</div>
  <div class="second section">
    Some text with <a href="#">another link</a>.
    <div class="first paragraph">Some text.</div>
    <div class="second paragraph">Some more text.
      <div>...</div>
    </div>
  </div>
  </body>
  </html>'
html <- read_html(raw)
html %>% html_nodes(".first + .second > div, div.second.paragraph > div")  # overly complex
html %>% html_nodes("div div")

#----- 3 Advanced Selection with XPATH ----------------------------------------------------------------------

#  brackets [] are predicates to the selector just before it
## ex: //div[@class = 'foo'] gets all divs with class of "foo"

html_raw <- "<html><body>
    <div id = 'first'>
      <h1 class = 'big'>Berlin Weather Station</h1>
      <p class = 'first'>Temperature: 20°C</p>
      <p class = 'second'>Humidity: 45%</p>
    </div>
    <div id = 'second'>...</div>
    <div id = 'third'>
      <p class = 'first'>Sunshine: 5hrs</p>
      <p class = 'second'>Precipitation: 0mm</p>
    </div>
  </body></html>"
html <- read_html(html_raw)
html %>% html_nodes(xpath = "//p")  # all p elements
html %>% html_nodes(xpath = "//p[@class = 'second']")  # p with class "second"
html %>% html_nodes(xpath = "//*[@id='third']/p") # Select p elements that are children of "#third"
html %>% html_nodes(xpath = "//*[@id='third']/p[@class='second']")  # Select p elements with class "second" that are children of "#third"


html %>% html_nodes(xpath = "//div")  # all divs
html %>% html_nodes(xpath = "//div[p]")  # all divs with p descendants
html %>% html_nodes(xpath = '//div[p [@class="second"]]')    # Select all divs with p descendants having the "second" class

html_raw <- "<div>
  <h2>Today's rules</h2>
  <p>Wear a mask</p>
  <p>Wash your hands</p>
</div>
<div>
  <h2>Tomorrow's rules</h2>
  <p>Wear a mask</p>
  <p>Wash your hands</p>
  <small>Bring hand sanitizer with you</small>
</div>"
html <- read_html(html_raw)
html %>% html_nodes(xpath = "//div/p[position() = 2]") %>% html_text() # Select the text of the second p in every div
html %>% html_nodes(xpath = "//div/p[position() != 2]") %>% html_text()
html %>% html_nodes(xpath = "//div[position() = 2]/*[position() >= 2]") # Select the text of the last three nodes of the second div
html %>% html_nodes(xpath = "")

html_raw <- "<div>
  <h1>Tomorrow</h1>
</div>
<div>
  <h2>Berlin</h2>
  <p>Temperature: 20°C</p>
  <p>Humidity: 50%</p>
</div>
<div>
  <h2>London</h2>
  <p>Temperature: 15°C</p>
</div>
<div>
  <h2>Zurich</h2>
  <p>Temperature: 22°C</p>
  <p>Humidity: 60%</p>
</div>"
html <- read_html(html_raw)
html %>% html_nodes(xpath = "//div[count(h2) = 1 and count(p) >= 2]") # Select only divs with one h2 header and at least two paragraphs
html %>% html_nodes(xpath = "")
html %>% html_nodes(xpath = "")
html %>% html_nodes(xpath = "")
html %>% html_nodes(xpath = "")

#----- 4 Scraping best practices ----------------------------------------------------------------------

library(httr)
wikipedia_response <- GET('https://en.wikipedia.org/wiki/Varigotti')  # Get the HTML document from Wikipedia using httr
status_code(wikipedia_response)  # Check the status code of the response
wikipedia_page <- content(wikipedia_response)  # Parse the response into an HTML doc
wikipedia_page %>% 
  html_nodes(xpath = "//table//tr[position() = 9]/td") %>% 
  html_text()


response = GET("http://foo.com", user_agent("this is @bdill"))  # set user-agent for single call
set_config(add_headers(`User-Agent` = "This is @bdill")) #<< config User-Agent for the session


response <- GET("https://httpbin.org/headers")  # Access https://httpbin.org/headers with httr
content(response)  # Print its content

# Pass a custom user agent to a GET query to the mentioned URL
response <- GET("https://httpbin.org/user-agent", user_agent("A request from a DataCamp course on scraping"))
content(response)  # Print the response content


# Globally set the user agent to "A request from a DataCamp course on scraping"
set_config(add_headers(`User-Agent` = "A request from a DataCamp course on scraping"))
# Pass a custom user agent to a GET query to the mentioned URL
response <- GET("https://httpbin.org/user-agent")
# Print the response content
content(response)


library(httr)
library(purrr)
throttled_GET <- slowly(
  ~ GET(.),
  rate = rate_delay(3)) 

while(TRUE) {
  print(Sys.time())
  response <- throttled_GET("https://wikipedia.org")
  print(status_code(response))
}


url_list <- c("https://httbin.org/anything/1",
              "https://httbin.org/anything/2",
              "https://httbin.org/anything/3")
for(url in url_list){
  response <- throttled_GET(url) 
  print(status_code(response))
}


# ----- final example -----
library(httr)
library(purrr)
mountain_wiki_pages <- c("https://en.wikipedia.org/w/index.php?title=Mount_Everest&oldid=958643874",
                         "https://en.wikipedia.org/w/index.php?title=K2&oldid=956671989",           
                         "https://en.wikipedia.org/w/index.php?title=Kangchenjunga&oldid=957008408")
read_html_delayed <- slowly(~ read_html(.), 
                            rate = rate_delay(0.5))
for(page_url in mountain_wiki_pages){
  html <- read_html_delayed(page_url)
  peak <- html %>% 
    html_nodes("#firstHeading") %>% html_text()
  coords <- html %>% 
    html_nodes("#coordinates .geo-dms") %>% html_text()
  print(paste(peak, coords, sep = ": "))
}



