
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
html %>% html_nodes("table:first-child")  # :first-child not supported?
html %>% html_nodes("table:nth-child(2)") # :nth-child(1) not supported?
html %>% html_nodes("table:last-child") %>% html_table(header=T)

lst <- html %>% html_nodes("table") %>% html_table(header = T)
lst[[1]]
lst[[2]]


#----- 2 Navigation and Selection with CSS -----
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
html %>% html_nodes("div.paragraph, .second.paragraph > div")
html %>% html_nodes("div div")
#----- 3 Advanced Selection with XPATH -----

#----- 4 Scraping best practices -----