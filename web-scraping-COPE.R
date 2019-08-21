### webscraping

# install.packages("rvest")
# install.packages("tidyverse")
# install.packages("XML")
# install.packages("httr")
# install.packages("purrr")

## load libraries
library(rvest)
library(tidyverse)
library(XML)
library(httr)
library(purrr)


install.packages("robotstxt")
library(robotstxt)
library(jsonlite)

## check what allowed and dissallowed to access on the COPE website
robtext <- robotstxt(domain = "https://publicationethics.org/")

robtext$comments %>% tbl_df
robtext$permissions



## check if members subdomain is allowed to be scraped
print(paths_allowed("https://publicationethics.org/members"))

# check crawl delay
print(robtext$crawl_delay)
# 10 secs



## sleep crawl delay function
nytnyt <- function(periods = c(1, 1.5)){
  tictoc <- runif(1, periods[1], periods[2])
  cat(paste0(Sys.time()), "sleeping for", round(tictoc,2), "seconds\n")
  Sys.sleep(tictoc)
  
}


## check for IP
get_ip <- function(){
  read_html("https://api.ipify.org/?format=json") %>%
  html_text(., trim = T) %>%
  jsonlite::fromJSON(.)
}


COPE_members_URL <- "https://publicationethics.org/members"

COPE_journals_URL %>% 
  html_nodes(".search-result__title")


#################################################################################################
##################### trying out a purrr map_df example #############################################
url_base <- "https://publicationethics.org/members?t=journal&page="

map_df(0:233, function(i) {
  
  # simple but effective progress indicator
  cat("boom! ")
  
  pg <- read_html(sprintf(url_base, i))
  
  data.frame(type=gsub("\n","", html_text(html_nodes(pg, ".search-result__type"))),
             title=gsub("\n","", html_text(html_nodes(pg, ".search-result__title"))),
             info=gsub("\n","", html_text(html_nodes(pg, ".search-result__meta"))),
             stringsAsFactors=FALSE) 
  
}) -> journal_DF


# convert type to factor value as there are only a select few group/type
journal_DF$type <- as.factor(journal_DF$type)


# split info column 
journal_DF <- journal_DF %>%
  separate(info, c("editor", "country", "publisher"), "                  ")

#######################
#### try one more time


pages_total <- seq(0, 3) #233

#collect page function

collect_pg <- function(x){
  read_html(paste0("https://publicationethics.org/members?page=", x ,
                   "&t=")) %>% 
    
    html_nodes(".search-result__type") %>%
    html_text() 
}



all_pages <- list()

for (i in 1:length(pages_total)){
  cat("[",paste0(Sys.time()), "] Now collecting page ", i, "\n")
  
  all_pages[[i]] <- collect_pg(pages_total[i]) # %>%
    
   # nytnyt(periods = c(10, 11))
  
  # data.frame(#gsub removes the extra \n html tags, html_text reads html into text, html_nodes identifies the css-selector on the "page"
  #   type=gsub("\n","", html_text(html_nodes(all_pages, ".search-result__type"))),
  #   title=gsub("\n","", html_text(html_nodes(all_pages, ".search-result__title"))),
  #   info=gsub("\n","", html_text(html_nodes(all_pages, "search-result__meta"))),
  #   stringsAsFactors=FALSE)-> journal_DF
  
  
}

test <-all_pages %>% map_df(~as.list(.))

# all_pages <- as.data.frame(all_pages)


data.frame(#gsub removes the extra \n html tags, html_text reads html into text, html_nodes identifies the css-selector on the "page"
  type=gsub("\n","", html_text(html_nodes(all_pages, ".search-result__type"))),
  title=gsub("\n","", html_text(html_nodes(all_pages, ".search-result__title"))),
  info=gsub("\n","", html_text(html_nodes(all_pages, "search-result__meta"))),
  stringsAsFactors=FALSE)-> journal_DF


############################### ALL RECORDS ########################################################
#################################################################################################
##################### trying out a purrr map_df example FOR ALL RECORDS!!!!!! page 0 - 637 #############################################
url_base_all <- "https://publicationethics.org/members?page="

## map_df transforms the input and returns a dataframe
map_df(3:4, function(i) {
  
  # simple progress indicator
  cat("boom! ")
 
  for (i in 1:3) {
  
 all_URL <- rvest:::html_session(paste0(url_base_all, i, "&t="),
                              user_agent("Mozilla/5.0 (Macintosh; Intel Mac OS X 10_11_4) 
                                         AppleWebKit/537.36 (KHTML, like Gecko) 
                                         Chrome/50.0.2661.86 Safari/537.36")
                             #, httr::headers("User-Agent" = "Mozilla/5.0 (X11; Linux x86_64) 
                             # AppleWebKit/537.36 (KHTML, like Gecko) 
                             # Chrome/75.0.3770.90 Safari/537.36"),
                             ) 

 
 
 # %>% read_html() %>%
 #   as.character()
 #   cat


 # nytnyt(periods = c(10, 11))
 
  #for each page of search results generate URL
  page <- all_URL %>%
        nytnyt(periods = c(10, 11)) %>%
      read_html() %>%
        cat
  
  }
  
  data.frame(#gsub removes the extra \n html tags, html_text reads html into text, html_nodes identifies the css-selector on the "page"
             type=gsub("\n","", html_text(html_nodes(page, ".search-result__type"))),
             title=gsub("\n","", html_text(html_nodes(page, ".search-result__title"))),
             info=gsub("\n","", html_text(html_nodes(page, "search-result__meta"))),
             stringsAsFactors=FALSE)-> journal_DF
  
}) -> journal_DF ##map into this dataframe

journal_DF <- NULL

# convert type to factor value as there are only a select few group/type
journal_DF$type <- as.factor(journal_DF$type)


# split info column 
journal_DF <- journal_DF %>%
  separate(info, c("editor", "country", "publisher"), "                  ")

#######################



for( i in 0:233){
 COPE_journals_URL <- read_html("https://publicationethics.org/members?page=0&t=journal")
journals_LINK <- read_html(paste0("https://publicationethics.org/members?t=journal&page=", i))



# journals_LINK <- read_html(COPE_journals_URL)

# # FREE_TEXT <- LINK %>%
#           html_nodes("block-system-main", ".search-result")
# 
# 
# FREE_TEXT <- LINK %>%
#   html_nodes("#block-system-main", ":nth-child(2)")


# FREE_TEXT <- LINK %>%
#   html_nodes(".search-result") %>%
#   html_text()

## extract the exact elements of search results from 
# type - journal or publisher
journals_info_type <- page %>%
  html_nodes(".search-result__type") %>%
  html_text()#%>%
#  as.data.frame()


## trying to get URL but not working
journals_info_url <- page %>%
  html_nodes(".search-result__title") %>%
  html_attr("a href")# %>%

journals_info_url <- COPE_journals_URL %>%
  # html_nodes(".search-result__title") %>%
  html_attr("href")# %>%

# title
journals_info_title <- journals_LINK %>%
  html_nodes(".search-result__title") %>%
  html_text()#%>%
#  as.data.frame()

# meta info including editor, country, publisher etc... 
journals_info_info <- journals_LINK %>%
  html_nodes(".search-result__meta") %>%
  html_text()#%>%
#  as.data.frame()

# head(journals_info)


## remove /n 
journals_info_type<-gsub("\n","",journals_info_type)
journals_info_title<-gsub("\n","",journals_info_title)
journals_info_info<-gsub("\n","",journals_info_info)

# convert type to factor value as there are only a select few group/type
journals_info_type <- as.factor(journals_info_type)

# merge into data.frame
journal_info_df <- data.frame(type = journals_info_type, title = journals_info_title, info = journals_info_info)


# split info column 
journal_DF <- journal_info_df %>%
  separate(info, c("editor", "country", "publisher"), "                  ")



}


##################### testing tripadvisor href eg ###################
################## https://www.worldfullofdata.com/basics-web-scraping-r-rvest/ 


tripadvisor_home <- read_html("https://www.tripadvisor.com/Restaurants-g60763-New_York_City_New_York.html")

restaurant_URLs <- tripadvisor_home %>%
  html_nodes(".property_title") %>%
  html_attr("href")
tripadvisor_home



###############################################################

Words <- str_extract_all(journals_info, boundary("word"))

things <- do.call(cbind.data.frame, Words)


## remove /n

journal_df <- gsub("\n\n  \n    \n", "", journals_info)
journal_df <- gsub("\n\n  \n", "", journals_info)


journal_data$. <-gsub("\n\n  \n    \n","",journals_info$.)


## split into columns 
journal_df <- str_split(journals_info$., "[\r\n]{2,}", 3)


# remove excess spaces
journal_data2 <-gsub(" ","",journal_data)


head(journal_data)

## for each page of the search - 



############################################################################################################
########## https://stat4701.github.io/edav/2015/04/02/rvest_tutorial/ #################################

query = "journal"

session <- html_session("https://publicationethics.org/members")
form <- html_form(session)[[1]]
form <- set_values(form, t = query)


submit_form2 <- function(session, form){
  library(XML)
  url <- XML::getRelativeURL(form$url, session$url)
  url <- paste(url,'?',sep='')
  values <- as.vector(rvest:::submit_request(form)$values)
  att <- names(values)
  if (tail(att, n=1) == "NULL"){
    values <- values[1:length(values)-1]
    att <- att[1:length(att)-1]
  }
  q <- paste(att,values,sep='=')
  q <- paste(t, collapse = '&')
  q <- gsub(" ", "+", t)
  url <- paste(url, t, sep = '')
  html_session(url)
}


appendList <- function (x, val)
{
  stopifnot(is.list(x), is.list(val))
  xnames <- names(x)
  for (v in names(val)) {
    x[[v]] <- if (v %in% xnames && is.list(x[[v]]) && is.list(val[[v]]))
      appendList(x[[v]], val[[v]])
    else c(x[[v]], val[[v]])
  }
  x
}


submit_geturl <- function (session, form)
{
  query <- rvest:::submit_request(form)
  query$method <- NULL
  query$encode <- NULL
  query$url <- NULL
  names(query) <- "query"
  
  relativeurl <- XML::getRelativeURL(form$url, session$url)
  basepath <- parse_url(relativeurl)
  
  fullpath <- appendList(basepath,query)
  fullpath <- build_url(fullpath)
  fullpath
}

session1 <- submit_form2(session, form)

## doesnt work - submit_form2 needs amending to the specific webpage
##################################################################################################




##############################################################################################
### testing Jenny Shen's demo https://github.com/rladies/meetup-presentations_brisbane/blob/master/2019/07/2019-07-02__Jenny__Webscraping_demo.R 


COURSE <- 'RII20715'
CODE <- '0260'
URL <- paste0("https://www.myskills.gov.au/RegisteredTrainers/Details?rtocode=", CODE, 
              "&CourseCode=", COURSE)
print(URL)

try({
  LINK <- read_html(URL)
  # As myskills is not set out as an table, scraped as free text 
  FREE_TEXT <- LINK %>%
    html_nodes("#campuses") %>%
    html_text()
  
  PATTERN <- "[[:space:]]+[0-9]+[0-9]+[0-9]+[0-9]"
  
  POSTCODES <- str_extract_all (FREE_TEXT, PATTERN)
  
  TEMP <- do.call(cbind.data.frame, POSTCODES) %>%
    setNames(c("postcode")) %>%
    mutate(rto_code = CODE,
           postcode_type = "Campus",
           course_code = COURSE)
  TEMP[1, 3] = "Head office"
  
})
