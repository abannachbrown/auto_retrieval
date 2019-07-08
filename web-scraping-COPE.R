### webscraping

#install.packages("rvest")


library(rvest)
library(tidyverse)
library(tidyr)
library(XML)
library(httr)
library(purrr)

COPE_members_URL <- "https://publicationethics.org/members"


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


############################### ALL RECORDS ########################################################
#################################################################################################
##################### trying out a purrr map_df example FOR ALL RECORDS!!!!!! page 0 - 637 #############################################
url_base_all <- "https://publicationethics.org/members?page="

map_df(0:6, function(i) {
  
  # simple but effective progress indicator
  cat("boom! ")
  
  pg <- read_html(sprintf(url_base_all, i))
  
  data.frame(type=gsub("\n","", html_text(html_nodes(pg, ".search-result__type"))),
             title=gsub("\n","", html_text(html_nodes(pg, ".search-result__title"))),
          #   journal_URL
             info=gsub("\n","", html_text(html_nodes(pg, "dl, dt, dd"))),
             stringsAsFactors=FALSE)
  
}) -> journal_DF


# convert type to factor value as there are only a select few group/type
journal_DF$type <- as.factor(journal_DF$type)


# split info column 
journal_DF <- journal_DF %>%
  separate(info, c("editor", "country", "publisher"), "                  ")

#######################



for( i in 0:233){
# COPE_journals_URL <- "https://publicationethics.org/members?page=0&t=journal"
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
journals_info_type <- journals_LINK %>%
  html_nodes(".search-result__type") %>%
  html_text()#%>%
#  as.data.frame()


## trying to get URL but not working
journals_info_url <- journals_LINK %>%
  html_nodes(".search-result__title") %>%
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
