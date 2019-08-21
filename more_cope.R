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



##########################################################################
#### try one more time using https://hanjostudy.github.io/Presentations/UseR2018/Rvest/rvest.html#38 - example
#################### youtube video here: https://www.youtube.com/watch?v=OxbvFiYxEzI&list=PL4IzsxWztPdnyAKQQLxA4ucpaCLdsKvZw&index=13&t=3678s 


pages_total <- seq(0, 3) #233
journals_DF <- data.frame()
#collect page function

collect_pg <- function(x){
  read_html(paste0("https://publicationethics.org/members?page=", x ,
                   "&t=")) %>% 
    
    html_nodes(".search-result__type") %>%
    html_text() %>%
    gsub("\n","", .)
}


all_pages <- list()

for (i in 1:length(pages_total)){
  cat("[",paste0(Sys.time()), "] Now collecting page ", i, "\n")
  
  all_pages[[i]] <- collect_pg(pages_total[i]) # %>%
  
  # nytnyt(periods = c(10, 11))
  
  # data.frame(#gsub removes the extra \n html tags, html_text reads html into text, html_nodes identifies the css-selector on the "page"
  #   type=gsub("\n","", html_text(html_nodes(all_pages, ".search-result__type"))),
  #   title=gsub("\n","", html_text(html_nodes(all_pages, ".search-result__title"))),
  #   info=gsub("\n","", html_text(html_nodes(all_pages, ".search-result__meta"))),
  #   stringsAsFactors=FALSE)-> journal_DF
  
  
}

## this doesnt work
test <-all_pages %>% map_df(~as.list(.))



######################################################################################## 
### https://rpubs.com/ryanthomas/webscraping-with-rvest
####################################

# pages<-seq(0, 3)

journals_DF <- data.frame()


for (i in (0:3)){

  cat("[",paste0(Sys.time()), "] Now collecting page ", i, "\n")
  URLs <- paste0("https://publicationethics.org/members?page=", 0:3 , "&t=")

cope_html <- read_html(URL)

cope_html <- map(URLs[0:2], function(URL){
  Sys.sleep(10)    # set crawl delay
  read_html(URL)
}) 


page_type <- cope_html %>%
  html_nodes(".search-result__type") %>%
  html_text() %>%
  gsub("\n","", .) 
  
page_title <- cope_html %>%
  html_nodes(".search-result__title") %>%
  html_text() %>%
  gsub("\n","", .) 
  

page_info <- cope_html %>%
  html_nodes(".search-result__meta") %>%
  html_text() %>%
  gsub("\n","", .) 




tmp <- data.frame(page_type, page_title, page_info)
if (pages == 0 ) {
  cope_df <- data.frame(tmp)
} else {
  cope_df <- rbind(cope_df, tmp)  
}



}

journal_DF <- cope_df %>%
  separate(page_info, c("editor", "country", "publisher"), "                  ")
