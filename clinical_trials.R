# clinical trials.gov

#install package 
install.packages("devtools")
library(devtools)
install_github("sachsmc/rclinicaltrials")
library(rclinicaltrials)


#count trials - run on 23/05/2019

clinicaltrials_count(query = "ALL") # returns all records 306388

# find out how to use advanced settings
advanced_search_terms

#actively recruiting trials
clinicaltrials_count(query = c("recr=Recruiting", "ALL")) # 50,679

#completed trials
clinicaltrials_count(query = c("recr=Completed", "ALL")) # 163,624

# first recieved or first posted on ClinicalTrials.gov mm/dd/yyyy in 2018
clinicaltrials_count(query = c("rcv_s=01/01/2018", "rcv_e=12/31/2018")) #30985

# last updated posted in mm/dd/yyyy 2018
clinicaltrials_count(query = c("lup_s=01/01/2018", "lup_e=12/31/2018")) # 62844




