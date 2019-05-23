## load libraries
## pubmed package
install.packages("rentrez")
library(rentrez)



## Pubmed searches

## find out what fields we can search in pubmed
pubmed_fields <- entrez_db_searchable("pubmed")

# human systematic reviews pubmed query
sys_rev_human_query <- "(systematic review[PTYP] AND human[MESH])"


sys_rev_human_search <- entrez_search(db = "pubmed", sys_rev_human_query, use_history = TRUE )
sys_rev_human_search #105352 hits



#search for years of systematic reviews human
sys_rev_year <- function(year, term){
  query <- paste(term, "AND(", year, "[PDAT])")
  entrez_search(db="pubmed", term = query, retmax=0)$count
  
}

year <- 2008:2018
papers <- sapply(year, sys_rev_year, term="(systematic review[PTYP] AND human[MESH])", USE.NAMES = FALSE)

plot(year, papers, type='b', main="Increase in Systematic Reviews - Count per year 2008:2018")
