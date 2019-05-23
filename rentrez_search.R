## load libraries
## pubmed package
install.packages("rentrez")
library(rentrez)



## Pubmed searches

## find out what fields we can search in pubmed
pubmed_fields <- entrez_db_searchable("pubmed")

# human systematic reviews pubmed query
sys_rev_human_query <- "(systematic review[Publication Type] AND human[MeSH Terms])"


sys_rev_human_search <- entrez_search(db = "pubmed", sys_rev_human_query, use_history = TRUE, )