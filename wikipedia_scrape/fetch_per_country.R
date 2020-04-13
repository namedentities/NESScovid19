library(rvest)
library(dplyr)
library(lubridate)
library(stringr)


# This function filters all the tables to obtain only the wiki-tables for that country.
filter_wikitables <- function(list_of_nodes)
{
  temp_list = list()
  
  for (i in 1:length(list_of_nodes)){
    s = html_attr(list_of_nodes[i],name='class')
    if(is.na(s)){
      s = ''
    }
    
    if(str_detect(s,'wikitable') == TRUE)
    {
      temp_list = append(temp_list,list_of_nodes[i])
    }
  }
  
return(temp_list)
}

# This function actually fetches data from the tables links
fetch_tables <- function(list_of_table_names){
  temp_list = list()
  filtered = filter_wikitables(list_of_table_names)
  for (i in 1:length(filtered)){
    temp_list = append(temp_list,list(html_table(filtered[[i]],header = FALSE,fill = TRUE)))
  }
  return(temp_list)
}

# This function is the first function called in the chain , where we create the country link and 
# Changes to be made - Add hardcoded links for edge cases like china , netherlands etc.
# Also handle error 404 for links that do not have wikitables (wikitables are statistics tables)
fetch_for_country <- function(country){
  print(country[1])
  tryCatch({
        country_page_url = paste("https://en.wikipedia.org/",country[2],sep = "")
        print(country_page_url)
      country_table_nodes = read_html(country_page_url,options = 'RECOVER') %>% html_nodes("table")
      return(fetch_tables(country_table_nodes))
      },
    error = function(err){
      print('Suitable tables not found')
    }
  )
}

#This function filters the keywords -> States , Region and Province from region

state_filter <- function(row){
  tbls = fetch_for_country(row)
  return(tbls[grep("State|Province|Region|Area|Hospital|Territory",tbls,ignore.case = T)])
}

# HOW TO RUN

# apply(country_map[3,],1,state_filter)

# OUTPUT

# > apply(country_map[3,],1,state_filter)
# countries 
# "Armenia" 
# [1] "https://en.wikipedia.org//wiki/2020_coronavirus_pandemic_in_Armenia"
# $`3`
# $`3`[[1]]
# X1    X2      X3    X4                     X5
# 1     Province Cases Deathsb  Date               Sourcesc
# 2  Yerevan (c)   219       7 03/31 [14],[citation needed]
# 3       Ararat   179         04/09                   [15]
# 4       Kotayk    88         03/31                   [14]
# 5      Armavir    51         03/31                   [14]
# 6       Syunik    25         04/06                   [14]
# 7       Shirak    13         04/09                   [16]
# 8   Aragatsotn    10         04/07                   [14]
# 9         Lori     8         04/03                   [17]
# 10      Tavush     6       0 04/09                   [18]
# 11 Vayots Dzor     1         03/31                   [14]
# 12 Gegharkunik     0       0 03/31      [citation needed]
# 13        TBDa   413       6 04/10                       
# 14       Total 1,013      13 

