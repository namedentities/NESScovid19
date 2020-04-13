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
fetch_for_country <- function(country_name){
  country_name = str_replace_all(country_name,' ','_')
  print(country_name)
  tryCatch({
      
        country_page_url = paste("https://en.wikipedia.org/wiki/2020_coronavirus_pandemic_in_",country_name,sep = "")
        print(country_page_url)
      country_table_nodes = read_html(country_page_url,options = 'RECOVER') %>% html_nodes("table")
      return(fetch_tables(country_table_nodes))
      
      },
      
    error = function(err){
        country_page_url = paste("https://en.wikipedia.org/wiki/2019-20_coronavirus_pandemic_in_",country_name,sep = "")
        country_table_nodes = read_html(country_page_url,options = 'RECOVER') %>% html_nodes("table")
      return(fetch_tables(country_table_nodes))
    }
  )
}

#This function filters the keywords -> States , Region and Province from region
state_filter <- function(country){
  tbls = fetch_for_country(country)
  return(tbls[grep("State|Province|Region",tbls,ignore.case = T)])
}

# Run using
#state_filter('India')
#state_filter('Austria')

