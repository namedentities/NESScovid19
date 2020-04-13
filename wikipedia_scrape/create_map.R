library(rvest)
library(dplyr)
library(lubridate)
library(magrittr)
library(tidyverse)


wp_page_url = "https://en.wikipedia.org/wiki/COVID-19_testing"
testing_webpage <- read_html(wp_page_url)
table_list = testing_webpage %>% html_nodes("table") %>% html_table(fill = TRUE) %>% .[[3]]
hyperrefs = table_list %>% html_nodes('a') 


fetch_valid_links <- function(link_list){
  links = list()
  for(i in 1:length(link_list))
    {
        current_link = html_attr(link_list[[i]],name = 'href')
        links  = append(links,current_link[grep('wiki/20',current_link)])
    }
  return(links)
}

links = as.data.frame(unlist((fetch_valid_links(hyperrefs))))
countries = html_table(table_list)[1]

#These states contain errors on the wikipedia end. The links point to non-coronavirus links, hence removing them
italian_states  = c("Emilia-Romagna","Liguria","Lombardy","Marche","Piedmont","Tuscany","Veneto")
countries = countries[!(countries$`Country or region` %in% italian_states),]

country_map <- cbind.data.frame(countries,links)
