# Vietnam scraping
# First case table was seen on 11th March . We have the data till 24th April for the same.

library(rvest)
library(stringr)
library(dplyr)

month_start=1
month_end=4
dates_to_grab <- apply(expand.grid(str_pad(month_start:month_end, 2, pad = "0") , str_pad(1:31, 2, pad = "0") ), 1, paste, collapse="")

histor_links_list <- list()
for(q in dates_to_grab){
  # Scraping from main wikipedia page , since the actual table does not have a V.T.E format.
  url <- paste0("https://en.wikipedia.org/w/index.php?title=Template:2019%E2%80%9320_coronavirus_pandemic_data/Russia_medical_cases&action=history&offset=2020",q,"000000&limit=1&action=history")
  print(url)
  wikipedia_history_page <- read_html(url)
  links <- wikipedia_history_page %>% html_nodes("a") %>% html_attr(c('href'))
  titles <- wikipedia_history_page %>% html_nodes("a") %>% html_attr(c('title'))
  text <- wikipedia_history_page %>% html_nodes("a") %>% html_text()
  wikipedia_history_page_links <- data.frame(link=paste0("https://en.wikipedia.org/",links) ,
                                             title=titles,
                                             text=text) %>%
    filter(str_detect(link,"oldid") & text!="cur" & text!="prev")
  histor_links_list[[q]] <- wikipedia_history_page_links
}

history_links_df <- bind_rows(histor_links_list) %>% arrange(link) %>% filter(!duplicated(link))


# Adding function to filter out only wikitables from the tables that are fetched. Wikitables are data tables.

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

#history_links_df = history_links_df[1:5,]

#

tables_list <- list()
for(i in 1:nrow(history_links_df) ){
  q <- history_links_df$link[i]
  edit_date <- history_links_df$text[i]
  print(q)
  
  temp_nodes <- read_html(q)  %>% html_nodes("table")

  filtered = filter_wikitables(temp_nodes)
  
  temp_tables = list()
  for(i in 1:length(filtered))
  {
    # Added try catch for better control on rvest scraper.
    tryCatch({
      t <- html_table(filtered[[i]],fill = TRUE, header=T)
      temp_tables[[i]] <- t
    },error=function(cond) {
      print("Error in one of the tables. Skipping that")
    })
  }

  try({
    for(h in temp_tables){
      #print(dim(h))
      if(
        (names(h) %>% tolower() %>% str_detect(pattern=c("federal subjects")) %>% any(na.rm = TRUE)) && dim(h)[[2]] <= 8
        # Makes sure we have filter out tables with higher dimensions
      ){
        #preprocess h
        if(dim(h)[2] == 7)
        {        
          colnames(h) = c("Federal Subject","Federal Subject","Cases","Recov.","Deaths","Active")
          h = h[3:dim(h)[1],]
          h = h[,2:7]
        }
        else
          {
            colnames(h) = c("Federal Subject","Federal Subject","Cases","Recov.","Deaths","Cases(1 million)","Deaths(1 million)")
            h = h[4:dim(h)[1],]
            h = h[,2:5]
        }
        # Add to list and add a column called edit date.
        tables_list[[q]] <- h
        tables_list[[q]]$edit_date <- edit_date
        print("Table Found")
        break
      }
    }
  })
}

#length(tables_list)
#lapply(tables_list, names)


library(janitor)

russia_subnation_cases <- bind_rows(lapply(tables_list, clean_names))
russia_subnation_cases$na = NULL

for(col in colnames(russia_subnation_cases)){
  for(i in 1:length(russia_subnation_cases[,col]) - 1){
    russia_subnation_cases[i,col] = gsub('\\[.\\]','',russia_subnation_cases[i,col])
    russia_subnation_cases[i,col] = gsub(',','',russia_subnation_cases[i,col])
  }
}

russia_subnation_cases$cases <- as.integer(russia_subnation_cases$cases)
russia_subnation_cases$deaths <- as.integer(russia_subnation_cases$deaths)
russia_subnation_cases$recov <- as.integer(russia_subnation_cases$recov)
russia_subnation_cases$active <- as.integer(russia_subnation_cases$active)

write.csv(russia_subnation_cases,"~/Desktop/mssl/NESScovid19/wikipedia_scrape/russia_scrape.csv")