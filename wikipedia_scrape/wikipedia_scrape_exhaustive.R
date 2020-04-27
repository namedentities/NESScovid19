library('countrycode')

data(codelist)
countries_for_matching <- codelist %>% unlist() %>% unique() %>% sort()

#link has
"2020_coronavirus_pandemic_in_"
url <- "https://en.wikipedia.org/wiki/2019%E2%80%9320_coronavirus_pandemic_by_country_and_territory"
#the 5000 is to say don't limit number of hits on the page, 5k 
url <- "https://en.wikipedia.org/w/index.php?title=Special:Search&limit=5000&offset=0&profile=default&search=%22coronavirus+pandemic+in%22&advancedSearch-current={}&ns0=1" #here's a search querry that captures both countries and places
wikipedia_by_country_webpage <- read_html(url)
links <- wikipedia_by_country_webpage %>% html_nodes("a") %>% html_attr(c('href'))
titles <- wikipedia_by_country_webpage %>% html_nodes("a") %>% html_attr(c('title')) 
text <- wikipedia_by_country_webpage %>% html_nodes("a") %>% html_text() 
wikipedia_by_country_links <- data.frame(link=paste0("https://en.wikipedia.org/",links) , 
                                         title=titles,
                                         text=text) %>% 
  filter(!is.na(title)) %>% 
  filter(!str_detect(title,"Template")) %>% 
  filter(!str_detect(title,"Timeline")) %>% 
  filter(!str_detect(title,"Chronology of the|Economic impact|Responses to|page does not exist")) %>% 
  
  filter(str_detect(title,"coronavirus pandemic in")) %>%
  mutate(is_country = text %in% countries_for_matching) %>%
  mutate_if(is.factor, as.character) %>%
  mutate(place=str_replace(title, ".*coronavirus pandemic in ","")) %>%
  distinct()
dim(wikipedia_by_country_links) #345   4
tables_keep_list <- list()
for(i in 1:nrow(wikipedia_by_country_links)){
  temp_place=wikipedia_by_country_links$place[i]
  print(temp_place)
  temp_html <- read_html(wikipedia_by_country_links$link[i])
  temp_tables <- temp_html  %>% html_nodes("table") %>%  html_table(fill = TRUE, header=F) 
  
  test <- temp_tables[[10]] %>%  html_table(fill = TRUE, header=F) 
  test2 <- temp_tables[[10]] %>%  html_table(fill = F, header=F) #nope still throws an error
  
  library(htmltab) ; #install.packages('htmltab')
  url <- wikipedia_by_country_links$link[i]
  ukLang <- htmltab(doc = url, which = 1)
  ukLang <- htmltab(doc = url, which = 10, complementary=F, headerSep="_")
  
  head(ukLang)
  
  temp_tables_df <- lapply(temp_tables, FUN=function(x) { 
    tryCatch({
      html_table(x,fill = TRUE, header=F)} ,
      error = function(e) return(data.frame()) 
    )   } )  #the one I want is throwing an error
  
  condition <- which(sapply(temp_tables_df, ncol)>3 & sapply(temp_tables_df, nrow)>3)
  temp_tables_deaths <- temp_tables_df[condition]
  #do any of these tables have the word death
  condition <- which(  sapply(temp_tables_deaths, FUN=function(x) sum(str_detect(unique(unlist(x)), "death|Death"), na.rm=T)  )>0)
  temp_tables_deaths <- temp_tables_deaths[condition]
  condition <- which(  sapply(temp_tables_deaths, FUN=function(x) sum(str_detect(unique(unlist(x)), "Data \\(templates\\)|Medical professional|SARS-CoV-2 \\(virus\\) COVID-19 \\(disease\\)"), na.rm=T)  )==0)
  temp_tables_deaths <- temp_tables_deaths[condition]
  
  #lapply(temp_tables_deaths, dim)
  #lapply(temp_tables_deaths, colnames)
  tables_keep_list[[temp_place]] <- temp_tables_deaths
  temp1 <- temp_tables_deaths[[1]]
  temp2 <- temp_tables_deaths[[2]]
  temp3 <- temp_tables_deaths[[3]]
  temp4 <- temp_tables_deaths[[4]]
}


country_map <- wikipedia_by_country_links[,c(5,1)]
