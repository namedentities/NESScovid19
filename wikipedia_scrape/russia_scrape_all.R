library(rvest)  
library(stringr)
library(dplyr)
library(purrr)
library(tidyr)
library(lubridate)

q = "https://en.wikipedia.org/wiki/Template:2019%E2%80%9320_coronavirus_pandemic_data/Russia_medical_cases"


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

temp_tables <- read_html(q)  %>% html_nodes("table") #%>%  html_table(fill = TRUE)   

temp_tables = filter_wikitables(temp_tables)

tables_list <- list()
try({   
  for(h in temp_tables){
    h = html_table(h,fill = TRUE,header = TRUE)
    print(tolower(names(h)))
    if(     
        names(h) %>% tolower() %>% str_detect( pattern=c("tests")) %>% any(na.rm = TRUE)    
    ){  
      tables_list[[q]] <- h
      print("Table Found")
    }   
  }
})

df = tables_list[[1]]

# Have to do this because table reads wrong headers.
cols = list()
for(each in df[1,]){
  if(each != "Central"){
    cols = append(cols,each)
  }
}
cols = append(cols,"tests")
cols = append(cols,"sources")

colnames(df) <- cols
colnames(df)[87] <- "confirmed new"
colnames(df)[88] <- "confirmed total"
colnames(df)[89] <- "recovered new"
colnames(df)[90] <- "recovered total"
colnames(df)[91] <- "deaths new"
colnames(df)[92] <- "deaths total"
colnames(df)[93] <- "tests"
colnames(df)[94] <- "sources"

colnames(df) <- lapply(X = colnames(df), FUN = function(t) gsub('\\[.\\]' ,'', x = t))

df = df[2:(dim(df)[1]-3),]

for(col in colnames(df)){
  df[,col] = trimws(df[,col])
  for(i in 2:length(df[,col]) - 1){
    df[i,col] = gsub('\\[.\\]','',df[i,col])
    df[i,col] = gsub(',','',df[i,col])
    df[i,col] = gsub('[-|,]','',df[i,col],fixed = TRUE)
    x1 = df[i,col]
    x2 = df[i+1,col]
    if(x2 == ""){
      df[i+1,col] = x1
    }
  }
}

provinces = colnames(df)[2:86]
df = pivot_longer(df,provinces,names_to= 'Province',values_to= "No. of confirmed cases")

df$Date = as.Date(paste(df$Date,"2020"), format = "%d %B %Y")
df$sources = NULL

russia_wikipedia_cases <- df
 
russia_wikipedia_cases_long <- russia_wikipedia_cases %>% janitor::clean_names() %>%
  mutate(date_asdate = ymd(date)) %>%
   mutate(dataset="wikipedia") %>%
 
   mutate(admin0_name_clean= "Russia" %>% rex_clean()) %>%
   mutate(admin1_name_clean= province %>% rex_clean() ) %>%
   mutate(admin2_name_clean= '' ) %>%
   rex_admin_function()
 
 forjoining_russia_wikipedia_cases <- russia_wikipedia_cases_long %>% dplyr::select(dataset,gid, geonameid,wikidata_id,date_asdate, confirmed=no_of_confirmed_cases)  %>%
   mutate(confirmed=confirmed %>% str_replace_all(",","") %>% as.numeric() )
 
 write.csv(russia_subnation_cases,"~/Desktop/mssl/NESScovid19/wikipedia_scrape/russia_scrape_all.csv")