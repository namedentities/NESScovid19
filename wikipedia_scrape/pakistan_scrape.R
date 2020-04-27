# Pakistan Scrape

library(rvest)	
library(stringr)
library(dplyr)

q = "https://en.wikipedia.org/wiki/Template:2019%E2%80%9320_coronavirus_pandemic_data/Pakistan_medical_cases"

temp_tables <- read_html(q)  %>% html_nodes("table") %>%  html_table(fill = TRUE) 	
tables_list <- list()
try({	
  for(h in temp_tables){	
    if( 	
      names(h) %>% tolower() %>% str_detect( pattern=c("confirmed")) %>% any(na.rm = TRUE,header = TRUE) #it has a stupid extra header that hangs it up, so added search term for the header	
    ){	
      tables_list[[q]] <- h
      print("Table Found")	
      break
    }	
  }
})

df = tables_list[[1]]
df[1,which(names(df) == "Confirmed")] <- paste("Total Confirmed",df[1,which(names(df) == "Confirmed")])
df[1,which(names(df) == "Deaths")] <- paste("Total Deaths",df[1,which(names(df) == "Deaths")])
df[1,which(names(df) == "Recovered")] <- paste("Total Recovered",df[1,which(names(df) == "Recovered")])
df = df[!df[, 1] == df[, 2],] # Cleaning rows with column names
colnames(df) <- df[1,]
df = df[2:dim(df)[1],]
df = pivot_longer(df,c("Punjab","Sindh","Khyber Pakhtunkhwa","Balochistan","Gilgit-Baltistan","Azad Kashmir","Islamabad"),names_to= 'Province',values_to= "No. of confirmed cases")
df$Date = as.Date(paste(df$Date,"2020"), format = "%B %d %Y")
df=df[!is.na(df$Date), ]


write.csv(df,"~/Desktop/mssl/NESScovid19/wikipedia_scrape/pakistan_scrape.csv")