
#libraries
library(lubridate)
library(tidyverse)

#devtools::install_github("ropensci/USAboundaries")
#devtools::install_github("ropensci/USAboundariesData")
library(USAboundaries) ; #install.packages('USAboundaries')
data(state_codes)

library(tidyverse)
library(scales)
library(gghighlight)
library(lubridate)
library(R0)  # consider moving all library commands to top -- this one was in a loop below

library(WikidataR)
library(countrycode)

library(usmap) ; # install.packages('usmap')
data(statepop)
#devtools::install_github("ropensci/USAboundaries")
#devtools::install_github("ropensci/USAboundariesData")
library(USAboundaries) ; #install.packages('USAboundaries')
data(state_codes)

library(tidyverse)
library(sf)

library(jsonlite)

lhs_long <- readRDS("/media/skynet2/905884f0-7546-4273-9061-12a790830beb/rwd_github_private/NESScovid19/data_temp/lhs_long.Rds")

#This is too slow it's downloading each
library(GADMTools)

#gadm36 = st_read("/media/skynet2/905884f0-7546-4273-9061-12a790830beb/rwd_github_private/NESSgadm/data_in/gadm36_gpkg/gadm36.gpkg")
st_layers("/media/skynet2/905884f0-7546-4273-9061-12a790830beb/rwd_github_private/NESScovid19/data_temp/gadm36_bycountry/gadm36_levels_gpkg/gadm36_levels.gpkg")
gadm36_levels_0 = st_read("/media/skynet2/905884f0-7546-4273-9061-12a790830beb/rwd_github_private/NESScovid19/data_temp/gadm36_bycountry/gadm36_levels_gpkg/gadm36_levels.gpkg", layer="level0")  %>%
                  st_simplify(preserveTopology = FALSE, dTolerance =0.1) #  0.025 this is supposedly broken up by 6 levels and so should have u.s. 

#plot(gadm36_levels_0)
#dim(gadm36_levels_0_sf$sf)
#gadm_plot(gadm36_levels_0_sf)
lhs_long_place_sources <- lhs_long  %>% dplyr::select(gid,   geonameid, wikidata_id, dataset) %>% 
                           group_by(gid,  geonameid, wikidata_id) %>% count(dataset) %>%
                           group_by(gid,  geonameid, wikidata_id) %>%
                           summarise(datasets_n=n()) 

p0 <- gadm36_levels_0 %>% 
    left_join(lhs_long_place_sources %>% dplyr::select(gid=gid, datasets_n) %>%  left_join( gadm36_levels_0 %>% as.data.frame() %>% dplyr::select(gid=GID_0, NAME_0)  %>% distinct()  )
              ) %>%
    #replace_na(list(datasets_n = 0)) %>% 
    ggplot() + geom_sf(aes(fill = datasets_n)) +
    scale_fill_gradient(low="blue", high="red") +
    theme_bw() 
p0


gadm36_levels_1 = st_read("/media/skynet2/905884f0-7546-4273-9061-12a790830beb/rwd_github_private/NESScovid19/data_temp/gadm36_bycountry/gadm36_levels_gpkg/gadm36_levels.gpkg", layer="level1")  %>%
  st_simplify(preserveTopology = FALSE, dTolerance =0.1) #  0.025 this is supposedly broken up by 6 levels and so should have u.s. 


p1 <- gadm36_levels_1 %>% 
  left_join(lhs_long_place_sources %>% dplyr::select(gid=gid, datasets_n) %>%  left_join( gadm36_levels_1 %>% as.data.frame() %>% dplyr::select(gid=GID_1, NAME_1) %>% distinct()  )
  ) %>%
  #replace_na(list(datasets_n = 0)) %>% 
  ggplot() + geom_sf(aes(fill = datasets_n)) +
  scale_fill_gradient(low="blue", high="red") +
  theme_bw() 
p1



gadm36_levels_2 = st_read("/media/skynet2/905884f0-7546-4273-9061-12a790830beb/rwd_github_private/NESScovid19/data_temp/gadm36_bycountry/gadm36_levels_gpkg/gadm36_levels.gpkg", layer="level2")  %>%
  st_simplify(preserveTopology = FALSE, dTolerance = 0.001) #  0.025 this is supposedly broken up by 6 levels and so should have u.s. I have to keep shrinking it so misisng goes to zero 

p2 <- gadm36_levels_2 %>% 
  left_join(lhs_long_place_sources %>% dplyr::select(gid=gid, datasets_n) %>%  left_join( gadm36_levels_2 %>% as.data.frame() %>% dplyr::select(gid=GID_2, NAME_2) %>% distinct()  )
  ) %>%
  #replace_na(list(datasets_n = 0)) %>% 
  ggplot() + geom_sf(aes(fill = datasets_n),lwd = 0) +
  scale_fill_gradient(low="blue", high="red") +
  theme_bw() 
p2

