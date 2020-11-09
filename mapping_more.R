library(hurricaneexposuredata)
library(hurricaneexposure)
library(tmap)
library(sf)
library(tmaptools)

pacman::p_load("tidyverse","drat","maps","tmap","sf","viridis","sp")


addRepo("geanders")

data("hurr_tracks") 

data("rain")

head(hurr_tracks)

head(rain)

# get map data
ob <- st_as_sf(map('county',plot=F,fill=T))
colnames(county.fips)[2] = 'ID'
ob <- merge(ob, county.fips, by="ID")

# filter Floyd-1999 in hurr_tracks data and rain data
dt <- hurr_tracks %>% filter(storm_id == "Earl-2010")

ra <- rain %>% filter(storm_id == "Earl-2010") %>% group_by(fips) %>%
  summarise('storm_id'=storm_id[1],'precip'=sum(precip))
ra$fips <- as.numeric(ra$fips)

#merge data by fips variable
ra <- merge(ob, ra,by="fips")

Earl <- cbind(dt$longitude,dt$latitude)%>%
  Line()%>%Lines(ID='Earl-2010')%>%
  list()%>%SpatialLines()


# get map data

ra <- rain %>% filter(storm_id == "Earl-2010") %>% group_by(fips) %>%
  summarise(storm_id=storm_id[1],precip=sum(precip))
ra.new <- as.data.frame(ra)
ra.new$precip[ra.new$precip<=5] <- 'Unexposed'
ra.new$precip[ra.new$precip != "Unexposed"] <- 'Exposed'

ra.new$fips <- as.numeric(ra.new$fips)

# merge data by fips variable

ra.new <- merge(ob, ra.new,by="fips")

Earl <- cbind(dt$longitude,dt$latitude)%>%
  Line()%>%Lines(ID='Earl-2010')%>%
  list()%>%SpatialLines()



#Plotting tmap
t_F = tm_shape(ra.new)+
  tm_polygons(col='precip',title="Rain > 100 (mm)", style="pretty")+
  tm_legend(position=c("right","bottom"))+
  tm_shape(Earl)+
  tm_lines(col='red')+
  tm_layout(main.title='Earl-2010',main.title.position="center")
t_F
