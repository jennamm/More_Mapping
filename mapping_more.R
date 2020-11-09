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
dt2 <- hurr_tracks %>% filter(storm_id == "Alex-2010")

ra <- rain %>% filter(storm_id == "Earl-2010") %>% group_by(fips) %>%
  summarise('storm_id'=storm_id[1],'precip'=sum(precip))

ra2 <-rain %>% filter(storm_id == "Alex-2010") %>% group_by(fips) %>%
  summarise('storm_id'=storm_id[1],'precip'=sum(precip))

ra$fips <- as.numeric(ra$fips)
ra2$fips <- as.numeric(ra2$fips)

#merge data by fips variable
ra <- merge(ob, ra,by="fips")
ra2 <- merge(ob, ra2, by="fips")

Earl <- cbind(dt$longitude,dt$latitude)%>%
  Line()%>%Lines(ID='Earl-2010')%>%
  list()%>%SpatialLines()

Alex <- cbind(dt2$longitude, dt2$latitude)%>%
  Line()%>%Lines(ID='Alex-2010')%>%
  list()%>%SpatialLines()


# get map data

ra <- rain %>% filter(storm_id == "Earl-2010") %>% group_by(fips) %>%
  summarise(storm_id=storm_id[1],precip=sum(precip))
ra.new <- as.data.frame(ra)
ra.new$fips <- as.numeric(ra.new$fips)

ra2<- rain%>% filter(storm_id =="Alex-2010") %>% group_by(fips) %>%
  summarise(storm_id=storm_id[1], precip=sum(precip))
ra.new2 <- as.data.frame(ra2)
ra.new2$fips <- as.numeric(ra.new2$fips)

# merge data by fips variable

ra.new <- merge(ob, ra.new, by="fips")
ra.new2 <- merge(ob, ra.new2, by="fips")


ra.new$ID[ra.new$ID =="massachusetts,worcester"|ra.new$ID=="massachusetts,middlesex"|ra.new$ID=="massachusetts,essex"
          |ra.new$ID=="massachusetts,suffolk"|ra.new$ID=="massachusetts,norfolk"|ra.new$ID=="massachusetts,plymouth"
          |ra.new$ID=="massachusetts,bristol"|ra.new$ID=="massachusetts,barnstable"|ra.new$ID=="massachusetts,dukes"
          |ra.new$ID=="north carolina,halifax"|ra.new$ID=="north carolina,northampton"|ra.new$ID=="north carolina,hertford"
          |ra.new$ID=="north carolina,gates"|ra.new$ID=="north carolina,camden"|ra.new$ID=="north carolina,currituck"
          |ra.new$ID=="north carolina,pasquotank"|ra.new$ID=="north carolina,perquimans"|ra.new$ID=="north carolina,chowan"
          |ra.new$ID=="north carolina,bertie"|ra.new$ID=="north carolina,washington"|ra.new$ID=="north carolina,tyrrell"
          |ra.new$ID=="north carolina,edgecombe"|ra.new$ID=="north carolina,martin"|ra.new$ID=="north carolina,beaufort"
          |ra.new$ID=="north carolina,hyde"|ra.new$ID=="north carolina,pitt"|ra.new$ID=="north carolina,greene"
          |ra.new$ID=="north carolina,lenoir"|ra.new$ID=="north carolina,craven"|ra.new$ID=="north carolina,pamlico"
          |ra.new$ID=="north carolina,jones"|ra.new$ID=="north carolina,carteret"|ra.new$ID=="north carolina,duplin"
          |ra.new$ID=="north carolina,onslow"|ra.new$ID=="north carolina,pender"|ra.new$ID=="north carolina,new hanover"
          |ra.new$ID=="north carolina,columbus"|ra.new$ID=="north carolina,brunswick"
          |ra.new$ID=="north carolina,dare"] <-"Public Assistance"
ra.new$ID[ra.new$ID != "Public Assistance"] <- "No Designation"

ra.new2$ID[ra.new2$ID == "texas,val verde"] <- "Individual Assistance"
ra.new2$ID[ra.new2$ID != "Individual Assistance"] <- "No Designation"

#Plotting tmap
t_F = tm_shape(ra.new)+
  tm_polygons(col='ID',title="Assistance (FEMA)", style="pretty")+
  tm_legend(position=c("right","bottom"))+
  tm_shape(Earl)+
  tm_lines(col='red')+
  tm_layout(main.title='Earl-2010',main.title.position="center")
t_F
t_F2= tm_shape(ra.new2)+
  tm_polygons(col='ID',title="Assistance (FEMA)", style="pretty")+
  tm_legend(position=c("right","bottom"))+
  tm_shape(Alex)+
  tm_lines(col='red')+
  tm_layout(main.title='Alex-2010',main.title.position="center")
t_F2
