
pacman::p_load("tidyverse","drat","maps","tmap","sf","viridis","sp","hurricaneexposuredata","hurricaneexposure","tmaptools")
addRepo("geanders")
data("hurr_tracks") 
data("rain")

# get map data
ob <- st_as_sf(map('county',plot=F,fill=T))
colnames(county.fips)[2] = 'ID'
ob <- merge(ob, county.fips, by="ID")

# filter Alex and Earl in hurr_tracks data and rain data
dt_o <- hurr_tracks %>% filter(storm_id == "One-2009")
dt <- hurr_tracks %>% filter(storm_id == "Earl-2010")
dt_i <- hurr_tracks %>% filter(storm_id == "Irene-2011")


ra_o <- rain %>% filter(storm_id == "One-2009") %>% group_by(fips) %>%
  summarise('storm_id'=storm_id[1],'precip'=sum(precip))

ra <- rain %>% filter(storm_id == "Earl-2010") %>% group_by(fips) %>%
  summarise('storm_id'=storm_id[1],'precip'=sum(precip))

ra_i <- rain %>% filter(storm_id == "Irene-2011") %>% group_by(fips) %>%
  summarise('storm_id'=storm_id[1],'precip'=sum(precip))



ra_o$fips <- as.numeric(ra_o$fips)
ra$fips <- as.numeric(ra$fips)
ra_i$fips <- as.numeric(ra_i$fips)


#merge data by fips variable
ra <- merge(ob, ra, by="fips")
ra_i <- merge(ob, ra_i, by="fips")
ra_o <- merge(ob, ra_o, by="fips")


Earl <- cbind(dt$longitude,dt$latitude)%>%
  Line()%>%Lines(ID='Earl-2010')%>%
  list()%>%SpatialLines()

Irene <- cbind(dt_i$longitude,dt_i$latitude)%>%
  Line()%>%Lines(ID='Irene-2011')%>%
  list()%>%SpatialLines()

One <- cbind(dt_o$longitude,dt_o$latitude)%>%
  Line()%>%Lines(ID='One-2009')%>%
  list()%>%SpatialLines()


# get map data

ra <- rain %>% filter(storm_id == "Earl-2010") %>% group_by(fips) %>%
  summarise(storm_id=storm_id[1],precip=sum(precip))
ra.new <- as.data.frame(ra)
ra.new$fips <- as.numeric(ra.new$fips)

ra_i <- rain %>% filter(storm_id == "Irene-2011") %>% group_by(fips) %>%
  summarise(storm_id=storm_id[1],precip=sum(precip))
ra.new.i <- as.data.frame(ra_i)
ra.new.i$fips <- as.numeric(ra.new.i$fips)

ra_o <- rain %>% filter(storm_id == "One-2009") %>% group_by(fips) %>%
  summarise(storm_id=storm_id[1],precip=sum(precip))
ra.new.o <- as.data.frame(ra_o)
ra.new.o$fips <- as.numeric(ra.new.o$fips)

#make a new data frame that only include Hurricane One, Hurricane Earl, and Hurricane Irene
all.data.rain <- rain %>% filter(storm_id=="One-2009"|storm_id=="Earl-2010"|storm_id=="Irene-2011")
all.data.rain <- data.frame(all.data.rain[1],all.data.rain[2],all.data.rain[4], all.data.rain[5], all.data.rain[6])

# merge data by fips variable

ra.new <- merge(ob, ra.new, by="fips")
ra.new.i <- merge(ob, ra.new.i, by="fips")
ra.new.o <- merge(ob, ra.new.o, by="fips")

t_F_e = tm_shape(ra.new)+
  tm_polygons(col='precip',title="Rainfall (mm)")+
  tm_legend(position=c("right","bottom"))+
  tm_shape(Earl)+
  tm_lines(col='red')+
  tm_layout(main.title='Earl-2010',main.title.position = "center") 
t_F_e

t_F_i = tm_shape(ra.new.i)+
  tm_polygons(col='precip',title="Rainfall (mm)")+
  tm_legend(position=c("right","bottom"))+
  tm_shape(Irene)+
  tm_lines(col='red')+
  tm_layout(main.title='Irene-2011',main.title.position = "center") 
t_F_i

t_F_o = tm_shape(ra.new.o)+
  tm_polygons(col='precip',title="Rainfall (mm)")+
  tm_legend(position=c("right","bottom"))+
  tm_shape(One)+
  tm_lines(col='red')+
  tm_layout(main.title='One-2009',main.title.position = "center") 
t_F_o


