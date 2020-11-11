
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

png(width=100, height =100)
t_F_o = tm_shape(ra.new.o)+
  tm_polygons(col='precip',title="Rainfall (mm)")+
  tm_legend(position=c("right","bottom"))+
  tm_shape(One)+
  tm_lines(col='red')+
  tm_layout(main.title='One-2009',main.title.position = "center") 
t_F_o
dev.off()


ui <- fluidPage(
  title = "Examples of Data Tables",
  sidebarLayout(
    tabsetPanel(
      conditionalPanel(
        'input.dataset === "hurr_tracks"'),
      conditionalPanel(
        'input.dataset === "hurr_tracks"'),
      conditionalPanel(
        'input.dataset === "hurr_tracks"',
      )
    ),
    mainPanel(
      tabsetPanel(
        id = 'dataset',
        tabPanel("Storm tracks for Atlantic basin storms data table",
                 plotOutput(outputId = "t_F_o", width="100%"),
                 
                 # Create a new Row in the UI for selectInputs
                 fluidRow(
                   column(4,
                          selectInput("storm",
                                      "Storm ID:",
                                      c("All",
                                        unique(as.character(hurr_tracks$storm_id))))
                   ),
                   column(4,
                          selectInput("storm",
                                      "Storm ID:",
                                      c("All",
                                        unique(hurr_tracks$storm_id)))
                   )
                 ),
                 # Create a new row for the table.
                 DT::dataTableOutput("table1")),
                 tabPanel("Rainfall for US counties during Atlantic basin tropical storms data table",
                
                 # Create a new Row in the UI for selectInputs
                 fluidRow(
                   column(4,
                          selectInput("storm",
                                      "Storm ID:",
                                      c("All",
                                        unique(as.character(rain$storm_id))))
                   ),
                   column(4,
                          selectInput("fips",
                                      "Fips:",
                                      c("All",
                                        unique(rain$fips)))
                   )
                ),
                # Create a new row for the table.
                DT::dataTableOutput("table2")))
          )
      )   
  )

server <- function(input, output) {
  
  # Filter data based on selections
  output$table1 <- DT::renderDataTable(DT::datatable({
    data <- hurr_tracks
    if (input$storm != "All") {
      data <- data[data$storm_id == input$storm,]
    }
    if (input$year != "All") {
      data <- data[data$storm_id == input$storm,]
    }
    
    data
  }))
  
  # sorted columns are colored now because CSS are attached to them
  # Filter data based on selections
  output$table2 <- DT::renderDataTable(DT::datatable({
    data2 <- rain
    if (input$storm != "All") {
      data2 <- data2[rain$storm_id == input$storm,]
    }
    if (input$fips != "All") {
      data2 <- data2[rain$fips == input$fips,]
    }
    
    data2
  }))
  
  
}

# Run the application 
shinyApp(ui = ui, server = server)                

        
        
  

