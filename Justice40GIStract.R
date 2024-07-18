#install.packages("ggmap")
#install.packages("sf")
#install.packages("leaflet")
library(sf)
library(ggmap)
library(ggplot2)
library(dplyr)
library(tidyr)
library(tidyverse)
library(readr)
library(leaflet)
library(RColorBrewer)

##Grabbed from https://data.cityofnewyork.us/dataset/Hyperlocal-Temperature-Monitoring/qdq3-9eqn/data 
Hyperlocal_Temperature_Monitoring <- read_csv("Hyperlocal_Temperature_Monitoring.csv")
View(Hyperlocal_Temperature_Monitoring)
ModiData<-filter(Hyperlocal_Temperature_Monitoring,grepl("08/2019",Day))
# Initialize vectors to store temperatures and mean temperatures
Temps <- c()
MeanTemps <- c()
# Iterate over the rows of ModiData using a for loop with indices
for (i in 1:(nrow(ModiData) - 1)) {
  # Get the current and next sensor IDs
  name <- ModiData$Sensor.ID[i]
  name2 <- ModiData$Sensor.ID[i + 1]
  # Append the current temperature to Temps
  Temps <- c(Temps, ModiData$AirTemp[i])
  # Check if the current and next sensor IDs are different
  if (name != name2) {
    # Calculate the mean of Temps if it is not empty
    if (length(Temps) > 0) {
      Mean <- mean(Temps)
      MeanTemps <- c(MeanTemps, Mean)
    }
    # Reset Temps for the next group
    Temps <- c()
  }
}
# Handle the case for the last group if Temps is not empty
if (length(Temps) > 0) {
  Mean <- mean(Temps)
  MeanTemps <- c(MeanTemps, Mean)
}
# Convert MeanTemps to a single-column data frame
MeanTemps_df <- data.frame(MeanTemps = MeanTemps)

# Print the resulting data frame
print(MeanTemps_df)

##Convert ModiData into an individual sensor name, then gluing it to the calculated Mean Temp associated with it
ModiData<-ModiData[!duplicated(ModiData$Sensor.ID), ]
ModiData<-bind_cols(ModiData,MeanTemps_df)


###Some were from 2018 only? Very weird Stuff TBH, so we're going to make them their own set
UniqueNames<-data.frame(Sensor.ID=(setdiff(Hyperlocal_Temperature_Monitoring$Sensor.ID,ModiData$Sensor.ID)))
UniqueNames <- inner_join(UniqueNames, Hyperlocal_Temperature_Monitoring, by = "Sensor.ID")
UniqueNames<-filter(UniqueNames,grepl("08/2018",Day))

# Initialize vectors to store temperatures and mean temperatures
Temps <- c()
MeanTemps <- c()
# Iterate over the rows of ModiData using a for loop with indices
for (i in 1:(nrow(UniqueNames)-1)) {
  # Get the current and next sensor IDs
  name <- UniqueNames$Sensor.ID[i]
  name2 <- UniqueNames$Sensor.ID[i + 1]
  # Append the current temperature to Temps
  Temps <- c(Temps, UniqueNames$AirTemp[i])
  # Check if the current and next sensor IDs are different
  if (name != name2) {
    # Calculate the mean of Temps if it is not empty
    if (length(Temps) > 0) {
      Mean <- mean(Temps)
      MeanTemps <- c(MeanTemps, Mean)
    }
    # Reset Temps for the next group
    Temps <- c()
  }
}
# Handle the case for the last group if Temps is not empty
if (length(Temps) > 0) {
  Mean <- mean(Temps)
  MeanTemps <- c(MeanTemps, Mean)
}
# Convert MeanTemps to a single-column data frame
MeanTemps_df <- data.frame(MeanTemps = MeanTemps)

# Print the resulting data frame
print(MeanTemps_df)

###Same thing as earlier
UniqueNames<-UniqueNames[!duplicated(UniqueNames$Sensor.ID), ]
UniqueNames<-bind_cols(UniqueNames,MeanTemps_df)


####Leaflet wasn't working so I was wondering if it had to do with floating?
ModiData$MeanTemps <- round(ModiData$MeanTemps, 3)
UniqueNames$MeanTemps <- round(UniqueNames$MeanTemps, 3)

###Leaflet Wasn't working, so I fixed it by removing NA, and then converting the numbers to be read as strings, which is kind of annoying, but whatever
ModiData <- ModiData %>%
  mutate_all(~ ifelse(is.na(.), 0, .))
UniqueNames <- UniqueNames %>%
  mutate_all(~ ifelse(is.na(.), 0, .))
ModiData <- ModiData %>%
  mutate(MeanTemps = as.character(MeanTemps))
UniqueNames<- UniqueNames %>%
  mutate(MeanTemps=as.character(MeanTemps))
head(UniqueNames)
###Recombine 2018 & 2019 Data, now with their Mean Temperatures Attached
FinalDF<-bind_rows(UniqueNames,ModiData)
points_sf <- st_as_sf(FinalDF, coords = c("Longitude", "Latitude"), crs =4326)
rm(MeanTemps)
###Introduce Justice40Tracts Shape Data (Grabbed from NOFO Smart Grant 2024)
ShapeFile<-st_read("usa.shp")
ny_shape <- ShapeFile[ShapeFile$SF == "New York", ]
NYC_shape<-ny_shape[ny_shape$CF=="Bronx County"|ny_shape$CF=="Queens County"|ny_shape$CF=="New York County"|ny_shape$CF=="Kings County", ]
NYC_shape <- st_transform(NYC_shape, crs = 4326)
NYC_shapeJustice<-filter(NYC_shape,NYC_shape$SN_C==1)
###World Polygon Son, just for contrast
world_polygon <- st_as_sfc(st_bbox(c(xmin = -180, ymin = -90, xmax = 180, ymax = 90)), crs = st_crs(nyc_shape))
JusticeMap<-leaflet() %>%
  addTiles() %>%  # Add default OpenStreetMap map tiles
  addPolygons(data = world_polygon, color = NA, fillColor = "black", fillOpacity = 0.6) %>%
  addPolygons(data = NYC_shapeJustice, color = "lightblue", weight = 1, fillOpacity = 0.5)%>% addCircleMarkers(data = ModiData, lng=~Longitude, lat=~Latitude,
                   color = "red", radius = 0.2, popup = ~MeanTemps)%>%
  addCircleMarkers(data=UniqueNames, lng=~Longitude, lat=~Latitude, color="orange", radius=0.2,popup=~MeanTemps) %>%  addLegend("bottomright", colors = c("red", "orange","lightblue"),labels = c("2019, Mean August Temp", "2018, Mean August Temp","Justice 40 Tract"), title = "Mean Temperatures") %>%
  setView(lng = -73.91691, lat = 40.66621, zoom = 12)  # Center the map on NYC

###So We're gonna be measuring a bunch based on Percentiles
####So We have the leaflet made by the Justice 40 tracts but there are a lot of other tracts I want to measure by let's uh see... Diabetes: https://www.ncbi.nlm.nih.gov/pmc/articles/PMC10039694

NYC_shapeDiabetes <- NYC_shape %>% ###To Create a % based number as opposed to a decimal one
  mutate(DiabetesPercent = DF_PFS * 100)

###Palette Creation, that references the %'s of Diabetes to determine gradient
Diabetespal=colorNumeric( palette = brewer.pal(9,"Purples"),
domain= NYC_shapeDiabetes$DiabetesPercent)

###Now just for the Simplified Boolean version
NYC_shapeDiabetesBoolean<-filter(NYC_shapeDiabetes,DLI==1) #Filter where Diabetes is considered to be in top 10 Percentiles, see Codebook 

DiabetesMapBoolean<-leaflet(NYC_shapeDiabetesBoolean) %>% ###Making the Map
  addTiles() %>%  # Add default OpenStreetMap map tiles
  addPolygons(data = world_polygon, color = NA, fillColor = "black", fillOpacity = 0.6) %>% ###Adds the black background
  addPolygons(data = NYC_shapeDiabetesBoolean, color="purple", weight = 1, fillOpacity = 0.5)%>% addCircleMarkers( color = "red", radius = 0.2, popup = ~MeanTemps) %>% 
  addCircleMarkers(data=UniqueNames, lng=~Longitude, lat=~Latitude, color="orange", radius=0.2,popup=~MeanTemps) %>%  addLegend("bottomright", colors = c("red", "orange"),labels = c("2019, Mean August Temp", "2018, Mean August Temp"), title = "Mean Temperatures")
####Unlikely that Diabetes is related to Placement

###Just Practice Test Case is completed, time to move into things that may actually matter... Such as Asthma
####Next Step, We kind of realized where we're at, so maybe I can generate these graphs Dynamically?

EverythingWeWantToTest<-c("P200_I_PFS","P100_PFS",
"DF_PFS","AF_PFS","HDF_PFS","DSF_PFS","EBF_PFS","LLEF_PFS","PM25F_PFS","NPL_PFS","TSDF_PFS","IS_PFS") ###Testing all of the following Variables
CleanNames<-c("DoublePoverty","PovertyLine","Diabetes","Asthma","HeartDisease","Diesel","EnergyBurden","Low-LifeExpectancy","PM25","SuperFundProximity","HazardousWasteProximity","Imperviousness") ###This is for Simplified Naming Conventions Later
Counter=0 ###Also used for Later because [i] is attached directly to the variable and is not a number in itself
Brewski<-c("Blues","Purples","Greens","Oranges","Reds","PuRd","YlGn","Greys",'BuPu') ###Color Compound List
for (i in EverythingWeWantToTest) ###Declares the For Loop, 
  {
  Counter=Counter+1
Random_color<-sample(Brewski,1) ###Take a Random Color so not all my Maps Look the Same
column_name<- paste0(i,"Percent") #####This is creating a String Right now that will be used to read off of the Main DataFrames Column Later
NYC_shape<-NYC_shape %>% mutate(!!column_name:=!!sym(i)*100)  ###Creating a New Column using a !!, which is our % as described earlier
palette_name<-(paste0(i,"pal")) ###Doing the Same Thing but for Color
palette<-colorNumeric(palette=brewer.pal(9,Random_color), domain=NYC_shape[[column_name]]) ###Double Indexing to refer to the actual DataPoints, not just the Column as a whole
assign(palette_name,palette)
###Creating a Named Palette for later
map_name<-paste0(CleanNames[Counter],"Map")
map<-leaflet(NYC_shape) %>% 
  addTiles() %>%
addPolygons(data=NYC_shape, color=~ "black", fillColor=~palette(NYC_shape[[column_name]]), weight=1, fillOpacity=0.7,opacity=0.2,popup=~paste("ID:", CF,"<br> Percentile:",NYC_shape[[column_name]])) %>%  addCircleMarkers(data = ModiData, lng=~Longitude, lat=~Latitude, color = "red", radius = 0.2, popup = ~MeanTemps) %>% 
addCircleMarkers(data=UniqueNames, lng=~Longitude, lat=~Latitude, color="orange",radius=0.2,popup=~MeanTemps) %>% setView(lng = -73.91691, lat = 40.66621, zoom = 12) %>%
  addLegend("bottomright", colors = c("red", "orange"),labels = c("2019, Mean August Temp", "2018, Mean August Temp"), title = paste(CleanNames[Counter]))
###Makes the Map

###Creates the Actual Maps
assign(map_name,map)}
print(PovertyLineMap)
print(DoublePovertyMap)
### Alright, Now we're going to make a simplified boolean of 90th percentilers for them

EverythingWeWantToTest<-c("SN_C","A_ET","DS_ET","EB_ET","PM25_ET","NPL_ET","TSDF_ET","TP_ET","HRS_ET","N_CLT_EOMI","N_ENY_EOMI","N_PLN_EOMI","N_HLTH_88","ADJ_ET")
CleanNames<-c("Justice","Asthma","Diesel","EnergyBurden","ParticulateMatter","SuperFundProximity","HazardousWasteProximity","TrafficProximitiy","Redlining","ClimateIssues","EnergyIssues","Pollution","HealthIssues","AdjacenttoJustice40Tract")
Counter=0
for (i in EverythingWeWantToTest) {
 Random_color<-sample(c("red","blue","purple","green","darkgreen","pink","magenta","yellow"),1)
NYC_Shape_Modi<-filter(NYC_shape,!!sym(i)==1)
Counter=Counter+1

map_name<-paste0(CleanNames[Counter],"Map90thPercentile")
PercentName<-paste0(CleanNames[Counter],"PercentIn")
map<-leaflet(NYC_Shape_Modi) %>% 
  addTiles() %>%  # Add default OpenStreetMap map tiles
  addPolygons(data = world_polygon, color = NA, fillColor = "black", fillOpacity = 0.6) %>%
  addPolygons(data = NYC_Shape_Modi, color=Random_color, weight = 1, fillOpacity = 0.5) %>% addCircleMarkers(data = ModiData, lng=~Longitude, lat=~Latitude,
                                                                                                                   color = "lightblue", radius = 0.2, popup = ~MeanTemps)%>%
  addCircleMarkers(data=UniqueNames, lng=~Longitude, lat=~Latitude, color="magenta", radius=0.2,popup=~MeanTemps) %>% setView(lng = -73.91691, lat = 40.66621, zoom = 12) %>%
  addLegend("bottomright", colors = c("lightblue", "magenta"),labels = c("2019, Mean August Temp", "2018, Mean August Temp"), title =paste(CleanNames[Counter]) )

within_shape <- st_within(points_sf, NYC_Shape_Modi)
within_shape_matrix <- as.matrix(within_shape)
rows_with_true <- as.matrix(apply(within_shape_matrix,1,any))
FinalBoolean<-data.frame(Bool=rows_with_true)
FinalBoolean<-filter(FinalBoolean,Bool==TRUE)
FinalPercent<-(nrow(FinalBoolean)/423)*100
FinalPercent<-round(FinalPercent,digits=3)
assign(PercentName,FinalPercent)
assign(map_name,map)}
