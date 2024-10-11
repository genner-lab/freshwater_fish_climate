#setwd

#install unusual packages
remotes::install_github("mikejohnson51/AOI")
remotes::install_github("mikejohnson51/climateR")
install.packages('rfishbase')
install.packages("data.table", type = "source", repos = "https://Rdatatable.gitlab.io/data.table")

#load packages
library(rfishbase)
library(dplyr)
library(rgbif)
library(tidyr)
library(data.table)
library(elevatr)
library(climateR)
library(remotes)
library(readr)
library(readxl)
library(sp)
library(AOI)
library(climateR)
library(sf)
library(raster)
library(rasterVis)
library(terra)
library(elevatr)
library(geosphere)

#Section1-----------------Obtaining RivFishTime data----------------------------

#read in RivFishdataframe, and the then add the zeros

RFT_S <- read.csv("RivFishTIME_SurveyTable.csv")
RFT_S <- subset(RFT_S, select=c("TimeSeriesID", "Year", "Species", "Abundance"))
full_RFT_S <- expand.grid(TimeSeriesID = unique(RFT_S$TimeSeriesID), Year = unique(RFT_S$Year), Species = unique(RFT_S$Species))
full_RFT_output <- full_join(full_RFT_S, RFT_S, by = c("TimeSeriesID", "Year", "Species"))
full_RFT_output <- full_RFT_output %>% replace(is.na(.), 0)

summary_RFT <- full_RFT_output %>%
  group_by(TimeSeriesID, Species) %>%
  summarise(mean = mean(Abundance))
colnames(summary_RFT)[3] <- "InTimeSeries"
summary_RFT$InTimeSeries <- ifelse(summary_RFT$InTimeSeries>0, 1, 0)
full_RFT_output2 <- left_join(full_RFT_output,summary_RFT, by=c("TimeSeriesID", "Species"))
full_RFT_output3 <- full_RFT_output2[full_RFT_output2$InTimeSeries > 0, ]

summary_RFT2 <- full_RFT_output3 %>%
  group_by(TimeSeriesID, Year) %>%
  summarise(mean = mean(Abundance))
colnames(summary_RFT2)[3] <- "SamplingYearinSeries"
summary_RFT2$SamplingYearinSeries <- ifelse(summary_RFT2$SamplingYearinSeries>0, 1, 0)
full_RFT_output4 <- left_join(full_RFT_output3,summary_RFT2, by=c("TimeSeriesID", "Year"))
full_RFT_output5 <- full_RFT_output4[full_RFT_output4$SamplingYearinSeries > 0, ]
full_RFT_output5 <- full_RFT_output5 [ , 1:4,]

####
RFT_TS <- read.csv("RivFishTIME_TimeseriesTable.csv")
RFT_TS <- RFT_TS [ , 1:12]

#combine RivFishTime dataframes
RFT_COMB <- left_join(full_RFT_output5,RFT_TS,by="TimeSeriesID")

#clean house
rm(RFT_S)
rm(RFT_TS)
rm(full_RFT_S)
rm(full_RFT_output)
rm(full_RFT_output2)
rm(full_RFT_output3)
rm(full_RFT_output4)
rm(full_RFT_output5)
rm(summary_RFT)
rm(summary_RFT2)

write_csv(RFT_COMB, file="RFT_COMB.csv")
RFT_COMB <- read.csv(file="RFT_COMB.csv")

#Section2-----------------Initial Filtering of RivFishTime data-----------------

#Remove timeseries less than 10 years 
getmode <- function(v) {#create a new function to calculate mode
  uniqv <- unique(v)
  uniqv[which.max(tabulate(match(v, uniqv)))]
}

RFT_FILTERED <- RFT_COMB %>%
  group_by(TimeSeriesID, Species, Year) %>%
  summarise(Avg_Abundance = mean(Abundance),SiteID=getmode(SiteID),HydroBasin=getmode(HydroBasin),Latitude=median(Latitude),Longitude=median(Longitude),BioRealm=getmode(BioRealm))%>%
  mutate(Duration = Year - first(Year)) %>% #calculates duration as year of survey - earliest year in group
  filter(max(Duration)>9)

#remove datapoints from years before 1958
RFT_FILTERED <- filter(RFT_FILTERED,Year>1957)

#clean house
rm(RFT_COMB)

#Section3-----------------Obtaining Fish Ecology Data---------------------------

species_data <- as.data.frame(RFT_FILTERED$Species) #obtain list of species in filtered dataframe
species_data <- unique(species_data) #remove duplicates

species_data$Species <- validate_names(species_data$`RFT_FILTERED$Species`) #create new column in which species names are validated by fishbase
species_input <- as.data.frame(unique(species_data$Species))#removes repeated records (this step was put in due to a taxonomy issue, e.g., G. gymnurus is not recognised as a valid species so changes to G. aculeatus creating a double record)

length_data <- species(species_input$`unique(species_data$Species)`, fields = c("Species","Length"))#extracts data
trophic_data <- estimate(species_input$`unique(species_data$Species)`, fields = c("Species", "Troph")) #extracts trophic level data that appears in estimates section of fishbase

fish_data <- left_join(species_data,trophic_data,by="Species")#join fish data
fish_data <- left_join(fish_data,length_data,by="Species")

#clean house
rm(length_data)
rm(species_data)
rm(species_input)
rm(trophic_data)

#Section4-----------------Obtaining Distribution Data from GBIF-----------------

options(gbif_user = "xxxxxxxxx")
options(gbif_email = "xxxxxxxxx")
options(gbif_pwd = "xxxxxxxxx")

SpeciesList <- fish_data$`RFT_FILTERED$Species`

long_checklist <- SpeciesList

# match the names
gbif_taxon_keys <- long_checklist %>%
  name_backbone_checklist() %>%
  filter(!matchType == "NONE") %>% # get matched names
  pull(usageKey)

# download the data
occ_download(
  pred_in("taxonKey", gbif_taxon_keys),
  pred("hasCoordinate", TRUE),
  format = "SIMPLE_CSV")

#There will now be an option to download the data

#Reading in data, skipping the section5 above

GBIF_Data <- fread ("0005327-240425142415019.csv",sep="\t",fill = TRUE, quote="")

#Section5--------------Estimating Latitude Ranges from GBIF data----------------

#Simplify the dataframe and remove dubious zero latitude data

GBIF_Data_Simple <- GBIF_Data[,c(10,22)]
GBIF_Data_Simple$decimalLatitude <- as.numeric(GBIF_Data_Simple$decimalLatitude)
GBIF_Data_Simple <- filter_if(GBIF_Data_Simple, is.numeric, all_vars((.) != 0))

#Generate a dataframe, list non-tropical species crossing the equator

GBIF_summary <- GBIF_Data_Simple %>%
  group_by(species) %>%
  reframe(qs = quantile(decimalLatitude, c(0.01, 0.99),na.rm=TRUE), prob = c(0.01, 0.99))

GBIF_summary_wide <- spread(GBIF_summary, prob, qs)
colnames(GBIF_summary_wide)[1] <- "species"
colnames(GBIF_summary_wide)[2] <- "Min_Latitude"
colnames(GBIF_summary_wide)[3] <- "Max_Latitude"

Temp <- data.frame(GBIF_summary_wide$species)
Temp$Crossing1 <- ifelse(GBIF_summary_wide$Min_Latitude>0 & GBIF_summary_wide$Max_Latitude>0, "BothNorth","Other")
Temp$Crossing2 <- ifelse(GBIF_summary_wide$Min_Latitude<0 & GBIF_summary_wide$Max_Latitude<0, "BothSouth","Other")
GBIF_summary_wide$Crossing <- ifelse(Temp$Crossing1 == "Other" & Temp$Crossing2 == "Other", "Crossing","NotCrossing")
GBIF_summary_wide$Tropical <- ifelse(GBIF_summary_wide$Max_Latitude > 40 | GBIF_summary_wide$Min_Latitude < -40, "NotTropical","Tropical")
GBIF_summary_wide$NonTrop_EqCross <- ifelse(GBIF_summary_wide$Crossing == "Crossing" & GBIF_summary_wide$Tropical == "NotTropical", "Yes","No")

NonTrop_EqCrossList <- GBIF_summary_wide[ which(GBIF_summary_wide$NonTrop_EqCross=='Yes'), ]
NonTrop_EqCrossList <- subset(NonTrop_EqCrossList,select = c(1))

###Remove species from list as tropical
NonTrop_EqCrossList <- NonTrop_EqCrossList[-c(7,11,15),]

#Make new filtered GBIF list excluding NonTrop_EqCrossList

GBIF_Data_Simple2 <- GBIF_Data_Simple[! (GBIF_Data_Simple$species %in% NonTrop_EqCrossList$species & GBIF_Data_Simple$decimalLatitude < 0), ]

#Estimate the quantiles of filtered data

GBIF_summary_final <- GBIF_Data_Simple2 %>%
  group_by(species) %>%
  reframe(qs = quantile(decimalLatitude, c(0.01, 0.99),na.rm=TRUE), prob = c(0.01, 0.99))
GBIF_summary_final_wide <- spread(GBIF_summary_final, prob, qs)
colnames(GBIF_summary_final_wide)[1] <- "species"
colnames(GBIF_summary_final_wide)[2] <- "Min_Latitude"
colnames(GBIF_summary_final_wide)[3] <- "Max_Latitude"
GBIF_summary_final_wide$Longitude <- 0

#Estimate the geographic range (km) of the species
#consider distm (c(0, dataframe_complete$lat1), c(0, dataframe_complete$lat2), fun = distHaversine

NewFile1 <- st_as_sf(GBIF_summary_final_wide, coords = c("Longitude","Min_Latitude"), crs = 4326) #turn coordinates in dataframe into spatial object
NewFile2 <- st_as_sf(GBIF_summary_final_wide, coords = c("Longitude","Max_Latitude"), crs = 4326) #turn coordinates in dataframe into spatial object
NewFile1$Point1 <- NewFile1$geometry
NewFile2$Point2 <- NewFile2$geometry
GBIF_summary_final_wide$range <- (st_distance(NewFile1$Point1, NewFile2$Point2, by_element = TRUE))/1000
GBIF_summary_final_wide$range <- as.numeric(GBIF_summary_final_wide$range)
GBIF_summary_final_wide <- GBIF_summary_final_wide[ -c(4) ]

#clean house
rm(GBIF_Data)
rm(GBIF_Data_Simple)
rm(GBIF_Data_Simple2)
rm(GBIF_summary)
rm(GBIF_summary_final)
rm(GBIF_summary_wide)
rm(NewFile1)
rm(NewFile2)
rm(Temp)

#Section6---------Second Filter of RivFishTime data-----------------------------

#Filter southern hemisphere records of the non-tropical species that were crossing the equator - NB all filtered species native to northern hemisphere

RFT_FILTERED2 <- RFT_FILTERED[! (RFT_FILTERED$Species %in% NonTrop_EqCrossList$species & RFT_FILTERED$Latitude < 0), ]

#clean house
rm(NonTrop_EqCrossList)
rm(RFT_FILTERED)

#Section7-----------------Obtaining Elevation Data------------------------------

elev_input <- RFT_FILTERED2[,c(7:8)]
elev_input2 <- st_as_sf(elev_input, coords = c("Longitude","Latitude"), crs = 4326)
elev_input_calc <-get_elev_point(locations = elev_input2, units="meters", prj = NULL, src = "aws")
RFT_FILTERED2_Elev <- cbind(RFT_FILTERED2,elev_input_calc$elevation)
names(RFT_FILTERED2_Elev)[11] <- "Elevation"

#clean house
rm(elev_input)
rm(elev_input2)
rm(elev_input_calc)
rm(RFT_FILTERED2)

#Section8---------Downloading the climate data----------------------------------

#this will take days, advise using pre-saved and going to Section 11 below------

#create new dataframe of only year + location data
Tclim_input <- RFT_FILTERED2[,c(3,12,13)]

Tclim_input <- unique(Tclim_input)
Tclim_input <- st_as_sf(Tclim_input, coords = c("Longitude","Latitude"), crs = 4326) #turn coordinates in dataframe into spatial object

Tclim_input['Maximum temperature'] <- NA #add null column for climate data inputs
Tclim_input['Average temperature'] <- NA

#set up objects to act as inputs into climate data request
from_suffix <- "-01-01" #creates character to attach to year, this can be input into function to get climate data
to_suffix <- "-12-01" #as above

#Getting climate data: for each location/year,
#take coordinates and year and input into function which retrieves data from TerraClim,
#take annual average of climate data and input value into dataframe
for (i in 0:91274) {  #repeat code below for each row in spatial df
  
  from_date <- (Tclim_input[i,1]) #get year from dataframe
  from_date <- from_date$Year #drop geometry column
  from_date <- paste(from_date,from_suffix,sep="") #attach year and suffix to create date input
  
  to_date <- (Tclim_input[i,1]) #same process for the end date
  to_date <- to_date$Year
  to_date <- paste(to_date,to_suffix,sep="")
  
  tempmax <-  getTerraClim(Tclim_input[i,2], #AOI = coordinates in row
                           "tmax", #climate parameter
                           from_date, #collect data from Jan of year sampled
                           to_date#collect data from Dec of survey year
  )
  #tempmax['stream_temp']<- NA #apply global regression model that converts air temp to stream temp
  #tempmax$stream_temp <- 32/(1+exp((-0.13*tempmax$tmax)+1.94))
  stream_temp <-as.data.frame(32/(1+exp((-0.13*tempmax$tmax)+1.94)))
  
  Tclim_input[i,3] <- max(stream_temp$`32/(1 + exp((-0.13 * tempmax$tmax) + 1.94))`) #calculates mean of tmax values
  Tclim_input[i,4] <- mean(stream_temp$`32/(1 + exp((-0.13 * tempmax$tmax) + 1.94))`) #inputs mean value into main dataframe
}

#Section9------Unpack the climate data-----------------------------------------

#Requires function below:

sfc_as_cols <- function(x, geometry, names = c("x","y")) {
  if (missing(geometry)) {
    geometry <- sf::st_geometry(x)
  } else {
    geometry <- rlang::eval_tidy(enquo(geometry), x)
  }
  stopifnot(inherits(x,"sf") && inherits(geometry,"sfc_POINT"))
  ret <- sf::st_coordinates(geometry)
  ret <- tibble::as_tibble(ret)
  stopifnot(length(names) == ncol(ret))
  x <- x[ , !names(x) %in% names]
  ret <- setNames(ret,names)
  dplyr::bind_cols(x,ret)
}

#Code for reformatting
Tclim_input <- sfc_as_cols(Tclim_input_temp, st_centroid(geometry))
Tclim_input <-as.data.frame(Tclim_input_temp%>% st_drop_geometry())

colnames(Tclim_input)[4] <- "Longitude"
colnames(Tclim_input)[5] <- "Latitude"

#If wish to save file
#write.csv(Tclim_input, file="Tclim_input.csv")

#Section10----Climate data------------------------------------------------------

#If wish to reload from saved file, skipping sections 9 and 10 above
Tclim_input <- read.csv(file="Tclim_input.csv")
Tclim_input = subset(Tclim_input, select = -c(1) )

colnames(Tclim_input)[4] <- "Longitude"
colnames(Tclim_input)[5] <- "Latitude"

#Section11---------Joining the RivFishTime, Species Data and Elevation Data------

dataframe_complete <- data.frame(RFT_FILTERED2_Elev) #join elevation data with unpacked geometry to allow joining to main dataframe
dataframe_complete <- left_join(dataframe_complete,Tclim_input,by=c('Longitude','Latitude','Year')) #add climate data to main df
colnames(dataframe_complete)[colnames(dataframe_complete) == 'species'] <- 'Species'
colnames(GBIF_summary_final_wide)[colnames(GBIF_summary_final_wide) == 'species'] <- 'Species'
dataframe_complete <- left_join(dataframe_complete,GBIF_summary_final_wide,by="Species")
fish_data <- subset(fish_data, select=c("Species", "Troph", "Length"))
fish_data <- distinct(fish_data)
dataframe_complete <- left_join(dataframe_complete,fish_data,by="Species")

#clean house
rm(RFT_FILTERED2_Elev)
rm(GBIF_summary_final_wide)
rm(Tclim_input)
rm(fish_data)

#Filter out the locations without Minimum and Maximum Latitude data
dataframe_complete <- dataframe_complete[complete.cases(dataframe_complete$Min_Latitude),]

write.table (dataframe_complete, file="dataframe_complete.txt")

dataframe_complete <- read.table("dataframe_complete.txt")

#Section12-----------Relative Latitude Calculations----------------------------

#the following lines of code apply the relative latitude calculation to the data, filtering for whether observations/species ranges 
#fall in the northern and/or southern hemisphere
#observation latitude = col 7; min lat = col 14, max lat = col 15; outcolumn = 19
for (i in 1:972469){ #for each row in main data table...
  if(dataframe_complete[i,7]>0){ #if the observation latitude is in the northern hemisphere
    if(dataframe_complete[i,14]>0){#and the species range is entirely in NH
      dataframe_complete[i,19] <- ((dataframe_complete[i,7] - dataframe_complete[i,14])/(dataframe_complete[i,15] - dataframe_complete[i,14]))#apply the equation as standard (obs lat - equatorward extent/poleward extent-equatorward extent)
    }
    else{#if species range spans equator and observation is in northern hemisphere
      dataframe_complete[i,19] <- dataframe_complete[i,7]/dataframe_complete[i,14]#apply equation (obs lat- 0/poleward extent - 0)
    }
  }
  else{#if observation latitude is in southern hemisphere
    if(dataframe_complete[i,7]<0){#and species range entirely in southern hemisphere
      dataframe_complete[i,19] <- ((dataframe_complete[i,15] - dataframe_complete[i,7])/(dataframe_complete[i,15] - dataframe_complete[i,14]))#apply equation as standard
    }
    else{dataframe_complete[i,19] <- (dataframe_complete[i,7]/dataframe_complete[i,14])#or if species range spans equator but obs lat in southern hemisphere...
    }
  }
}

colnames(dataframe_complete)[19] <- "RelativeLatitude"

#As it stands 43335 of the 972469 records (4.45% are outside of the range of the species)
sum(dataframe_complete$RelativeLatitude < 0 | dataframe_complete$RelativeLatitude >1)

#removing records outside of the range
dataframe_complete <- dataframe_complete[dataframe_complete$RelativeLatitude<1,]
dataframe_complete <- dataframe_complete[dataframe_complete$RelativeLatitude>0,]

#save a copy for good measure
write.csv(dataframe_complete, file="dataframe_complete_23May2024.csv")

#summary_statistics for final dataframe

n_distinct(dataframe_complete$TimeSeriesID)
n_distinct(dataframe_complete$SurveyID)
n_distinct(dataframe_complete$SiteID)
n_distinct(dataframe_complete$HydroBasin)
n_distinct(dataframe_complete$Species)

#Section13--Max Data:Calculate correlations between climate and abundance-------

#functions for calculations and standardization

getmode <- function(v) {#create a new function to calculate mode
  uniqv <- unique(v)
  uniqv[which.max(tabulate(match(v, uniqv)))]
}

standardize <- function(x) (x-mean(x,na.rm=T))/sd(x,na.rm=T)

dataframe_complete$Avg_Abundance_Std <- dataframe_complete %>%
  group_by(TimeSeriesID, Species) %>% #group by the grouping variable in the data (comma separate if multiple)
  reframe(Avg_Abundance_Std = standardize(Avg_Abundance))
dataframe_complete$Avg_Abundance_Std <- dataframe_complete$Avg_Abundance_Std$Avg_Abundance_Std

#Calculations for slopes and correlations

df_models_max <- dataframe_complete %>%
  group_by(TimeSeriesID,Species) %>% #group dataframe by TimeSeriesID & Species
  summarise(Slope_value = cov(Avg_Abundance_Std,Maximum.temperature) / var(Avg_Abundance_Std),Cor_value = cor (Avg_Abundance_Std,Maximum.temperature), Year=min(Year),Duration=max(Duration),Latitude=mean(Latitude),Longitude=mean(Longitude),BioRealm=getmode(BioRealm),Elevation=mean(Elevation),Troph=getmode(Troph),Length=mean(Length),Range=mean(range),RelativeLatitude=mean(RelativeLatitude))

#There are 94658 observations at this stage
df_models_max <- subset(df_models_max,!is.na(df_models_max$Cor_value))#remove rows with slope values of NA
#There are 91936 observations at this stage
df_models_max <- subset(df_models_max,!df_models_max$Cor_value == 0)#remove rows with correlation values of 0
#There are 91935 observations at this stage

#make data binomial
df_models_max$AbundanceChange <- ifelse(df_models_max$Slope_value >0, "1","0")

#Section14--Avg Data:Calculate correlations between climate and abundance-------

#Calculations for slopes and correlations

df_models_avg <- dataframe_complete %>%
  group_by(TimeSeriesID,Species) %>% #group dataframe by TimeSeriesID & Species
  summarise(Slope_value = cov(Avg_Abundance_Std,Average.temperature) / var(Avg_Abundance_Std),Cor_value = cor (Avg_Abundance_Std,Average.temperature), Year=min(Year),Duration=max(Duration),Latitude=mean(Latitude),Longitude=mean(Longitude),BioRealm=getmode(BioRealm),Elevation=mean(Elevation),Troph=getmode(Troph),Length=mean(Length),Range=mean(range),RelativeLatitude=mean(RelativeLatitude))

#There are 94658 observations at this stage
df_models_avg <- subset(df_models_avg,!is.na(df_models_avg$Cor_value))#remove rows with slope values of NA
#There are 91965 observations at this stage
df_models_avg <- subset(df_models_avg,!df_models_avg$Cor_value == 0)#remove rows with correlation values of 0
#There are 91965 observations at this stage

#make data binomial
df_models_avg$AbundanceChange <- ifelse(df_models_avg$Slope_value >0, "1","0")

#Section15--Add migratory data---------------------------------------------------

#add migratory status to the model

migratory_species <- read.table("SpeciesMigratory.txt", header=TRUE,fill=TRUE,sep="\t",check.names=FALSE)

df_models_avg_mig <- left_join(df_models_avg, migratory_species, by=c("Species"))
df_models_max_mig <- left_join(df_models_max, migratory_species, by=c("Species"))

names(df_models_avg_mig)[15] <-'Position_in_range'
names(df_models_avg_mig)[16]<-'Binary_change'
names(df_models_max_mig)[15]<-'Position_in_range'
names(df_models_max_mig)[16]<-'Binary_change'

#clean house
rm(df_models_avg)
rm(df_models_max)
rm(migratory_species)

#Section16--Final processing of dataframes--------------------------------------

#One final filter to make avg and max dataframes match

df_models_avg_mig$Binary_change <- as.factor(df_models_avg_mig$Binary_change)
df_models_max_mig$Binary_change <- as.factor(df_models_max_mig$Binary_change)

df_models_both <- left_join(df_models_max_mig, df_models_avg_mig, by=c("TimeSeriesID", "Species", "Year", "Duration","Latitude","Longitude"))
df_models_avg <-  subset(df_models_both[,c(2:17)])
names(df_models_avg)[3]<-'Slope_value'
names(df_models_avg)[4]<-'Cor_value'
names(df_models_avg)[9]<-'BioRealm'
names(df_models_avg)[10]<-'Elevation'
names(df_models_avg)[11]<-'Troph'
names(df_models_avg)[12]<-'Length'
names(df_models_avg)[13]<-'Range'
names(df_models_avg)[14]<-'Position_in_range'
names(df_models_avg)[15]<-'Binary_change'
names(df_models_avg)[16]<-'Migratory'

df_models_both <- left_join(df_models_max_mig, df_models_avg_mig, by=c("TimeSeriesID", "Species", "Year", "Duration","Latitude","Longitude"))
df_models_max <-  subset(df_models_both[,c(2,3,19,20,6:9,21:28)])
names(df_models_max)[3]<-'Slope_value'
names(df_models_max)[4]<-'Cor_value'
names(df_models_max)[9]<-'BioRealm'
names(df_models_max)[10]<-'Elevation'
names(df_models_max)[11]<-'Troph'
names(df_models_max)[12]<-'Length'
names(df_models_max)[13]<-'Range'
names(df_models_max)[14]<-'Position_in_range'
names(df_models_max)[15]<-'Binary_change'
names(df_models_max)[16]<-'Migratory'

#clean house
rm(df_models_both)
rm(df_models_avg_mig)
rm(df_models_max_mig)

#some summary data

n_distinct(df_models_avg$TimeSeriesID)
n_distinct(df_models_avg$Species)
n_distinct(df_models_avg$Latitude, df_models_avg$Longitude)

n_distinct(df_models_max$TimeSeriesID)
n_distinct(df_models_max$Species)
n_distinct(df_models_max$Latitude, df_models_max$Longitude)

#write_final_CSV

write.csv(df_models_avg, file="df_models_avg_01Oct2024.csv")
write.csv(df_models_max, file="df_models_max_01Oct2024.csv")

#end of code
