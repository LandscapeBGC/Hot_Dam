## Hot Dam - Download Data
## Danielle Hare

##--- Pull Data (that could be) Relevant for Dams and Comparison to Reference Sites

##--- Install.packages
#install.packages("here")
#install.packages("dataRetrieval")
#install.packages("dplyr")
#install.packages('rnoaa')

##--- Library 
library(here)
library(dataRetrieval) #Retreive NWIS datasetset
library(dplyr)#for filter
library(rnoaa)# Retreive NOAA data

#######################################
start_time <- Sys.time()
#######################################

# --- Input Parameters

file_list <- c("conterm_bas_classif_.txt","conterm_hydro.txt","conterm_hydromod_dams.txt")#list of 'extra' GAGEII datasets of interest
dam_dis_max <- 10 #max dam distance km
SW_out_dam <- file.path(here(), "input/Dam_Raw")
if(!file.exists(SW_out_dam)){
  dir.create(SW_out_dam, recursive = TRUE)}
SW_out_ref <- file.path(here(), "input/Ref_Raw")
if(!file.exists(SW_out_ref)){
  dir.create(SW_out_ref, recursive = TRUE)}
NOAA_pullR <- file.path(here(), "input/NOAA_Raw")
if(!file.exists(NOAA_pullR)){
  dir.create(NOAA_pullR, recursive = TRUE)}
f_out <- "input/LocData.csv"

##---- Read location files 

#initate dataframe with location information
GAGE_loc <- read.csv(list.files(pattern = "conterm_basinid.txt", recursive = TRUE, full.names = TRUE),
                     header=T,
                     stringsAsFactors=F,
                     colClasses=c("STAID"="character"))

# Merge with GageII Dataset, based on specified files above
for (f in file_list){
  df <- read.csv(list.files(pattern = f, recursive = TRUE, full.names = TRUE),
                 header=T,
                 stringsAsFactors=F,
                 colClasses=c("STAID"="character"))
  GAGE_loc <- merge(GAGE_loc, df, on = 'STAID')
}

rm(df)
# Column name "id" necessary to run NOAA part
GAGE_loc$id <- GAGE_loc$STAID


### --- Perform Relevant Dam Download
GAGE_loc$NWIS_Tavail <- NaN #Set column for SW Temperature records that are available for download

#make two relevant folders Dam within raw distance and ref locations
GAGE_Dam <- filter(GAGE_loc, (GAGE_loc$RAW_DIS_NEAREST_DAM < dam_dis_max & GAGE_loc$RAW_DIS_NEAREST_DAM >=0 & GAGE_loc$CLASS != "Ref"))
GAGE_Ref <- filter(GAGE_loc, GAGE_loc$CLASS == "Ref")
GAGE_Sites <- merge(GAGE_Dam, GAGE_Ref, all = TRUE) # Combine Ref and Non Ref sites
#GAGE_Sites <-GAGE_Sites

for (n in 1:nrow(GAGE_Sites)) # perform a loop that pulls stream temp data into either dam or ref folders
  {
  ID <- GAGE_Sites$id[n]
  print(ID)
  if (ID %in% GAGE_Dam$STAID)
    {
    temp_df<- renameNWISColumns(readNWISdv(ID, "00010"))#, #Mean Temperature (C)
    df <- temp_df
    if (nrow(df)>(0.75*365*3))
      {# at least has 75% of three years worth of temp data
      GAGE_Sites$NWIS_Tavail[n] <-  1
      #--- also download discharge data if available
      try(
        {
        discharge_df<- renameNWISColumns(readNWISdv(ID, "00060"))#, #Daily Discharge cubic feet per second
        #startDate = date_se[1],
        #endDate = date_se[2]))
        df <- merge(temp_df, discharge_df, on = "Date", how = 'left')
        }
      )
    
      fn <- sprintf('%s.csv', ID)
      fp <- file.path(SW_out_dam,fn)
      write.csv(df, file = fp)
    }
    
    else { #not enough data or no data available
      GAGE_Sites$NWIS_Tavail[n] <- 0 
    }
  }
  
  else if (ID %in% GAGE_Ref$STAID) {
    
    temp_df<- renameNWISColumns(readNWISdv(ID, "00010"))#, #Mean Temperature (C)
    df <- temp_df
    if (nrow(df)>(0.75*365*3)){# at least has 75% of three years worth of temp data
      GAGE_Sites$NWIS_Tavail[n] <-  1
      #--- also download discharge data if available
      try({
        discharge_df<- renameNWISColumns(readNWISdv(ID, "00060"))#, #Daily Discharge cubic feet per second
        #startDate = date_se[1],
        #endDate = date_se[2]))
        df <- merge(temp_df, discharge_df, on = "Date", how = 'left')
      }
      )
      fn <- sprintf('%s.csv', ID)
      fp <- file.path(SW_out_ref,fn)
      write.csv(df, file = fp)
    }
    else { #not enough data or no data available
      GAGE_Sites$NWIS_Tavail[n] <-  0 
    }
  }
}

# --- Only keep rows with Temp Data
GAGE_Sites1 <- filter(GAGE_Sites, NWIS_Tavail== 1)
#GAGE_Sites_edt$id <- GAGE_Sites_edt$locname

# Save GAGE_Dam_NWIS_Available
saveRDS(GAGE_Sites1, file = file.path(here(), "input/DAM_GAGE_NWISAvail.rds"))



## Get NOAA DATA
#Get a list of all the NOAA stations currently available
G_st <- ghcnd_stations() #Use first time, or if need of an update - takes a long time. 
saveRDS(G_st, file = file.path(here(), "input/NOAA_Stations_All.RData"))

# Determine the nearest NOAA station to each of the SW station in the list filter for only SW stations that meet the timeframe requirements
# Here we look for the NOAA station within 25 mi of station, no data returned for SW stations
# without any within that range. Limit = 1 only provide 1 per station (the closet one)
nearby_stations <- meteo_nearby_stations(lat_lon_df = GAGE_Sites1, 
                                         lat_colname = "LAT_GAGE", 
                                         lon_colname = "LNG_GAGE", #" #"dec_long_va",
                                         station_data = G_st,
                                         year_min = 2000, #year_min = 2011 #This way we wont get air temp records with just 2010, but include 2010 in data pull- see below 
                                         year_max = 2019, #year_max = 2018
                                         var = c("TMAX", "TMIN", "TAVG"), #var = c("TMAX", "TMIN", "TAVG"),
                                         radius = 25,
                                         limit = 1)

# Merge the nearby station data to one dataframe
match_data = do.call(rbind, nearby_stations) # make output of meteo nearby stations to a usable df

# Make a list of the unique NOAA station ids (remove duplicates)
noaa_pull <- unique(match_data$id)
# remove NA values (if some stations do not have lat long this will occur)
noaa_pull <- noaa_pull[!is.na(noaa_pull)]

#put in question to pull air data
# Pull the data for each of the NOAA stations identified as closest stations to the input SW stations
for (i in noaa_pull) {
  fn <- sprintf('%s.csv', i)
  fp <- file.path('input/NOAA_Raw',fn)
  # If the NOAA station is not already downloaded, download it. This is so i dont have to manipulate input files for speed
  if(!file.exists(fp)){
    # If there is an error the script will continue to run
    tryCatch({
      # Pull the station temperature data
      df <- meteo_pull_monitors(monitors = i, 
                                var = c("TMAX", "TMIN", "TAVG")) #var = c("TAVG", "TMIN", "TMAX"))
      rec <- colnames(df[3:length(df)])
      
      for (n in rec){
        df_temp <- df[n]/10
        df[n] <- df_temp
      }
      
      write.csv(df, file = fp)
    }, 
    error=function(e){cat("ERROR :",conditionMessage(e), "\n")})
  }
}

#change index/rownames to be a active column - locname is consistent for next step (2)
match_data$locname <- row.names(match_data)
#change original input siteno/id to locname for match (needed to be id for last package to work)
colnames(GAGE_Sites)[colnames(GAGE_Sites)=="id"] <- "locname"

#Change Column names to match input for step 2, as well as provide appropriate colnames for join with SW location data
colnames(match_data)[names(match_data) == "id"] <- "NOAA_ID"
colnames(match_data)[names(match_data) == "name"] <- "NOAA_NAME"
colnames(match_data)[names(match_data) == "latitude"] <- "NEAR_Y"
colnames(match_data)[names(match_data) == "longitude"] <- "NEAR_X"
colnames(match_data)[names(match_data) == "distance"] <- "NEAR_DIST"

# Merge the SW and Air Station data together (in the same format as the ArcGIS python script)
GAGE_Sites <- GAGE_Sites[, !duplicated(colnames(GAGE_Sites))]#remove duplicate column names locname
station_loc <- left_join(GAGE_Sites,match_data, by = "locname")

#Export this merge data set for next step (step 2 in python for annual signal)
station_loc$STAID = paste0(1, station_loc$STAID) # add prefix to avoid loss of '0' at the beginning of STAID id values
write.csv(station_loc, file = f_out)


#######################################
end_time <- Sys.time()
time <- end_time - start_time
print(time)
print("Script Complete")
#######################################