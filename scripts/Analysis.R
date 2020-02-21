#install.packages("here")
#install.packages('data.table')
#install.packages('tidyverse')
#install.packages('broom')
#install.packages('dplyr')
library(here)
library(dplyr)
library(ggplot2)
library(broom)
library(stats)
library(tidyverse)
library(data.table)

#######################################
start_time <- Sys.time()
#######################################

path1 <- here()
Orig_file <- read.csv(file.path(path1,'LocData_NID.csv'))

results <- here("results")
if(!file.exists(results)){
  dir.create(results, recursive = TRUE)}

path2 <- file.path(results, 'Thermal_Metrics.csv') # make copy of NWIS_NOAA_NID_Eco_clean.csv that can be edited
write.csv(Orig_file, file = path2)

Main_df <- read.csv(file.path(results,'Thermal_Metrics.csv'))

##################################################
#### Danielle's Cooling/Warming Rate function ####
##################################################

annual_rate <- function(df){
  start <- 58 #end of feburary as to remove freeze period
  max <- which.max(df$y) #row number of max value
  end <- 365
  # create df for each warming/cooling segement
  warm_seg <- df[start:max,]
  cool_seg <- df[max:end,]
  
  #linear fit to warming annual trend
  warm_fit <- lm(warm_seg$y ~warm_seg$X)
  warm_rate <- warm_fit[["coefficients"]][["warm_seg$X"]]
  
  #linear fit to cooling annual trend
  cool_fit <- lm(cool_seg$y ~ cool_seg$X)
  cool_rate <- cool_fit[["coefficients"]][["cool_seg$X"]]
  
  result  <- list(warm_rate_degpd = warm_rate, cool_rate_degpd = cool_rate)
  
  return(result)
}

#######################################
#### Create Thermal Metric Columns #### 
#######################################

Main_df$PstSplnCnt <- NA
Main_df$pstRcrdCnt <- NA
Main_df$pstP90 <- NA
Main_df$pstP90Cnt <- NA
Main_df$pstP50 <- NA
Main_df$pstP10 <- NA
Main_df$pstP10Cnt <- NA
Main_df$pstTmax <- NA
Main_df$pstTmin <- NA
Main_df$pstTmean <- NA
Main_df$pstTmaxT <- NA
Main_df$pstTminT <- NA
Main_df$pstCoolRt <- NA
Main_df$pstWarmRt <- NA
Main_df$pstCffcntVrtn <- NA
Main_df$pstP25cnt <- NA
Main_df$pstP75cnt <- NA

Main_df$PreSplnCnt <- NA
Main_df$preRcrdCnt <- NA
Main_df$preTmax <- NA
Main_df$preTmin <- NA
Main_df$preTmean <- NA
Main_df$preTmaxT <- NA
Main_df$preTminT <- NA

Main_df$NOAA_SplnCnt <- NA
Main_df$NOAA_TmaxT <- NA
Main_df$NOAA_TminT <- NA
Main_df$Lag_TmaxT <- NA
Main_df$Lag_TminT <- NA
Main_df$NOAARcrdCn <- NA

Main_df$Ref <- NA

#########################################################
#### Create Path + List of Post-Construction Records ####
#########################################################

path3 <- here('output/Dam_post')
pstDam <- dir(path = path3, pattern = "*.csv")
pstDam_lst <- str_sub(pstDam, end = -8)
pstUSGs_St_num <- as.list(pstDam_lst) # create an iterable list of USGS gauges with post construction temperature data

##########################################################################################
#### Calcuate Thermal Metrics of Post-Contruction Records and populate main dataframe ####
##########################################################################################

NWIS_Str<- str_split(Main_df$STAID, pattern = ",")
NWIS_Str <- str_split(str_sub(NWIS_Str, start = 2), pattern = ",")

cnt=0
for (n in 1:length(NWIS_Str)){
  if (NWIS_Str[n] %in% pstUSGs_St_num == TRUE) {
    tryCatch(
      expr = {
        x <- c(file.path(here('output/Dam_post/xyxyxyxy_tr.csv')))
        NWISsubstitution <- gsub("xyxyxyxy",NWIS_Str[n], x)
        analysis.df <- read.csv(NWISsubstitution)
        tmax <- max(analysis.df$y)
        tmin <- min(analysis.df$y)
        tmean <- mean(analysis.df$y)
        tmax_timing <- analysis.df$x[which.max(analysis.df$y)]
        tmin_timing <- analysis.df$x[which.min(analysis.df$y)]
        P90 <-qnorm(0.90,mean=mean(analysis.df$y, na.rm = TRUE),sd=sd(analysis.df$y, na.rm = TRUE))
        P50 <- qnorm(0.50,mean=mean(analysis.df$y, na.rm = TRUE),sd=sd(analysis.df$y, na.rm = TRUE))
        P10 <- qnorm(0.10,mean=mean(analysis.df$y, na.rm = TRUE),sd=sd(analysis.df$y, na.rm = TRUE))
        P75 <- qnorm(0.75,mean=mean(analysis.df$y, na.rm = TRUE),sd=sd(analysis.df$y, na.rm = TRUE))
        P25 <- qnorm(0.25,mean=mean(analysis.df$y, na.rm = TRUE),sd=sd(analysis.df$y, na.rm = TRUE))
        SplineCnt <- length(unique(analysis.df$x))
        P10Cnt <- sum(analysis.df$y < P10)/SplineCnt
        P90Cnt <- sum(analysis.df$y > P90)/SplineCnt
        P25Cnt <- sum(analysis.df$y < P25)/SplineCnt
        P75Cnt <- sum(analysis.df$y > P75)/SplineCnt
        RcrdCnt <- analysis.df$RcrdCnt[1]
        CoefficientVariation <- sd(analysis.df$y, na.rm = TRUE)/mean(analysis.df$y, na.rm = TRUE)
        for (z in 1:nrow(Main_df)){
          if (NWIS_Str[n] == str_sub(Main_df$STAID[z], start = 2)){
            Main_df$pstTmax[z] <- tmax
            Main_df$pstTmin[z] <- tmin
            Main_df$pstTmean[z] <- tmean
            Main_df$pstTmaxT[z] <- tmax_timing
            Main_df$pstTminT[z] <- tmin_timing
            Main_df$pstP90[z] <- P90
            Main_df$pstP90Cnt[z] <- P90Cnt
            Main_df$pstP50[z] <- P50
            Main_df$pstP10[z] <- P10
            Main_df$pstP10Cnt[z] <- P10Cnt
            Main_df$pstRcrdCnt[z] <- RcrdCnt
            df <- analysis.df
            Main_df$pstCoolRt[z] <- annual_rate(df)[2]
            Main_df$pstWarmRt[z] <- annual_rate(df)[1]
            Main_df$pstCffcntVrtn[z] <- CoefficientVariation 
            Main_df$pstP25cnt[z] <- P25Cnt
            Main_df$pstP75cnt[z] <- P75Cnt
            Main_df$PstSplnCnt[z] <- SplineCnt
            cnt=cnt+1
          }}},error = function(cond){
            message(paste0(NWIS_Str[n]))
            return(NA)})}}
print(cnt)
########################################################
#### Create Path + List of Pre-Construction Records ####
########################################################

path4 <- here('output/Dam_pre')
preDam <- dir(path = path4, pattern = "*.csv")
preDam_lst <- str_sub(preDam, end = -8)
preUSGs_St_num <- as.list(preDam_lst) # create an iterable list of USGS gauges with post construction temperature data

#########################################################################################
#### Calcuate Thermal Metrics of Pre-Contruction Records and populate main dataframe ####
#########################################################################################
NWIS_Str<- str_split(Main_df$STAID, pattern = ",")
NWIS_Str <- str_split(str_sub(NWIS_Str, start = 2), pattern = ",")
cnt=0
for (n in 1:length(NWIS_Str)){
  if (NWIS_Str[n] %in% preUSGs_St_num == TRUE) {
    tryCatch(
      expr = {
        x <- c(file.path(here('output/Dam_pre/xyxyxyxy_tr.csv')))
        NWISsubstitution <- gsub("xyxyxyxy",NWIS_Str[n], x)
        analysis.df <- read.csv(NWISsubstitution)
        tmax <- max(analysis.df$y)
        tmin <- min(analysis.df$y)
        tmean <- mean(analysis.df$y)
        tmax_timing <- analysis.df$x[which.max(analysis.df$y)]
        tmin_timing <- analysis.df$x[which.min(analysis.df$y)]
        RcrdCnt <- analysis.df$RcrdCnt[1]
        SplineCnt <- length(unique(analysis.df$x))
        for (z in 1:nrow(Main_df)){
          if (NWIS_Str[n] == str_sub(Main_df$STAID[z], start = 2)){
            Main_df$preTmax[z] <- tmax
            Main_df$preTmin[z] <- tmin
            Main_df$preTmean[z] <- tmean
            Main_df$preTmaxT[z] <- tmax_timing
            Main_df$preTminT[z] <- tmin_timing
            Main_df$preRcrdCnt[z] <- RcrdCnt
            Main_df$PreSplnCnt[z] <- SplineCnt
            cnt=cnt+1
          }}},error = function(cond){
            message(paste0(NWIS_Str[n]))
            return(NA)})}}
print(cnt)

#########################################
#### Create Path + List NOAA Records ####
#########################################

path5 <- here('output/NOAA')
NOAA <- dir(path = path5, pattern = "*.csv")
NOAA_lst <- str_sub(NOAA, end = -5)
NOAA_St_num <- as.list(NOAA_lst) # create an iterable list of USGS gauges with post construction temperature data

##############################################################
#### Calcuate Julian Date of NOAA populate main dataframe ####
##############################################################

NOAA_Str<- str_split(Main_df$NOAA_ID, pattern = ",")
NOAA_Str <- str_split(str_sub(NOAA_Str), pattern = ",")

cnt=0
for (n in 1:length(NOAA_Str)){
  if (NOAA_Str[n] %in% NOAA_St_num == TRUE && !is.na(NOAA_Str[[n]])) {
    tryCatch(
      expr = {
        x <- c(file.path(here('output/NOAA/xyxyxyxy.csv')))
        NOAAsubstitution <- gsub("xyxyxyxy",NOAA_Str[[n]], x)
        analysis.df <- read.csv(NOAAsubstitution)
        tmax <- max(analysis.df$y)
        tmin <- min(analysis.df$y)
        tmax_timing <- analysis.df$x[which.max(analysis.df$y)]
        tmin_timing <- analysis.df$x[which.min(analysis.df$y)]
        NOAA_SplnCnt <- length(unique(analysis.df$x))
        RcrdCnt <- analysis.df$pstRcrdCnt[1]
        tmax_tm_diff <- abs(Main_df$pstTmaxT[n] - tmax_timing)
        tryCatch(
          expr = {
            if (!is.na(tmax_tm_diff)){
              if (tmax_tm_diff > 180){
                if (tmax_timing < Main_df$pstTmaxT[n]){
                  tmax_timing = tmax_timing + 365
                }
                else {
                  tmax_timing = tmax_timing - 365
                }
              }
      
              if (tmin_tm_diff > 180){
                if (tmin_timing < Main_df$pstTminT[n]){
                  tmin_timing = tmin_timing + 365
                }
                else {
                  tmin_timing = tmin_timing - 365
                }
              }
              Main_df$NOAA_SplnCnt[n] <- NOAA_SplnCnt
              Main_df$NOAA_TmaxT[n] <- tmax_timing
              Main_df$NOAA_TminT[n] <- tmin_timing
              Main_df$Lag_TmaxT[n] <- abs(Main_df$pstTmaxT[n] - tmax_timing)
              Main_df$Lag_TminT[n] <- abs(Main_df$pstTminT[n] - tmin_timing)
              Main_df$NOAARcrdCn[n] <- RcrdCnt
              cnt=cnt+1}}
          ,error = function(cond){
            message(paste0(NOAA_Str[[n]]))
            return(NA)
            }
        )
        }
      ,error = function(cond){
        message(paste0(NOAA_Str[[n]]))
        return(NA)
        }
      )
    }
  }
print(cnt)

#################################################
#### Create Path + List of Reference Records ####
#################################################

path6 <- here('output/Ref')
Rfrnc <- dir(path = path6, pattern = "*.csv")
Rfrnc_lst <- str_sub(Rfrnc, end = -8)
Rfrnc_St_num <- as.list(Rfrnc_lst) 

###################################################################################
#### Calcuate Thermal Metrics of Reference Records and populate main dataframe ####
###################################################################################

NWIS_Str<- str_split(Main_df$STAID, pattern = ",")
NWIS_Str <- str_split(str_sub(NWIS_Str, start = 2), pattern = ",")
cnt=0
for (n in 1:length(NWIS_Str)){
  if (NWIS_Str[n] %in% Rfrnc_St_num == TRUE) {
    tryCatch(
      expr = {
        x <- c(file.path(here('output/Ref/xyxyxyxy_tr.csv')))
        NWISsubstitution <- gsub("xyxyxyxy",NWIS_Str[n], x)
        analysis.df <- read.csv(NWISsubstitution)
        tmax <- max(analysis.df$y)
        tmin <- min(analysis.df$y)
        tmean <- mean(analysis.df$y)
        tmax_timing <- analysis.df$x[which.max(analysis.df$y)]
        tmin_timing <- analysis.df$x[which.min(analysis.df$y)]
        P90 <- qnorm(0.90,mean=mean(analysis.df$y, na.rm = TRUE),sd=sd(analysis.df$y, na.rm = TRUE))
        P50 <- qnorm(0.50,mean=mean(analysis.df$y, na.rm = TRUE),sd=sd(analysis.df$y, na.rm = TRUE))
        P10 <- qnorm(0.10,mean=mean(analysis.df$y, na.rm = TRUE),sd=sd(analysis.df$y, na.rm = TRUE))
        P25 <- qnorm(0.25,mean=mean(analysis.df$y, na.rm = TRUE),sd=sd(analysis.df$y, na.rm = TRUE))
        P75 <- qnorm(0.75,mean=mean(analysis.df$y, na.rm = TRUE),sd=sd(analysis.df$y, na.rm = TRUE))
        SplineCnt <- length(unique(analysis.df$x))
        P10Cnt <- sum(analysis.df$y < P10)/SplineCnt
        P90Cnt <- sum(analysis.df$y > P90)/SplineCnt
        P25Cnt <- sum(analysis.df$y < P25)/SplineCnt
        P75Cnt <- sum(analysis.df$y > P75)/SplineCnt
        RcrdCnt <- analysis.df$pstRcrdCnt[1]
        CoefficientVariation <- sd(analysis.df$y, na.rm = TRUE)/mean(analysis.df$y, na.rm = TRUE)
        for (z in 1:nrow(Main_df)){
          if (NWIS_Str[n] == str_sub(Main_df$STAID[z], start = 2)){
            Main_df$Ref[z] <- '1'
            Main_df$pstTmax[z] <- tmax
            Main_df$pstTmin[z] <- tmin
            Main_df$pstTmaxT[z] <- tmax_timing
            Main_df$pstTminT[z] <- tmin_timing
            Main_df$pstTmean[z] <- tmean
            Main_df$pstP90[z] <- P90
            Main_df$pstP90Cnt[z] <- P90Cnt
            Main_df$pstP50[z] <- P50
            Main_df$pstP10[z] <- P10
            Main_df$pstP10Cnt[z] <- P10Cnt
            Main_df$pstRcrdCnt[z] <- RcrdCnt
            df <- analysis.df
            Main_df$pstCoolRt[z] <- annual_rate(df)[2]
            Main_df$pstWarmRt[z] <- annual_rate(df)[1]
            Main_df$pstCffcntVrtn[z] <- CoefficientVariation
            Main_df$pstP25cnt[z] <- P25Cnt
            Main_df$pstP75cnt[z] <- P75Cnt
            Main_df$PstSplnCnt[z] <- SplineCnt
            cnt=cnt+1
          }}},error = function(cond){
            message(paste0(NWIS_Str[n]))
            return(NA)})}
  else{
    Main_df$Ref[n] <- '0'
  }
}
print(cnt)

Main_df2 <- vapply(Main_df, paste, collapse = ", ", character(1L))
df_Place2 = data.frame(lapply(Main_df, as.character), stringsAsFactors=FALSE)
df_Place2 <- df_Place2[!is.na(df_Place2$pstTmax), ]
path2_fnl <- file.path(here('results'), 'Thermal_Metrics.csv')
#######################################
write.csv(df_Place2, file = path2)
#######################################
end_time <- Sys.time()
time <- end_time - start_time
print(time)
print("Script Complete")
#######################################