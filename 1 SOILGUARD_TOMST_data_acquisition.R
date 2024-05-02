#################################################################
######## Script of data recovery for TMS-4 sensors ##############
######## Developed by: Luis Daniel Olivares Martínez ############
######## May-Jun 2022 - Universidad Miguel Hernández ############
#################################################################

# this script allows the calculation of volumetric 
# soil water content and compilation of soil and air 
# temperatures as they come from individual .csv files 
# and acquired from the TOMST sensors.
# It uses the equations and the information of soil bulk density, 
# soil texture, and the sensor's serial number for the assignation 
# to each experimental treatment. 
# FOLLOW THE STEP BY STEP COMMENTS TO RUN THE CODE

#### STEP 0 (not mandatory) ####
install.packages(c("readxl","tidyverse","feather")) #Run this in case you don't have these packages installed 


#### STEP 1: Update working directory and Run ####
directorio <- "C:/Users/tcastaneda/Documents/SOILGUARD/tomst extracting code/SOILGUARD_TOMST_data_workshop/" ## put your internal working directory in here

#### STEP 2: run all this once correctly defined your working directory ####
library(tidyverse)
library(readxl); library(feather)

carpe <- "/a procesar" # set at folder where you have your .csv files without any modifications, directly as they come from the TOMST sensors

setwd(dirname(rstudioapi::getActiveDocumentContext()$path)) # stablish the file location as the project's working directory

carpe <- "a procesar" # set at folder where you have your .csv files without any modifications, directly as they come from the TOMST sensors
ecuaciones <- read.csv("equations.csv")
ids <- read_excel("SOILGUARD-WP3_TOMST_ID_2023.xlsx", sheet=2, range = cell_cols("A:T")); ids <- as.data.frame(ids)
hwe <- read_excel("SOILGUARD-WP3_TOMST_ID_2023.xlsx", sheet='heatwave_ID')
irri <- read_excel("SOILGUARD-WP3_TOMST_ID_2023.xlsx", sheet='irrigation')
cuts <- read_excel("SOILGUARD-WP3_TOMST_ID_2023.xlsx", sheet='cuts')

datos_csv <- list.files(path=carpe, pattern = "data_*")
sensores <- substr(datos_csv,6,13)
trat <- ids$short_plot_ID[!is.na(ids$sensor_ID)]
datos_sensores <- as.list(trat) 

setwd(carpe)

for(i in 1:length(trat)){
  fi_ids <- which(ids$short_plot_ID == trat[i])
  
  if(isFALSE(ids$sensor_ID[fi_ids] %in% sensores)){next}
  else{
    fi_sen <- which(sensores == ids$sensor_ID[fi_ids])
    
    archivo <- datos_csv[fi_sen]
    
    inicio_m <- ymd(ids$init_date[fi_ids])
    fin_m <- ymd(ids$end_date[fi_ids])
    datos <- read.csv2(archivo, header = F)
    datos <- datos[,-10] #added this to drop an empty column with zeros
    names(datos) <- c("index","time","time_zone","Ts6cm","Ts0cm","Ta12cm","raw_moist","shake","errFlag")
    datos$time <- ymd_hm(datos$time) + hours(ids$UTC[fi_ids])
    datos$Ts6cm <- as.numeric(datos$Ts6cm)
    datos$Ts0cm <- as.numeric(datos$Ts0cm)
    datos$Ta12cm <- as.numeric(datos$Ta12cm)
    datos$temp_prom <- rowSums(datos[,4:6])/3
    
    # Euclidean distances from clay+sand+bulkdens compared with the equation of each model
    euclidiana <- function(a, b=c(dens_ap,arcilla,arena)) sqrt ( sum ((a - b) ^ 2))
    arcilla <- ids$clay[which(ids$sensor_ID == sensores[i])] ## porcentaje de arcilla del suelo
    arena <- ids$sand[which(ids$sensor_ID == sensores[i])] ## porcentaje de arena del suelo
    dens_ap <- ids$bulk_d[which(ids$sensor_ID == sensores[i])]
    
    ecuaciones[8,c("bulk_dens","clay","sand")] <- 0
    ecuaciones$eu <- apply(ecuaciones[c("bulk_dens","clay","sand")],1,euclidiana)
    fi <- which.min(ecuaciones$eu)
    
    # Link data.frame with textures, equations and parameters (second grade equation with form y = a*x^2 + b*x + c; where x = datos$raw_moist)
    datos$hum_simple <- ecuaciones$a[fi]*datos$raw_moist^2 + ecuaciones$b[fi]*datos$raw_moist + ecuaciones$c[fi]
    
    # Add temperature correction
    t_ref <- 24 ## Reference temperature
    delta_air <- 1.91132689118083 ## delta pulses by air temperature
    delta_water <- 0.64108 ## delta pulses by water temperature
    delta_ref <- delta_water - delta_air # reference delta (soil)
    datos$hum_corr_t <- datos$raw_moist + (t_ref - datos$Ts6cm)*(delta_air + delta_ref * datos$hum_simple)
    
    sensor_hum_air <- 50 ## Raw sensor count in air at 20°C
    sensor_hum_water <- 3635 ## Raw sensor count in water at 20°C
    temp_sensor_user <- 20 ## Temperature by user
    norm.period <- sensor_hum_air+(t_ref-temp_sensor_user)*delta_air
    kalib_air <- 114.533980673542 ## "moje čidla pro kalibraci průměr"
    kalib_water <- 3634.72324 ## "moje čidla pro kalibraci průměr"
    dif_air <- kalib_air - norm.period
    dif_water <- kalib_water - (sensor_hum_water + (t_ref - temp_sensor_user)*delta_water)
    dif_kalib_hum <- dif_water - dif_air # na "dil" objemove vlhkosti
    
    ### put a, b and c values by texture ###
    m_kalib_eq <- datos$hum_corr_t + dif_kalib_hum * datos$hum_simple
    datos$hum <- ecuaciones$a[fi]*m_kalib_eq^2 + ecuaciones$b[fi]*m_kalib_eq + ecuaciones$c[fi]
    
    datos$sensor <- sensores[fi_sen]
    datos$long_plot_ID <- ids$long_plot_ID[fi_ids]
    datos$short_plot_ID <- ids$short_plot_ID[fi_ids]
    
    datos <- subset(datos, datos$time > inicio_m & datos$time < fin_m)
    datos_sensores[[i]] <- datos
  }
}
dat_senso <- datos_sensores[sapply(datos_sensores, function(x) is.data.frame(x))]
rm(datos_sensores)
TOMST_all <- do.call(rbind, dat_senso) ; rm(dat_senso)
TOMST_all <- merge(TOMST_all,ids, by = c('long_plot_ID','short_plot_ID'))

#### STEP 2: DATA QUERYING Run all this (from line 108 to 126) ####
cutt <- na.exclude(cuts)
cutt$t1 <- ymd_hms(paste(cutt$date_1,cutt$time_1))
cutt$t2 <- ymd_hms(paste(cutt$date_2,cutt$time_2))

TOMST_cut <- TOMST_all
for(i in 1:dim(cutt)[1]){
 fi <- which(TOMST_cut$long_plot_ID != cutt$long_plot_ID[i] & TOMST_cut$time < cutt$t1[i] | TOMST_cut$long_plot_ID != cutt$long_plot_ID[i] & TOMST_cut$time > cutt$t2[i])
 TOMST_cut <- TOMST_cut[fi,]
}
TOMST_cut <- subset(TOMST_cut, hum >= 0 )

#### STEP 3: Output generation. Run this but check output name first ####
setwd(directorio)
write_feather(TOMST_cut, paste0("SOILGUARD_WP3_2023_sensors_",today(),".feather")) # You can change the name inside the quotes to your country name e.g. SOILGUARD_FINDLAND_sensors_
# write.csv(TOMST_cut, paste0("SOILGUARD_WP3_2023_sensors_",today(),".csv"), row.names = FALSE) # Alternative option if you want to export to a csv format (not recommended for big data bases)
