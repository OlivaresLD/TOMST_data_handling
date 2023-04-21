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
install.packages(c("readxl","lubridate")) #Run this in case you don't have these packages installed 


#### STEP 1: Update working directory and Run ####
directorio <- "C:/Users/tcastaneda/Documents/SOILGUARD/tomst extracting code/SOILGUARD_TOMST_data_workshop/" ## put your internal working directory in here

#### STEP 2: run all this once correctly defined your working directory ####
library(lubridate)
library(readxl)

carpe <- "/a procesar" # set at folder where you have your .csv files without any modifications, directly as they come from the TOMST sensors

setwd(directorio)
ecuaciones <- read.csv("equations.csv")
ids <- read_excel("SOILGUARD-WP3_TOMST_ID.xlsx", sheet=2)

setwd(paste0(directorio,carpe))
datos_csv <- list.files(pattern = "data_*")
sensores <- substr(datos_csv,6,13)
datos_sensores <- as.list(sensores)

for(i in 1:length(sensores)){
  archivo <- datos_csv[i]
  
  inicio_m <- ymd(ids$init_date[which(ids$sensor_ID == sensores[i])])
  datos <- read.csv2(archivo, header = F)
  datos <- datos[,-10] #added this to drop an empty column with zeros - not needed for DK data
  names(datos) <- c("index","time","time_zone","Ts6cm","Ts0cm","Ta12cm","raw_moist","shake","errFlag")
  datos$time <- ymd_hm(datos$time)
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
  
  datos$sensor <- sensores[i]
  datos$long_plot_ID <- ids$long_plot_ID[which(ids$sensor_ID == sensores[i])]
  datos$short_plot_ID <- ids$short_plot_ID[which(ids$sensor_ID == sensores[i])]
  
  datos <- subset(datos, datos$time > inicio_m)
  datos_sensores[[i]] <- datos
}

#### STEP 3: Output generation. Run this but check output name first ####
setwd(directorio)
datox <- do.call(rbind, datos_sensores)

write.csv(datox, paste0("SOILGUARD_DENMARK_sensors_",today(),".csv"), row.names = FALSE) # You can change the name inside the quotes to your country name e.g. SOILGUARD_FINDLAND_sensors_