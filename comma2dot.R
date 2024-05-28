#################################################################
######## Script for changing the decimal notation from  #########
######## comma to dot in raw TOMST sensor files         #########
######## Developed by: Olivares-Martinez, Luis Daniel   #########
######## May 2022 - Universidad Miguel Hernández        #########
######## Elche, Spain                                   #########
#################################################################

#### STEP 1: Update the working diretory and set an output forlder ####
directorio <- dirname(rstudioapi::getActiveDocumentContext()$path) # change this to your working directory location or leave as it is if the script is in the same location
raw <- "/data_ok" # change this to your output folder name (placed in the working directory)
setwd(directorio)

#### STEP 2: Run all this to generate the new files in the output folder ####
datos_csv <- list.files(pattern = "data_*")

for(i in 1:length(datos_csv)){ #read.text or read.delim
  setwd(directorio)
  cosa <- readLines(datos_csv[i])
  cosa <- gsub(',','.',cosa)
  setwd(paste0(directorio,raw))
  writeLines(cosa, datos_csv[i])
  closeAllConnections()
  }
