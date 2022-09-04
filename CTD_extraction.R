##################################
## Script for converting CTD data into usable data for ODV
## Specific for format of research vessel ALKOR in Baltic Sea
## Also requires "angle2dec.R" in the same folder
##################################
## Author: Theo Krueger
## Last update: 31.08.2022
##################################



library(tidyverse)
source("angle2dec.R")



# set to folder with TOB files
input_folder <- ""
# set to output folder or leave at default to save files in original folder
# be careful as default only works in ONLY TOB files are in folder
output_folder <- input_folder

#####################################################

{ # RUN HERE to run the whole script
  
in_files <- list.files(input_folder)
out_files <- gsub(".TOB", ".csv", in_files)


n <- 0
for (file in in_files){
  # keep count
  n = n+1
  print(paste("Processing: ", in_files[n]))
   
  # read table
  df <- read.table(paste0(input_folder, "/", file), skip =  51)
  
  # give meaningful column names
  names(df) <- c("ID","Pressure..db","Temp..degC","Leitf..mS.cm","RawO2..mV",
                 "Boden","SALIN..ppt","SIGMA..kg.m3","AO2_%..%","AO2mg..mg.l",
                 "Licor..pffr","date","time","Lat..Deg.N","Long..Deg.E","BsFlo",
                 "AO2ml..ml.l")
  
  # reformatting date
  df <- df %>% separate(date, c("Day", "Month", "Year"))
  
  # reformatting lat and lon
  long <- df$Long..Deg.E
  long_new <- c()
  for (long_sub in long){
    new_long_sub <- strtrim(long_sub, nchar(long_sub)-1)
    new_long_sub <- new_long_sub %>% 
      str_replace("(.{2})", "\\1 ") %>% 
      str_trim()
    long_new <- append(long_new, new_long_sub)
  }
  df$Long..Deg.E <- long_new
  df$Long..Deg.E <- angle2dec(df$Long..Deg.E)
  
  lat <- df$Lat..Deg.N
  lat_new <- c()
  for (lat_sub in lat){
    new_lat_sub <- strtrim(lat_sub, nchar(lat_sub)-1)
    new_lat_sub <- new_lat_sub %>% 
      str_replace("(.{2})", "\\1 ") %>% 
      str_trim()
    lat_new <- append(lat_new, new_lat_sub)
  }
  df$Lat..Deg.N <- lat_new
  df$Lat..Deg.N <- angle2dec(df$Lat..Deg.N)
  
  # subset data to only include useful columns
  df_useful <- subset(df, select = c(1:3, 7, 12:17,19))
  
  # save as csv
  write.csv(df_useful, 
            file = paste0(output_folder, "/", out_files[n]), row.names = FALSE)

}
}
