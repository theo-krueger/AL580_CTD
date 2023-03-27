##################################
## Script for converting CTD data into usable data for ODV
## Specific for format of research vessel ALKOR in Baltic Sea
## Also requires "angle2dec.R" in the same folder
##################################
## Author: Theo Krueger
## Last update: 04.09.2022
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
  
  #prepare column names
  col_names_df <- c("ID","Pressure..db","Temp..degC","Leitf..mS.cm","RawO2..mV",
                    "Boden","SALIN..ppt","SIGMA..kg.m3","AO2_%..%","AO2mg..mg.l",
                    "Licor..pffr","date","time","Lat..Deg","Long..Deg","BsFlo",
                    "AO2ml..ml.l")
  col_names_master <- c("ID", "Pressure..db", "Temp..degC", "SALIN..ppt", 
                        "Day", "Month", "Year", "time", "Lat..Deg", 
                        "Long..Deg", "O2ml..ml.l")
  
  #prepare master sheet
  master <- data.frame(matrix(ncol = 17, nrow = 0))
  colnames(master) <- col_names_master
  
  n <- 0
  
  for (file in in_files){
    # keep count
    n = n+1
    print(paste("Processing:", in_files[n]))
    
    # read table
    df <- read.table(paste0(input_folder, "/", file), skip =  51)
    
    # give meaningful column names
    names(df) <- col_names_df
    
    # reformatting date
    df <- df %>% separate(date, c("Day", "Month", "Year"))
    
    # reformatting lat and lon to dec
    long <- df$Long..Deg
    long_new <- c()
    for (long_sub in long){
      switch_EW <- FALSE
      if (grepl("W", long_sub, fixed = TRUE)){
        switch_EW = TRUE
      }
      new_long_sub <- strtrim(long_sub, nchar(long_sub)-1)
      new_long_sub <- new_long_sub %>% 
        str_replace("(.{2})", "\\1 ") %>% 
        str_trim()
      if (switch_EW){
        new_long_sub <- paste0("-", new_long_sub)
      }
      
      long_new <- append(long_new, new_long_sub)
    }
    df$Long..Deg <- long_new
    df$Long..Deg <- angle2dec(df$Long..Deg)
    
    lat <- df$Lat..Deg
    lat_new <- c()
    for (lat_sub in lat){
      switch_NS <- FALSE
      if (grepl("S", lat_sub, fixed = TRUE)){
        switch_NS <- TRUE
      }
      
      new_lat_sub <- strtrim(lat_sub, nchar(lat_sub)-1)
      new_lat_sub <- new_lat_sub %>% 
        str_replace("(.{2})", "\\1 ") %>% 
        str_trim()
      if (switch_NS){
        new_lat_sub <- paste0("-", new_lat_sub)
      }
      
      lat_new <- append(lat_new, new_lat_sub)
    }
    df$Lat..Deg <- lat_new
    df$Lat..Deg <- angle2dec(df$Lat..Deg)
    
    # subset data to only include useful columns
    df_useful <- subset(df, select = c(1:3, 7, 12:17,19))
    
    # add to master
    master <- rbind(master, df_useful)
    
    # save individual file as csv
    write.csv(df_useful, 
              file = paste0(output_folder, "/", out_files[n]), row.names = FALSE)
    
    print(paste("Finished. File saved as:", out_files[n]))
  }
  
  # save master as csv
  write.csv(master,
            file = paste0(output_folder, "/master.csv"), row.names = FALSE)
  print(paste("Done. Master file saved as: master.csv"))
}
