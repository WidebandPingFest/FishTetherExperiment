# import functions
# moves all the functions to a single script
# Dec 1, 2022

# read_tidy_export_EVfiles
# Description: imports individual fish ev exports
## compensated TS frequency response
## single target file
## fish track regions
## fish bio data
# 
# Returns: dataframe of imported data
# Example: read_tidy_export_EVfiles("LT001")

read_tidy_export_EVfiles <- function(getFishID) {
  require(dplyr)
  require(readr)
  require(tidyr)
  
  fishbio <- read_csv("NonPingData/fishInfo_20220912.csv")
  
  freqLong <- read_comp_freq_response(getFishID)
  
  sinTar <- read.csv(paste0("Data/", getFishID, "/ExportedFishTracks (targets).csv")) %>% # Bring in single target data
    mutate(FishTrack = paste(Region_name, Ping_number, sep = "_")) %>%
    select(
      Region_name, FishTrack, Ping_time, Target_range, Angle_minor_axis, Angle_major_axis, Distance_minor_axis,
      Distance_major_axis, StandDev_Angles_Minor_Axis, StandDev_Angles_Major_Axis, Target_true_depth
    ) %>%
    mutate(
      fishNum = getFishID,
      FishTrack = gsub(pattern = " ", replacement = "_", FishTrack),
      Region_name = gsub(pattern = " ", replacement = "_", Region_name),
      pingNumber = as.numeric(str_extract(FishTrack,"[^_]+$"))
    ) %>%
    group_by(fishNum, Region_name) %>% 
    mutate(deltaRange = case_when(abs(lead(pingNumber)-pingNumber) == 1 ~ lead(Target_range)-Target_range),
           deltaMinAng = case_when(abs(lead(pingNumber)-pingNumber) == 1 ~ lead(Angle_minor_axis)-Angle_minor_axis),
           deltaMajAng = case_when(abs(lead(pingNumber)-pingNumber) == 1 ~ lead(Angle_major_axis)-Angle_major_axis),
           aspectAngle = atan(deltaRange/(deltaMajAng^2+deltaMinAng^2)^0.5)*180/pi
    ) %>% 
    relocate(fishNum) %>% 
    relocate(deltaRange:aspectAngle, .after = Ping_time)
  
  sinTar_region <- read_csv(paste0("Data/", getFishID, "/ExportedFishTracks (regions).csv"))%>%
    select(Region_name, Ping_S, Ping_E, Num_targets, TS_mean, Target_range_mean:Region_top_altitude_mean) %>%
    mutate(fishNum = getFishID,
           Region_name = gsub(pattern = " ", replacement = "_", Region_name)) %>%
    relocate(fishNum)
  
  sinTar <- inner_join(sinTar, sinTar_region, by =c("fishNum", "Region_name"))
  
  # Pivot frequency response data wide, join all data together
  freqWide <- freqLong %>%
    mutate(Frequency = paste0("F", freqLong$Frequency)) %>%
    pivot_wider(names_from = Frequency, values_from = TS) # values_fn = length necessary to create csv file
  
  freqWideDat <- inner_join(fishbio, sinTar, by = "fishNum") %>%
    left_join(freqWide, by = c("fishNum", "FishTrack")) %>%
    mutate(dateTimeSample = as.POSIXct(dateTimeSample))
  
  cat(paste("--------------", getFishID, "--------------"))
  
  # return the imported data
  return(freqWideDat)
}

# read_comp_freq_response
# Description: reads in frequency response data for each transducer for each fish
# Returns: table of frequency response dat in long form

read_comp_freq_response <- function(getFishID){
  require(dplyr)
  require(readr)
  require(tidyr)
  
  # Read in frequency response data
  pingindex <- read_csv(paste0("Data/", getFishID, "/FreqResponse70.csv"), col_names = FALSE, skip = 1, n_max = 1) # Subset rows with ID info (for later joining)
  pingindex <- as.vector(t(pingindex))
  pingindex <- pingindex[!pingindex == "Ping_index"]
  pingindex <- pingindex[!is.na(pingindex)]
  regionname <- read_csv(paste0("Data/", getFishID, "/FreqResponse70.csv"), col_names = FALSE, skip = 7, n_max = 1) # Subset rows with ID info (for later joining)
  regionname <- as.vector(t(regionname))
  regionname <- regionname[!regionname == "Region_name"]
  regionname <- regionname[!is.na(regionname)]
  regionname <- gsub(pattern = " ", replace = "_", regionname)
  length(pingindex) == length(regionname)
  
  region_index <- paste(regionname, pingindex, sep = "_")
  
  freq70 <- read_csv(paste0("Data/", getFishID, "/FreqResponse70.csv"), skip = 8, col_names = FALSE) %>%
    filter(.[[1]] <= 89.5) # Need to remove 90kHz to avoid duplicate columns (90 in 120kHz, too)
  f120csv <- paste0("Data/", getFishID, "/FreqResponse120.csv")
  if(file.exists(f120csv)){
    freq120 <- read_csv(f120csv, skip = 8, col_names = FALSE)
  } else (freq120 <- NULL)
  
  f200csv <- paste0("Data/", getFishID, "/FreqResponse200.csv")
  if(file.exists(f200csv)){
    freq200 <- read_csv(f200csv, skip = 8, col_names = FALSE) %>%
      filter(.[[1]] >= 173 ) # LWF004, LWF005, LWF006 duplicate 160-173
  } else (freq200 <- NULL)
  fishFreq <- bind_rows(freq70, freq120, freq200)
  
  names(fishFreq) <- c("Frequency", region_index, "Variable_Index", "Variable_Name")
  fishFreq <- fishFreq %>% select(-Variable_Index, -Variable_Name)
  
  freqLong <- fishFreq %>% pivot_longer(names_to = "fish", -Frequency)
  freqLong <- freqLong %>% rename(TS = value)
  freqLong$fishNum <- getFishID
  freqLong <- freqLong %>%
    relocate(fishNum) %>%
    relocate(fish, .after = fishNum) %>%
    rename(FishTrack = fish)
  
  freqLong
}

# read_uncomp_freq_response
# Description: reads in frequency response data for each transducer for each fish
# Returns: table of uncompensated frequency response data in long form

read_uncomp_freq_response <- function(getFishID){
  require(dplyr)
  require(readr)
  require(tidyr)
  
  # Read in frequency response data
  pingindex <- read_csv(paste0("Data/", getFishID, "/FreqResponse70_uncompTS.csv"), col_names = FALSE, skip = 1, n_max = 1) # Subset rows with ID info (for later joining)
  pingindex <- as.vector(t(pingindex))
  pingindex <- pingindex[!pingindex == "Ping_index"]
  pingindex <- pingindex[!is.na(pingindex)]
  regionname <- read_csv(paste0("Data/", getFishID, "/FreqResponse70_uncompTS.csv"), col_names = FALSE, skip = 7, n_max = 1) # Subset rows with ID info (for later joining)
  regionname <- as.vector(t(regionname))
  regionname <- regionname[!regionname == "Region_name"]
  regionname <- regionname[!is.na(regionname)]
  regionname <- gsub(pattern = " ", replace = "_", regionname)
  length(pingindex) == length(regionname)
  
  region_index <- paste(regionname, pingindex, sep = "_")
  
  freq70 <- read_csv(paste0("Data/", getFishID, "/FreqResponse70_uncompTS.csv"), skip = 8, col_names = FALSE) %>%
    filter(.[[1]] <= 89.5) # Need to remove 90kHz to avoid duplicate columns (90 in 120kHz, too)
  f120csv <- paste0("Data/", getFishID, "/FreqResponse120_uncompTS.csv")
  if(file.exists(f120csv)){
    freq120 <- read_csv(f120csv, skip = 8, col_names = FALSE)
  } else (freq120 <- NULL)
  
  f200csv <- paste0("Data/", getFishID, "/FreqResponse200_uncompTS.csv")
  if(file.exists(f200csv)){
    freq200 <- read_csv(f200csv, skip = 8, col_names = FALSE) %>%
      filter(.[[1]] >= 173 ) # LWF004, LWF005, LWF006 duplicate 160-173
  } else (freq200 <- NULL)
  fishFreq <- bind_rows(freq70, freq120, freq200)
  
  names(fishFreq) <- c("Frequency", region_index, "Variable_Index", "Variable_Name")
  fishFreq <- fishFreq %>% select(-Variable_Index, -Variable_Name)
  
  freqLong <- fishFreq %>% pivot_longer(names_to = "fish", -Frequency)
  freqLong <- freqLong %>% rename(uncompTS = value)
  freqLong$fishNum <- getFishID
  freqLong <- freqLong %>%
    relocate(fishNum) %>%
    relocate(fish, .after = fishNum) %>%
    rename(FishTrack = fish)
  
  freqLong
}