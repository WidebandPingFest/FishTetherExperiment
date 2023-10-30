### Max Comp Assessment
# Script explores the influence of max beam comp (db) on TS variability
# 2023-10-29
# AR

library(tidyverse)
library(here)

# Read data
calParams <- read_csv(here("maxBeamCompAssessment","calibrationAssessment_ecsComparison.csv"))
freqResp_originalECS <- read_csv(here("maxBeamCompAssessment","TS_frequencyResponse_ecsData_calApplied_originalMaxComp_20231029.csv"))
freqResp_6dB <- read_csv(here("maxBeamCompAssessment","TS_frequencyResponse_ecsData_calApplied_6dBMaxComp_20231029.csv"))
freqResp_35dB <- read_csv(here("maxBeamCompAssessment","TS_frequencyResponse_ecsData_calApplied_35dBMaxComp_20231029.csv"))

# Look at calibration parameters with different max beam comps
calParamsPlot <- ggplot(calParams) +
  geom_point(aes(x=frequency,y=value,colour=ecsFile), alpha=0.6)+
  facet_wrap(~variable, scales = "free")
calParamsPlot

calGain <- ggplot(filter(calParams, variable == "gain")) +
  geom_point(aes(x=frequency, y=value)) +
  facet_wrap(~ecsFile)
calGain

## Look at TS variation across the three different max beam comp options

# Original ecs applied to cal data
# Read in frequency response data
pingindex <- read_csv(here("maxBeamCompAssessment","TS_frequencyResponse_ecsData_calApplied_originalMaxComp_20231029.csv"), col_names = FALSE, skip = 1, n_max = 1) # Subset rows with ID info (for later joining)
pingindex <- as.vector(t(pingindex))
pingindex <- pingindex[!pingindex == "Ping_index"]
pingindex <- pingindex[!is.na(pingindex)]
regionname <- read_csv(here("maxBeamCompAssessment","TS_frequencyResponse_ecsData_calApplied_originalMaxComp_20231029.csv"), col_names = FALSE, skip = 7, n_max = 1) # Subset rows with ID info (for later joining)
regionname <- as.vector(t(regionname))
regionname <- regionname[!regionname == "Region_name"]
regionname <- regionname[!is.na(regionname)]
regionname <- gsub(pattern = " ", replace = "_", regionname)
length(pingindex) == length(regionname)

region_index <- paste(regionname, pingindex,seq(1,length(pingindex),1), sep = "_")

fishFreqOG <- read_csv(here("maxBeamCompAssessment","TS_frequencyResponse_ecsData_calApplied_originalMaxComp_20231029.csv"), skip = 8, col_names = FALSE) # Need to remove 90kHz to avoid duplicate columns (90 in 120kHz, too)
names(fishFreqOG) <- c("Frequency", region_index, "Variable_Index", "Variable_Name")
fishFreqOG <- fishFreqOG %>% select(-Variable_Index, -Variable_Name)

freqLongOG <- fishFreqOG %>% pivot_longer(names_to = "fish", -Frequency)
freqLongOG <- freqLongOG %>% rename(TS = value)
freqLongOG <- freqLongOG %>%
  rename(FishTrack = fish) %>% 
  filter(TS > -90)

freqPlotOG <- ggplot(freqLongOG, aes(x=Frequency,y=TS)) +
  geom_point(alpha = 0.02)
freqPlotOG

# 6dB ecs applied to cal data
# Read in frequency response data
pingindex <- read_csv(here("maxBeamCompAssessment","TS_frequencyResponse_ecsData_calApplied_6dBMaxComp_20231029.csv"), col_names = FALSE, skip = 1, n_max = 1) # Subset rows with ID info (for later joining)
pingindex <- as.vector(t(pingindex))
pingindex <- pingindex[!pingindex == "Ping_index"]
pingindex <- pingindex[!is.na(pingindex)]
regionname <- read_csv(here("maxBeamCompAssessment","TS_frequencyResponse_ecsData_calApplied_6dBMaxComp_20231029.csv"), col_names = FALSE, skip = 7, n_max = 1) # Subset rows with ID info (for later joining)
regionname <- as.vector(t(regionname))
regionname <- regionname[!regionname == "Region_name"]
regionname <- regionname[!is.na(regionname)]
regionname <- gsub(pattern = " ", replace = "_", regionname)
length(pingindex) == length(regionname)

region_index <- paste(regionname, pingindex,seq(1,length(pingindex),1), sep = "_")

fishFreq6dB <- read_csv(here("maxBeamCompAssessment","TS_frequencyResponse_ecsData_calApplied_6dBMaxComp_20231029.csv"), skip = 8, col_names = FALSE) # Need to remove 90kHz to avoid duplicate columns (90 in 120kHz, too)
names(fishFreq6dB) <- c("Frequency", region_index, "Variable_Index", "Variable_Name")
fishFreq6dB <- fishFreq6dB %>% select(-Variable_Index, -Variable_Name)

freqLong6dB <- fishFreq6dB %>% pivot_longer(names_to = "fish", -Frequency)
freqLong6dB <- freqLong6dB %>% rename(TS = value)
freqLong6dB <- freqLong6dB %>%
  rename(FishTrack = fish) %>% 
  filter(TS > -90)

freqPlot6dB <- ggplot(freqLong6dB, aes(x=Frequency,y=TS)) +
  geom_point(alpha = 0.02)
freqPlot6dB

# 35dB ecs applied to cal data
# Read in frequency response data
pingindex <- read_csv(here("maxBeamCompAssessment","TS_frequencyResponse_ecsData_calApplied_35dBMaxComp_20231029.csv"), col_names = FALSE, skip = 1, n_max = 1) # Subset rows with ID info (for later joining)
pingindex <- as.vector(t(pingindex))
pingindex <- pingindex[!pingindex == "Ping_index"]
pingindex <- pingindex[!is.na(pingindex)]
regionname <- read_csv(here("maxBeamCompAssessment","TS_frequencyResponse_ecsData_calApplied_35dBMaxComp_20231029.csv"), col_names = FALSE, skip = 7, n_max = 1) # Subset rows with ID info (for later joining)
regionname <- as.vector(t(regionname))
regionname <- regionname[!regionname == "Region_name"]
regionname <- regionname[!is.na(regionname)]
regionname <- gsub(pattern = " ", replace = "_", regionname)
length(pingindex) == length(regionname)

region_index <- paste(regionname, pingindex,seq(1,length(pingindex),1), sep = "_")

fishFreq35dB <- read_csv(here("maxBeamCompAssessment","TS_frequencyResponse_ecsData_calApplied_35dBMaxComp_20231029.csv"), skip = 8, col_names = FALSE) # Need to remove 90kHz to avoid duplicate columns (90 in 120kHz, too)
names(fishFreq35dB) <- c("Frequency", region_index, "Variable_Index", "Variable_Name")
fishFreq35dB <- fishFreq35dB %>% select(-Variable_Index, -Variable_Name)

freqLong35dB <- fishFreq35dB %>% pivot_longer(names_to = "fish", -Frequency)
freqLong35dB <- freqLong35dB %>% rename(TS = value)
freqLong35dB <- freqLong35dB %>%
  rename(FishTrack = fish) %>% 
  filter(TS > -90)

freqPlot35dB <- ggplot(freqLong35dB, aes(x=Frequency,y=TS)) +
  geom_point(alpha = 0.02)
freqPlot35dB

