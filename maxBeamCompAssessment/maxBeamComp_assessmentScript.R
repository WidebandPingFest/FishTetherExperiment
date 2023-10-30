### Max Comp Assessment
# Script explores the influence of max beam comp (db) on TS variability
# 2023-10-29
# AR

library(tidyverse)
library(here)

# Read data
calParams <- read_csv(here("maxBeamCompAssessment","calibrationAssessment_ecsComparison.csv"))

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

#### Look at the effect of ecs cal file + single target max comp choices ####
# Additionally, look at diff. b/w exports when beam comp is applied vs. not applied, and how many pts are retained

# OG ecs; 16dB max comp (STD), 10db data filter 
# Read in frequency response data
pingindex <- read_csv(here("maxBeamCompAssessment","LT011_originalECS_16dB_10db_beamCompApplied_freqResp.csv"), col_names = FALSE, skip = 1, n_max = 1) # Subset rows with ID info (for later joining)
pingindex <- as.vector(t(pingindex))
pingindex <- pingindex[!pingindex == "Ping_index"]
pingindex <- pingindex[!is.na(pingindex)]
regionname <- read_csv(here("maxBeamCompAssessment","LT011_originalECS_16dB_10db_beamCompApplied_freqResp.csv"), col_names = FALSE, skip = 7, n_max = 1) # Subset rows with ID info (for later joining)
regionname <- as.vector(t(regionname))
regionname <- regionname[!regionname == "Region_name"]
regionname <- regionname[!is.na(regionname)]
regionname <- gsub(pattern = " ", replace = "_", regionname)
length(pingindex) == length(regionname)

region_index <- paste(regionname, pingindex, sep = "_")

fishFreq <- read_csv(here("maxBeamCompAssessment","LT011_originalECS_16dB_10db_beamCompApplied_freqResp.csv"), skip = 8, col_names = FALSE) # Need to remove 90kHz to avoid duplicate columns (90 in 120kHz, too)
names(fishFreq) <- c("Frequency", region_index, "Variable_Index", "Variable_Name")
fishFreq <- fishFreq %>% select(-Variable_Index, -Variable_Name)

freqLong <- fishFreq %>% pivot_longer(names_to = "fish", -Frequency)
freqLong <- freqLong %>% rename(TS = value)
freqLong <- freqLong %>%
  rename(FishTrack = fish) 

freqPlot <- ggplot(freqLong, aes(x=Frequency,y=TS)) +
  geom_point(alpha = 0.02) +
  labs(title = "LT011; OG ecs, 16dB max comp, 10 db data filter")
freqPlot

# 6dB ecs; 16dB max comp (STD), 10db data filter 
# Read in frequency response data
pingindex <- read_csv(here("maxBeamCompAssessment","LT011_6dBECS_16dB_10db_beamCompApplied_freqResp.csv"), col_names = FALSE, skip = 1, n_max = 1) # Subset rows with ID info (for later joining)
pingindex <- as.vector(t(pingindex))
pingindex <- pingindex[!pingindex == "Ping_index"]
pingindex <- pingindex[!is.na(pingindex)]
regionname <- read_csv(here("maxBeamCompAssessment","LT011_6dBECS_16dB_10db_beamCompApplied_freqResp.csv"), col_names = FALSE, skip = 7, n_max = 1) # Subset rows with ID info (for later joining)
regionname <- as.vector(t(regionname))
regionname <- regionname[!regionname == "Region_name"]
regionname <- regionname[!is.na(regionname)]
regionname <- gsub(pattern = " ", replace = "_", regionname)
length(pingindex) == length(regionname)

region_index <- paste(regionname, pingindex, sep = "_")

fishFreq <- read_csv(here("maxBeamCompAssessment","LT011_6dBECS_16dB_10db_beamCompApplied_freqResp.csv"), skip = 8, col_names = FALSE) # Need to remove 90kHz to avoid duplicate columns (90 in 120kHz, too)
names(fishFreq) <- c("Frequency", region_index, "Variable_Index", "Variable_Name")
fishFreq <- fishFreq %>% select(-Variable_Index, -Variable_Name)

freqLong <- fishFreq %>% pivot_longer(names_to = "fish", -Frequency)
freqLong <- freqLong %>% rename(TS = value)
freqLong <- freqLong %>%
  rename(FishTrack = fish) 

freqPlot <- ggplot(freqLong, aes(x=Frequency,y=TS)) +
  geom_point(alpha = 0.02) +
  labs(title = "LT011; 6dB ecs, 16dB max comp, 10 db data filter")
freqPlot

# 35dB ecs; 16dB max comp (STD), 10db data filter 
# Read in frequency response data
pingindex <- read_csv(here("maxBeamCompAssessment","LT011_35dBECS_16dB_10db_beamCompApplied_freqResp.csv"), col_names = FALSE, skip = 1, n_max = 1) # Subset rows with ID info (for later joining)
pingindex <- as.vector(t(pingindex))
pingindex <- pingindex[!pingindex == "Ping_index"]
pingindex <- pingindex[!is.na(pingindex)]
regionname <- read_csv(here("maxBeamCompAssessment","LT011_35dBECS_16dB_10db_beamCompApplied_freqResp.csv"), col_names = FALSE, skip = 7, n_max = 1) # Subset rows with ID info (for later joining)
regionname <- as.vector(t(regionname))
regionname <- regionname[!regionname == "Region_name"]
regionname <- regionname[!is.na(regionname)]
regionname <- gsub(pattern = " ", replace = "_", regionname)
length(pingindex) == length(regionname)

region_index <- paste(regionname, pingindex, sep = "_")

fishFreq <- read_csv(here("maxBeamCompAssessment","LT011_35dBECS_16dB_10db_beamCompApplied_freqResp.csv"), skip = 8, col_names = FALSE) # Need to remove 90kHz to avoid duplicate columns (90 in 120kHz, too)
names(fishFreq) <- c("Frequency", region_index, "Variable_Index", "Variable_Name")
fishFreq <- fishFreq %>% select(-Variable_Index, -Variable_Name)

freqLong <- fishFreq %>% pivot_longer(names_to = "fish", -Frequency)
freqLong <- freqLong %>% rename(TS = value)
freqLong <- freqLong %>%
  rename(FishTrack = fish) 

freqPlot <- ggplot(freqLong, aes(x=Frequency,y=TS)) +
  geom_point(alpha = 0.02) +
  labs(title = "LT011; 35dB ecs, 16dB max comp, 10 db data filter")
freqPlot

# 6dB ecs; 12dB max comp (STD), 10db data filter 
# Read in frequency response data
pingindex <- read_csv(here("maxBeamCompAssessment","LT011_6dBECS_12dB_10db_beamCompApplied_freqResp.csv"), col_names = FALSE, skip = 1, n_max = 1) # Subset rows with ID info (for later joining)
pingindex <- as.vector(t(pingindex))
pingindex <- pingindex[!pingindex == "Ping_index"]
pingindex <- pingindex[!is.na(pingindex)]
regionname <- read_csv(here("maxBeamCompAssessment","LT011_6dBECS_12dB_10db_beamCompApplied_freqResp.csv"), col_names = FALSE, skip = 7, n_max = 1) # Subset rows with ID info (for later joining)
regionname <- as.vector(t(regionname))
regionname <- regionname[!regionname == "Region_name"]
regionname <- regionname[!is.na(regionname)]
regionname <- gsub(pattern = " ", replace = "_", regionname)
length(pingindex) == length(regionname)

region_index <- paste(regionname, pingindex, sep = "_")

fishFreq <- read_csv(here("maxBeamCompAssessment","LT011_6dBECS_12dB_10db_beamCompApplied_freqResp.csv"), skip = 8, col_names = FALSE) # Need to remove 90kHz to avoid duplicate columns (90 in 120kHz, too)
names(fishFreq) <- c("Frequency", region_index, "Variable_Index", "Variable_Name")
fishFreq <- fishFreq %>% select(-Variable_Index, -Variable_Name)

freqLong <- fishFreq %>% pivot_longer(names_to = "fish", -Frequency)
freqLong <- freqLong %>% rename(TS = value)
freqLong <- freqLong %>%
  rename(FishTrack = fish) 

freqPlot <- ggplot(freqLong, aes(x=Frequency,y=TS)) +
  geom_point(alpha = 0.02) +
  labs(title = "LT011; 6dB ecs, 12dB max comp, 10 db data filter")
freqPlot

# 6dB ecs; 16dB max comp (STD), 16db data filter  
# Read in frequency response data
pingindex <- read_csv(here("maxBeamCompAssessment","LT011_6dBECS_16dB_16db_beamCompApplied_freqResp.csv"), col_names = FALSE, skip = 1, n_max = 1) # Subset rows with ID info (for later joining)
pingindex <- as.vector(t(pingindex))
pingindex <- pingindex[!pingindex == "Ping_index"]
pingindex <- pingindex[!is.na(pingindex)]
regionname <- read_csv(here("maxBeamCompAssessment","LT011_6dBECS_16dB_16db_beamCompApplied_freqResp.csv"), col_names = FALSE, skip = 7, n_max = 1) # Subset rows with ID info (for later joining)
regionname <- as.vector(t(regionname))
regionname <- regionname[!regionname == "Region_name"]
regionname <- regionname[!is.na(regionname)]
regionname <- gsub(pattern = " ", replace = "_", regionname)
length(pingindex) == length(regionname)

region_index <- paste(regionname, pingindex, sep = "_")

fishFreq <- read_csv(here("maxBeamCompAssessment","LT011_6dBECS_16dB_16db_beamCompApplied_freqResp.csv"), skip = 8, col_names = FALSE) # Need to remove 90kHz to avoid duplicate columns (90 in 120kHz, too)
names(fishFreq) <- c("Frequency", region_index, "Variable_Index", "Variable_Name")
fishFreq <- fishFreq %>% select(-Variable_Index, -Variable_Name)

freqLong <- fishFreq %>% pivot_longer(names_to = "fish", -Frequency)
freqLong <- freqLong %>% rename(TS = value)
freqLong <- freqLong %>%
  rename(FishTrack = fish) 

freqPlot <- ggplot(freqLong, aes(x=Frequency,y=TS)) +
  geom_point(alpha = 0.02) +
  labs(title = "LT011; 6dB ecs, 16dB max comp, 16 db data filter")
freqPlot

# 6dB ecs; 16dB max comp (STD), 6db data filter  
# Read in frequency response data
pingindex <- read_csv(here("maxBeamCompAssessment","LT011_6dBECS_16dB_6db_beamCompApplied_freqResp.csv"), col_names = FALSE, skip = 1, n_max = 1) # Subset rows with ID info (for later joining)
pingindex <- as.vector(t(pingindex))
pingindex <- pingindex[!pingindex == "Ping_index"]
pingindex <- pingindex[!is.na(pingindex)]
regionname <- read_csv(here("maxBeamCompAssessment","LT011_6dBECS_16dB_6db_beamCompApplied_freqResp.csv"), col_names = FALSE, skip = 7, n_max = 1) # Subset rows with ID info (for later joining)
regionname <- as.vector(t(regionname))
regionname <- regionname[!regionname == "Region_name"]
regionname <- regionname[!is.na(regionname)]
regionname <- gsub(pattern = " ", replace = "_", regionname)
length(pingindex) == length(regionname)

region_index <- paste(regionname, pingindex, sep = "_")

fishFreq <- read_csv(here("maxBeamCompAssessment","LT011_6dBECS_16dB_6db_beamCompApplied_freqResp.csv"), skip = 8, col_names = FALSE) # Need to remove 90kHz to avoid duplicate columns (90 in 120kHz, too)
names(fishFreq) <- c("Frequency", region_index, "Variable_Index", "Variable_Name")
fishFreq <- fishFreq %>% select(-Variable_Index, -Variable_Name)

freqLong <- fishFreq %>% pivot_longer(names_to = "fish", -Frequency)
freqLong <- freqLong %>% rename(TS = value)
freqLong <- freqLong %>%
  rename(FishTrack = fish) 

freqPlot <- ggplot(freqLong, aes(x=Frequency,y=TS)) +
  geom_point(alpha = 0.02) +
  labs(title = "LT011; 6dB ecs, 16dB max comp, 6 db data filter")
freqPlot

## Take home: reducing the data filter compensation (not at STD level) is what controls 
# TS variability across a frequency range. Also leads to reduction in detectable fish
# tracks and data points

## Create frequency specific compensation ####
# LT011, 6db ecs, 16db max comp (STD), 6 db data filter
# Read in COMPENSATED frequency response data
pingindex <- read_csv(here("maxBeamCompAssessment","LT011_6dBECS_16dB_6db_beamCompApplied_freqResp.csv"), col_names = FALSE, skip = 1, n_max = 1) # Subset rows with ID info (for later joining)
pingindex <- as.vector(t(pingindex))
pingindex <- pingindex[!pingindex == "Ping_index"]
pingindex <- pingindex[!is.na(pingindex)]
regionname <- read_csv(here("maxBeamCompAssessment","LT011_6dBECS_16dB_6db_beamCompApplied_freqResp.csv"), col_names = FALSE, skip = 7, n_max = 1) # Subset rows with ID info (for later joining)
regionname <- as.vector(t(regionname))
regionname <- regionname[!regionname == "Region_name"]
regionname <- regionname[!is.na(regionname)]
regionname <- gsub(pattern = " ", replace = "_", regionname)
length(pingindex) == length(regionname)

region_index <- paste(regionname, pingindex, sep = "_")

fishFreq <- read_csv(here("maxBeamCompAssessment","LT011_6dBECS_16dB_6db_beamCompApplied_freqResp.csv"), skip = 8, col_names = FALSE) # Need to remove 90kHz to avoid duplicate columns (90 in 120kHz, too)
names(fishFreq) <- c("Frequency", region_index, "Variable_Index", "Variable_Name")
fishFreq <- fishFreq %>% select(-Variable_Index, -Variable_Name)

freqLong <- fishFreq %>% pivot_longer(names_to = "fish", -Frequency)
freqLong <- freqLong %>% rename(TS = value)
freqLong <- freqLong %>%
  rename(FishTrack = fish)

# Read in NON-COMPENSATED frequency response data
pingindexNC <- read_csv(here("maxBeamCompAssessment","LT011_6dBECS_16dB_6db_beamCompNOTApplied_freqResp.csv"), col_names = FALSE, skip = 1, n_max = 1) # Subset rows with ID info (for later joining)
pingindexNC <- as.vector(t(pingindexNC))
pingindexNC <- pingindexNC[!pingindexNC == "Ping_index"]
pingindexNC <- pingindexNC[!is.na(pingindexNC)]
regionnameNC <- read_csv(here("maxBeamCompAssessment","LT011_6dBECS_16dB_6db_beamCompNOTApplied_freqResp.csv"), col_names = FALSE, skip = 7, n_max = 1) # Subset rows with ID info (for later joining)
regionnameNC <- as.vector(t(regionnameNC))
regionnameNC <- regionnameNC[!regionnameNC == "Region_name"]
regionnameNC <- regionnameNC[!is.na(regionnameNC)]
regionnameNC <- gsub(pattern = " ", replace = "_", regionnameNC)
length(pingindexNC) == length(regionnameNC)

region_indexNC <- paste(regionnameNC, pingindexNC, sep = "_")

fishFreqNC <- read_csv(here("maxBeamCompAssessment","LT011_6dBECS_16dB_6db_beamCompNOTApplied_freqResp.csv"), skip = 8, col_names = FALSE) # Need to remove 90kHz to avoid duplicate columns (90 in 120kHz, too)
names(fishFreqNC) <- c("Frequency", region_indexNC, "Variable_Index", "Variable_Name")
fishFreqNC <- fishFreqNC %>% select(-Variable_Index, -Variable_Name)

freqLongNC <- fishFreqNC %>% pivot_longer(names_to = "fish", -Frequency)
freqLongNC <- freqLongNC %>% rename(TS_uncomp = value)
freqLongNC <- freqLongNC %>%
  rename(FishTrack = fish) 

freqCompData <- full_join(freqLong,freqLongNC) %>% 
  mutate(compDif = TS-TS_uncomp)
#df length = 453047

freqCompData_filtered <- filter(freqCompData, compDif < 6.000001 & compDif >0)
#df length = 374860
length(unique(freqCompData_filtered$FishTrack)) #1061,represents STs


freqPlot_comp <- ggplot(freqCompData_filtered, aes(x=Frequency,y=TS)) +
  geom_point(aes(colour=compDif), alpha = 0.02) +
  scale_color_viridis_c()+
  labs(title = "LT011; 6dB ecs, 16dB max comp, 6 db data filter; freq specific comp filter")
freqPlot_comp

# LT011, 6db ecs, 16db max comp (STD), 16 db data filter
# Read in COMPENSATED frequency response data
pingindex <- read_csv(here("maxBeamCompAssessment","LT011_6dBECS_16dB_16db_beamCompApplied_freqResp.csv"), col_names = FALSE, skip = 1, n_max = 1) # Subset rows with ID info (for later joining)
pingindex <- as.vector(t(pingindex))
pingindex <- pingindex[!pingindex == "Ping_index"]
pingindex <- pingindex[!is.na(pingindex)]
regionname <- read_csv(here("maxBeamCompAssessment","LT011_6dBECS_16dB_16db_beamCompApplied_freqResp.csv"), col_names = FALSE, skip = 7, n_max = 1) # Subset rows with ID info (for later joining)
regionname <- as.vector(t(regionname))
regionname <- regionname[!regionname == "Region_name"]
regionname <- regionname[!is.na(regionname)]
regionname <- gsub(pattern = " ", replace = "_", regionname)
length(pingindex) == length(regionname)

region_index <- paste(regionname, pingindex, sep = "_")

fishFreq <- read_csv(here("maxBeamCompAssessment","LT011_6dBECS_16dB_16db_beamCompApplied_freqResp.csv"), skip = 8, col_names = FALSE) # Need to remove 90kHz to avoid duplicate columns (90 in 120kHz, too)
names(fishFreq) <- c("Frequency", region_index, "Variable_Index", "Variable_Name")
fishFreq <- fishFreq %>% select(-Variable_Index, -Variable_Name)

freqLong <- fishFreq %>% pivot_longer(names_to = "fish", -Frequency)
freqLong <- freqLong %>% rename(TS = value)
freqLong <- freqLong %>%
  rename(FishTrack = fish)

# Read in NON-COMPENSATED frequency response data
pingindexNC <- read_csv(here("maxBeamCompAssessment","LT011_6dBECS_16dB_16db_beamCompNOTApplied_freqResp.csv"), col_names = FALSE, skip = 1, n_max = 1) # Subset rows with ID info (for later joining)
pingindexNC <- as.vector(t(pingindexNC))
pingindexNC <- pingindexNC[!pingindexNC == "Ping_index"]
pingindexNC <- pingindexNC[!is.na(pingindexNC)]
regionnameNC <- read_csv(here("maxBeamCompAssessment","LT011_6dBECS_16dB_16db_beamCompNOTApplied_freqResp.csv"), col_names = FALSE, skip = 7, n_max = 1) # Subset rows with ID info (for later joining)
regionnameNC <- as.vector(t(regionnameNC))
regionnameNC <- regionnameNC[!regionnameNC == "Region_name"]
regionnameNC <- regionnameNC[!is.na(regionnameNC)]
regionnameNC <- gsub(pattern = " ", replace = "_", regionnameNC)
length(pingindexNC) == length(regionnameNC)

region_indexNC <- paste(regionnameNC, pingindexNC, sep = "_")

fishFreqNC <- read_csv(here("maxBeamCompAssessment","LT011_6dBECS_16dB_16db_beamCompNOTApplied_freqResp.csv"), skip = 8, col_names = FALSE) # Need to remove 90kHz to avoid duplicate columns (90 in 120kHz, too)
names(fishFreqNC) <- c("Frequency", region_indexNC, "Variable_Index", "Variable_Name")
fishFreqNC <- fishFreqNC %>% select(-Variable_Index, -Variable_Name)

freqLongNC <- fishFreqNC %>% pivot_longer(names_to = "fish", -Frequency)
freqLongNC <- freqLongNC %>% rename(TS_uncomp = value)
freqLongNC <- freqLongNC %>%
  rename(FishTrack = fish) 

freqCompData <- inner_join(freqLong,freqLongNC) %>% 
  mutate(compDif = TS-TS_uncomp)
#df length = 1561989

freqCompData_filtered <- filter(freqCompData, compDif < 6.000001 & compDif > 0)
#df length = 442905
length(unique(freqCompData_filtered$FishTrack)) #3059,represents STs

freqPlot_comp <- ggplot(freqCompData_filtered, aes(x=Frequency,y=TS)) +
  geom_point(aes(colour=compDif), alpha = 0.02) +
  scale_color_viridis_c()+
  labs(title = "LT011; 6dB ecs, 16dB max comp, 16 db data filter; freq specific comp filter")
freqPlot_comp

# LT011, 6db ecs, 16db max comp (STD), 12 db data filter
# Read in COMPENSATED frequency response data
pingindex <- read_csv(here("maxBeamCompAssessment","LT011_6dBECS_16dB_12db_beamCompApplied_freqResp.csv"), col_names = FALSE, skip = 1, n_max = 1) # Subset rows with ID info (for later joining)
pingindex <- as.vector(t(pingindex))
pingindex <- pingindex[!pingindex == "Ping_index"]
pingindex <- pingindex[!is.na(pingindex)]
regionname <- read_csv(here("maxBeamCompAssessment","LT011_6dBECS_16dB_12db_beamCompApplied_freqResp.csv"), col_names = FALSE, skip = 7, n_max = 1) # Subset rows with ID info (for later joining)
regionname <- as.vector(t(regionname))
regionname <- regionname[!regionname == "Region_name"]
regionname <- regionname[!is.na(regionname)]
regionname <- gsub(pattern = " ", replace = "_", regionname)
length(pingindex) == length(regionname)

region_index <- paste(regionname, pingindex, sep = "_")

fishFreq <- read_csv(here("maxBeamCompAssessment","LT011_6dBECS_16dB_12db_beamCompApplied_freqResp.csv"), skip = 8, col_names = FALSE) # Need to remove 90kHz to avoid duplicate columns (90 in 120kHz, too)
names(fishFreq) <- c("Frequency", region_index, "Variable_Index", "Variable_Name")
fishFreq <- fishFreq %>% select(-Variable_Index, -Variable_Name)

freqLong <- fishFreq %>% pivot_longer(names_to = "fish", -Frequency)
freqLong <- freqLong %>% rename(TS = value)
freqLong <- freqLong %>%
  rename(FishTrack = fish)

# Read in NON-COMPENSATED frequency response data
pingindexNC <- read_csv(here("maxBeamCompAssessment","LT011_6dBECS_16dB_12db_beamCompNOTApplied_freqResp.csv"), col_names = FALSE, skip = 1, n_max = 1) # Subset rows with ID info (for later joining)
pingindexNC <- as.vector(t(pingindexNC))
pingindexNC <- pingindexNC[!pingindexNC == "Ping_index"]
pingindexNC <- pingindexNC[!is.na(pingindexNC)]
regionnameNC <- read_csv(here("maxBeamCompAssessment","LT011_6dBECS_16dB_12db_beamCompNOTApplied_freqResp.csv"), col_names = FALSE, skip = 7, n_max = 1) # Subset rows with ID info (for later joining)
regionnameNC <- as.vector(t(regionnameNC))
regionnameNC <- regionnameNC[!regionnameNC == "Region_name"]
regionnameNC <- regionnameNC[!is.na(regionnameNC)]
regionnameNC <- gsub(pattern = " ", replace = "_", regionnameNC)
length(pingindexNC) == length(regionnameNC)

region_indexNC <- paste(regionnameNC, pingindexNC, sep = "_")

fishFreqNC <- read_csv(here("maxBeamCompAssessment","LT011_6dBECS_16dB_12db_beamCompNOTApplied_freqResp.csv"), skip = 8, col_names = FALSE) # Need to remove 90kHz to avoid duplicate columns (90 in 120kHz, too)
names(fishFreqNC) <- c("Frequency", region_indexNC, "Variable_Index", "Variable_Name")
fishFreqNC <- fishFreqNC %>% select(-Variable_Index, -Variable_Name)

freqLongNC <- fishFreqNC %>% pivot_longer(names_to = "fish", -Frequency)
freqLongNC <- freqLongNC %>% rename(TS_uncomp = value)
freqLongNC <- freqLongNC %>%
  rename(FishTrack = fish) 

freqCompData <- inner_join(freqLong,freqLongNC) %>% 
  mutate(compDif = TS-TS_uncomp)
#df length = 1561989

freqCompData_filtered <- filter(freqCompData, compDif < 6.000001 & compDif > 0)
#df length = 442905
length(unique(freqCompData_filtered$FishTrack)) #2352,represents STs

freqPlot_comp <- ggplot(freqCompData_filtered, aes(x=Frequency,y=TS)) +
  geom_point(aes(colour=compDif), alpha = 0.02) +
  scale_color_viridis_c()+
  labs(title = "LT011; 6dB ecs, 16dB max comp, 12 db data filter; freq specific comp filter")
freqPlot_comp

### What happens when aspect angle is brought in ####
# LT011, 6db ecs, 16db max comp (STD), 6 db data filter
# Read in COMPENSATED frequency response data
pingindex <- read_csv(here("maxBeamCompAssessment","LT011_6dBECS_16dB_6db_beamCompApplied_freqResp.csv"), col_names = FALSE, skip = 1, n_max = 1) # Subset rows with ID info (for later joining)
pingindex <- as.vector(t(pingindex))
pingindex <- pingindex[!pingindex == "Ping_index"]
pingindex <- pingindex[!is.na(pingindex)]
regionname <- read_csv(here("maxBeamCompAssessment","LT011_6dBECS_16dB_6db_beamCompApplied_freqResp.csv"), col_names = FALSE, skip = 7, n_max = 1) # Subset rows with ID info (for later joining)
regionname <- as.vector(t(regionname))
regionname <- regionname[!regionname == "Region_name"]
regionname <- regionname[!is.na(regionname)]
regionname <- gsub(pattern = " ", replace = "_", regionname)
length(pingindex) == length(regionname)

region_index <- paste(regionname, pingindex, sep = "_")

fishFreq <- read_csv(here("maxBeamCompAssessment","LT011_6dBECS_16dB_6db_beamCompApplied_freqResp.csv"), skip = 8, col_names = FALSE) # Need to remove 90kHz to avoid duplicate columns (90 in 120kHz, too)
names(fishFreq) <- c("Frequency", region_index, "Variable_Index", "Variable_Name")
fishFreq <- fishFreq %>% select(-Variable_Index, -Variable_Name)

freqLong <- fishFreq %>% pivot_longer(names_to = "fish", -Frequency)
freqLong <- freqLong %>% rename(TS = value)
freqLong <- freqLong %>%
  rename(FishTrack = fish)

# Read in NON-COMPENSATED frequency response data
pingindexNC <- read_csv(here("maxBeamCompAssessment","LT011_6dBECS_16dB_6db_beamCompNOTApplied_freqResp.csv"), col_names = FALSE, skip = 1, n_max = 1) # Subset rows with ID info (for later joining)
pingindexNC <- as.vector(t(pingindexNC))
pingindexNC <- pingindexNC[!pingindexNC == "Ping_index"]
pingindexNC <- pingindexNC[!is.na(pingindexNC)]
regionnameNC <- read_csv(here("maxBeamCompAssessment","LT011_6dBECS_16dB_6db_beamCompNOTApplied_freqResp.csv"), col_names = FALSE, skip = 7, n_max = 1) # Subset rows with ID info (for later joining)
regionnameNC <- as.vector(t(regionnameNC))
regionnameNC <- regionnameNC[!regionnameNC == "Region_name"]
regionnameNC <- regionnameNC[!is.na(regionnameNC)]
regionnameNC <- gsub(pattern = " ", replace = "_", regionnameNC)
length(pingindexNC) == length(regionnameNC)

region_indexNC <- paste(regionnameNC, pingindexNC, sep = "_")

fishFreqNC <- read_csv(here("maxBeamCompAssessment","LT011_6dBECS_16dB_6db_beamCompNOTApplied_freqResp.csv"), skip = 8, col_names = FALSE) # Need to remove 90kHz to avoid duplicate columns (90 in 120kHz, too)
names(fishFreqNC) <- c("Frequency", region_indexNC, "Variable_Index", "Variable_Name")
fishFreqNC <- fishFreqNC %>% select(-Variable_Index, -Variable_Name)

freqLongNC <- fishFreqNC %>% pivot_longer(names_to = "fish", -Frequency)
freqLongNC <- freqLongNC %>% rename(TS_uncomp = value)
freqLongNC <- freqLongNC %>%
  rename(FishTrack = fish) 

freqCompData <- full_join(freqLong,freqLongNC) %>% 
  mutate(compDif = TS-TS_uncomp)
#df length = 453047

freqCompData_filtered <- filter(freqCompData, compDif < 6.000001 & compDif > - 6)
#df length = 374860

## Read in FishTrack data, create aspect angle variable
sinTar <- read.csv(here("maxBeamCompAssessment","LT011_6dBECS_16dB_6db_fishTracks (targets).CSV")) %>% # Bring in single target data
  mutate(FishTrack = paste(Region_name, Ping_number, sep = "_")) %>%
  select(
    Region_name, FishTrack, Ping_time, Target_range, Angle_minor_axis, Angle_major_axis, Distance_minor_axis,
    Distance_major_axis, StandDev_Angles_Minor_Axis, StandDev_Angles_Major_Axis, Target_true_depth
  ) %>%
  mutate(
    fishNum = "LT011",
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

# Join freq and aspect data
freqComp_aspect <- freqCompData_filtered %>% 
  left_join(select(sinTar,FishTrack,aspectAngle))

# Plot with aspect angle as colour gradient
freqAspectPlot_comp <- ggplot(freqComp_aspect, aes(x=Frequency,y=TS)) +
  geom_point(aes(colour=aspectAngle),alpha = 0.02) +
  scale_color_viridis_c() +
  labs(title = "LT011; 6dB ecs, 16dB max comp, 6 dB data filter; freq specific comp filter")
freqAspectPlot_comp

## Let's limit aspect angle to > -20 and < 20 degrees
aspectLimitedDat <- freqComp_aspect %>% 
  filter(aspectAngle > -20 & aspectAngle < 20)

# Plot with aspect angle as colour gradient
limitedAspectPlot_comp <- ggplot(aspectLimitedDat, aes(x=Frequency,y=TS)) +
  geom_point(aes(colour=aspectAngle),alpha = 0.02) +
  scale_color_viridis_c() +
  labs(title = "LT011; 6dB ecs, 16dB max comp, 6 dB data filter; freq specific comp filter")
limitedAspectPlot_comp
