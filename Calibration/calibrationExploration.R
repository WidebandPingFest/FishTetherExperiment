### Visualizations of calibration parameters calculated via Echoview's calibration assistant
# AJR: 2023-04-23

library(tidyverse)

calDat <- read_csv("Calibration/pfest2022_calGainAndOffsets.csv")
errorDat <- read_csv("Calibration/rmse_freqSpecific_2022calFile.csv")
calDat$transducer <- factor(calDat$transducer)

calDatLong <- pivot_longer(calDat, cols= !c(transducerFreq,transducer),
                           names_to = "parameter")

pAngleOffset <- ggplot(subset(calDatLong,parameter %in% c("majorAxisAngleOffset","minorAxisAngleOffset"))) +
  geom_point(aes(x=transducerFreq,y=value, colour=parameter), size=2.5, alpha=0.6) +
  #facet_wrap(~transducer, nrow = 3, scales = "free") +
  theme_classic() +
  theme(axis.text = element_text(size=14),
        axis.title = element_text(size=16),
        strip.text = element_blank(),
        legend.text = element_text(size=12),
        legend.position  = "top") +
  ylab("Beam Angle Offset") +
  xlab("Transducer Frequency (kHz)") +
  labs(colour="") +
  geom_vline(xintercept=90, colour="red",linetype="dashed")+
  geom_vline(xintercept = 170, colour="red", linetype="dashed")+
  scale_colour_discrete(labels=c("Major Axis","Minor Axis"))
pAngleOffset

pBeamWidth <- ggplot(subset(calDatLong,parameter %in% c("majorAxisBeamWidth","minorAxisBeamWidth"))) +
  geom_point(aes(x=transducerFreq,y=value, colour=parameter), size=2.5, alpha=0.6) +
  #facet_wrap(~transducer, nrow = 3, scales = "free") +
  theme_classic() +
  theme(axis.text = element_text(size=14),
        axis.title = element_text(size=16),
        strip.text = element_blank(),
        legend.text = element_text(size=12),
        legend.position  = "top") +
  ylab("Beam Angle Width") +
  xlab("Transducer Frequency (kHz)") +
  labs(colour="") +
  geom_vline(xintercept=90, colour="red",linetype="dashed")+
  geom_vline(xintercept = 170, colour="red", linetype="dashed")+
  scale_colour_discrete(labels=c("Major Axis","Minor Axis"))
pBeamWidth

pGain <- ggplot(subset(calDatLong,parameter == "TSgain")) +
  geom_point(aes(x=transducerFreq,y=value, colour=parameter)) +
  facet_wrap(~transducer, nrow = 1, scales = "free_x")
pGain

pGain <- ggplot(subset(calDatLong,parameter == "TSgain")) +
  geom_line(aes(x=transducerFreq,y=value),linewidth=2) +
  theme_classic() +
  theme(axis.text = element_text(size=14),
        axis.title = element_text(size=16)) +
  ylab("TS Gain") +
  geom_vline(xintercept=90, colour="red",linetype="dashed")+
  geom_vline(xintercept = 170, colour="red", linetype="dashed")+
  xlab("Transducer Frequency (kHz)")
pGain

pRMSE <- ggplot(errorDat)+
  geom_line(aes(x=frequency, y=rmse), linewidth=2) +
  theme_classic() +
  theme(axis.text = element_text(size=14),
        axis.title = element_text(size=16)) +
  ylab("RMSE (dB)") +
  geom_vline(xintercept=90, colour="red",linetype="dashed")+
  geom_vline(xintercept = 170, colour="red", linetype="dashed")+
  xlab("Transducer Frequency (kHz)")
pRMSE

### Let's look at single target wideband frequency response

## On raw cal data
# Read in frequency response data
pingindex <- read_csv("Calibration/calData_frequencyResponse_2023.csv", col_names = FALSE, skip = 1, n_max = 1) # Subset rows with ID info (for later joining)
pingindex <- as.vector(t(pingindex))
pingindex <- pingindex[!pingindex == "Ping_index"]
pingindex <- pingindex[!is.na(pingindex)]
regionname <- read_csv("Calibration/calData_frequencyResponse_2023.csv", col_names = FALSE, skip = 7, n_max = 1) # Subset rows with ID info (for later joining)
regionname <- as.vector(t(regionname))
regionname <- regionname[!regionname == "Region_name"]
regionname <- regionname[!is.na(regionname)]
regionname <- gsub(pattern = " ", replace = "_", regionname)
length(pingindex) == length(regionname)

region_index <- paste(regionname, pingindex,seq(1,length(pingindex),1), sep = "_")

fishFreq <- read_csv("Calibration/calData_frequencyResponse_2023.csv", skip = 8, col_names = FALSE) # Need to remove 90kHz to avoid duplicate columns (90 in 120kHz, too)
names(fishFreq) <- c("Frequency", region_index, "Variable_Index", "Variable_Name")
fishFreq <- fishFreq %>% select(-Variable_Index, -Variable_Name)

freqLong <- fishFreq %>% pivot_longer(names_to = "fish", -Frequency)
freqLong <- freqLong %>% rename(TS = value)
freqLong <- freqLong %>%
  rename(FishTrack = fish) %>% 
  filter(TS > -90)

freqPlot <- ggplot(freqLong, aes(x=Frequency,y=TS)) +
  geom_point(alpha = 0.02)
freqPlot

## On cal data where ecs has been applied
# Read in frequency response data
pingindex <- read_csv("calData_frequencyResponse_calApplied_2023.csv", col_names = FALSE, skip = 1, n_max = 1) # Subset rows with ID info (for later joining)
pingindex <- as.vector(t(pingindex))
pingindex <- pingindex[!pingindex == "Ping_index"]
pingindex <- pingindex[!is.na(pingindex)]
regionname <- read_csv("calData_frequencyResponse_calApplied_2023.csv", col_names = FALSE, skip = 7, n_max = 1) # Subset rows with ID info (for later joining)
regionname <- as.vector(t(regionname))
regionname <- regionname[!regionname == "Region_name"]
regionname <- regionname[!is.na(regionname)]
regionname <- gsub(pattern = " ", replace = "_", regionname)
length(pingindex) == length(regionname)

region_index <- paste(regionname, pingindex,seq(1,length(pingindex),1), sep = "_")

fishFreq <- read_csv("calData_frequencyResponse_calApplied_2023.csv", skip = 8, col_names = FALSE) # Need to remove 90kHz to avoid duplicate columns (90 in 120kHz, too)
names(fishFreq) <- c("Frequency", region_index, "Variable_Index", "Variable_Name")
fishFreq <- fishFreq %>% select(-Variable_Index, -Variable_Name)

freqLong <- fishFreq %>% pivot_longer(names_to = "fish", -Frequency)
freqLong <- freqLong %>% rename(TS = value)
freqLong <- freqLong %>%
  rename(FishTrack = fish) %>% 
  filter(TS > -90)

freqPlot <- ggplot(freqLong, aes(x=Frequency,y=TS)) +
  geom_point(alpha = 0.02)
freqPlot

## On 2022 cal data
# Read in frequency response data
pingindex <- read_csv("Calibration/calData_frequencyResponse_2022.csv", col_names = FALSE, skip = 1, n_max = 1) # Subset rows with ID info (for later joining)
pingindex <- as.vector(t(pingindex))
pingindex <- pingindex[!pingindex == "Ping_index"]
pingindex <- pingindex[!is.na(pingindex)]
regionname <- read_csv("Calibration/calData_frequencyResponse_2022.csv", col_names = FALSE, skip = 7, n_max = 1) # Subset rows with ID info (for later joining)
regionname <- as.vector(t(regionname))
regionname <- regionname[!regionname == "Region_name"]
regionname <- regionname[!is.na(regionname)]
regionname <- gsub(pattern = " ", replace = "_", regionname)
length(pingindex) == length(regionname)

region_index <- paste(regionname, pingindex,seq(1,length(pingindex),1), sep = "_")

fishFreq <- read_csv("Calibration/calData_frequencyResponse_2022.csv", skip = 8, col_names = FALSE) # Need to remove 90kHz to avoid duplicate columns (90 in 120kHz, too)
names(fishFreq) <- c("Frequency", region_index, "Variable_Index", "Variable_Name")
fishFreq <- fishFreq %>% select(-Variable_Index, -Variable_Name)

freqLong <- fishFreq %>% pivot_longer(names_to = "fish", -Frequency)
freqLong <- freqLong %>% rename(TS = value)
freqLong <- freqLong %>%
  rename(FishTrack = fish) %>% 
  filter(TS > -90)

freqPlot <- ggplot(freqLong, aes(x=Frequency,y=TS)) +
  geom_point(alpha = 0.02) +
  labs(title = "2022 calibration data")
freqPlot
