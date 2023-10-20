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

