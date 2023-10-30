### Frequency-specfic RMSE breakpoint analysis
# AJR: 2023-06-06

library(here)
library(tidyverse)
library(segmented)
library(patchwork)

# Read data
dat <- read_csv(here("ExploratoryAnalysis","calibrationAssessment","rmse_freqSpecific.csv"))

# Look at data
p1 <- ggplot(data = dat) +
  geom_point(aes(x=frequency, y=rmse)) +
  ylab("RMSE (dB)") +
  xlab("Frequency")
p1

# Breakpoint analysis - is there a split where we shouldn't keep data, based on calibration variance?
# 70kHz transducer
dat70 <- filter(dat, frequency <= 90)
modlm70 <- lm(rmse~frequency,dat70)
bpMod70 <- segmented(modlm70, seg.Z = ~frequency)
fit70 <- numeric(length(frequency)) * NA
fit70[complete.cases(rowSums(cbind(dat70$rmse, dat70$frequency)))] <- broken.line(bpMod70)$fit
(a <- summary(bpMod70))
plot(bpMod70)

p70 <- ggplot(data = dat70) +
  geom_point(aes(x=frequency, y=rmse)) +
  geom_line(aes(x=frequency, y=fit70),linewidth=2) +
  geom_vline(xintercept = a$psi[2], colour="red", linetype="dotted", linewidth=1.2) +
  ylab("RMSE (dB)") +
  xlab("Frequency") +
  theme_classic()+
  ylim(c(0,6.2)) #turn on or off; useful for comparing all three transducers
p70

# 120kHz transducer
dat120 <- filter(dat, frequency > 90 & frequency < 170)
modlm120 <- lm(rmse~frequency,dat120)
bpMod120 <- segmented(modlm120, seg.Z = ~frequency)
fit120 <- numeric(length(frequency)) * NA
fit120[complete.cases(rowSums(cbind(dat120$rmse, dat120$frequency)))] <- broken.line(bpMod120)$fit
(b <- summary(bpMod120))
plot(bpMod120)

p120 <- ggplot(data = dat120) +
  geom_point(aes(x=frequency, y=rmse)) +
  geom_line(aes(x=frequency, y=fit120), linewidth=2) +
  geom_vline(xintercept = b$psi[2], colour="red", linetype="dotted", linewidth=1.2) +
  ylab("") +
  xlab("Frequency") +
  theme_classic() +
  ylim(c(0,6.2)) #turn on or off; useful for comparing all three transducers
p120

# 200kHz transducer
dat200 <- filter(dat, frequency > 170)
modlm200 <- lm(rmse~frequency,dat200)
bpMod200 <- segmented(modlm, seg.Z = ~frequency)
fit200 <- numeric(length(frequency)) * NA
fit200[complete.cases(rowSums(cbind(dat200$rmse, dat200$frequency)))] <- broken.line(bpMod200)$fit
(c <- summary(bpMod200))
plot(bpMod200)

p200 <- ggplot(data = dat200) +
  geom_point(aes(x=frequency, y=rmse)) +
  geom_line(aes(x=frequency, y=fit200), linewidth=2) +
  geom_vline(xintercept = c$psi[2], colour="red", linetype="dotted", linewidth=1.2) +
  ylab("") +
  xlab("Frequency") +
  theme_classic() +
  ylim(c(0,6.2)) #turn on or off; useful for comparing all three transducers
p200

p70+p120+p200 #Plot all together
