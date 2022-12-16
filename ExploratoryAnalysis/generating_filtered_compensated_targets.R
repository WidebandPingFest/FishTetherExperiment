source("E:/Projects/FishTetherExperiment/Analysis_Scripts/custom_functions.R", echo=F)
# read_tidy_export_EVfiles("LT016")

library(dplyr)
fish<-dir("Data")
sumtable <- data.frame(Fish = NULL, Npings = NULL, Npings_filtered = NULL)

find_filtered <- function(fish){
  comp <- read_comp_freq_response(fish)
  uncomp <- read_uncomp_freq_response(fish)
  tsdiff <- inner_join(comp, uncomp)
  tsdiff <- tsdiff %>% 
    mutate(TSdifference = TS-uncompTS) 
  Npings_all <- length(unique(tsdiff$FishTrack))
  tsdiff_filtered <- tsdiff %>% group_by(fishNum, FishTrack) %>% 
  summarize(MaxTSdiff = max(TSdifference)) %>% 
  filter(MaxTSdiff<=6)
  Npings_filtered <- length(unique(tsdiff_filtered$FishTrack))
  data.frame(fish, Npings_all, Npings_filtered)
}

all_fish_summary<-bind_rows(lapply(fish, find_filtered))

all_fish_summary <- all_fish_summary %>% 
  mutate(PropRemaining = round(Npings_filtered/Npings_all,2)) %>% 
  arrange(PropRemaining)

write_csv(all_fish_summary, file="ExploratoryAnalysis/accepted_6dB_TS_compensation_singletargets_summary.csv")


find_filtered_fishTracks <- function(fish){
  comp <- read_comp_freq_response(fish)
  uncomp <- read_uncomp_freq_response(fish)
  tsdiff <- inner_join(comp, uncomp)
  tsdiff <- tsdiff %>% 
    mutate(TSdifference = TS-uncompTS) 
  Npings_all <- length(unique(tsdiff$FishTrack))
  tsdiff_filtered <- tsdiff %>% group_by(fishNum, FishTrack) %>% 
    summarize(MaxTSdiff = max(TSdifference)) %>% 
    filter(MaxTSdiff<=6)
  tsdiff_filtered
}

all_fish_to_analyze<-bind_rows(lapply(fish, find_filtered_fishTracks))
write_csv(all_fish_to_analyze, file="ExploratoryAnalysis/accepted_6dB_TS_compensation_singletargets.csv")
