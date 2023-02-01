# conduct additional filtering (beyond what exists in read_tidy)
# remove fish based on being dragged (all points in a single quadrat)
# check fish aspect angle

# process:
# assign single target detection to a quadrat
# summarize targets per quadrat
# remove fish where single targets are dominated in a quadrat (>80%?)


library(dplyr)
load("E:/Projects/FishTetherExperiment/ProcessedData/processed_AnalysisData.Rdata")

trackdat <- processed_data %>% 
  mutate(Quadrat = case_when(
    Angle_major_axis >= 0 & Angle_minor_axis >=0 ~ "NE",
    Angle_major_axis >= 0 & Angle_minor_axis < 0 ~ "NW",
    Angle_major_axis < 0 & Angle_minor_axis >=0 ~ "SE",
    Angle_major_axis < 0 & Angle_minor_axis < 0 ~ "SW"
  ))

trackdat %>% group_by(fishNum) %>% 
  summarize(MedianAspectAngle = median(aspectAngle, na.rm = T)) %>% 
  filter(abs(MedianAspectAngle)>0.1)

trackdat %>% group_by(fishNum, Quadrat) %>% 
  summarize(N = n()) %>% 
  mutate(Total = sum(N)) %>% 
  mutate(ProportionTotal = N/Total) %>% 
  filter(ProportionTotal > 0.8)

