library(tidyverse)
library(writexl)

results <- readRDS("data/clean_data/oneyeardata.rds")

county_analysis <- results %>% 
  mutate(home_status=case_when(
    grepl("Eligible", haStatus) ~ "Eligible",
    grepl("IOWNV", haStatus) ~ "Ineligible, Ownership Not Verified",
    TRUE ~ "Other"
  )) %>% 
  group_by(state, county, home_status) %>% 
  summarize(total=n()) %>% 
  group_by(state, county) %>% 
  mutate(percent=round(total/sum(total, na.rm=T)*100,1)) %>% 
  pivot_wider(names_from="home_status", values_from=c("total", "percent"))

write_csv(county_analysis, "outputs/county_analysis.csv", na="")
city_analysis <- results %>% 
  mutate(home_status=case_when(
    grepl("Eligible", haStatus) ~ "Eligible",
    grepl("IOWNV", haStatus) ~ "Ineligible, Ownership Not Verified",
    TRUE ~ "Other"
  )) %>% 
  group_by(state, city, home_status) %>% 
  summarize(total=n()) %>% 
  group_by(state, city) %>% 
  mutate(percent=round(total/sum(total, na.rm=T)*100,1)) %>% 
  pivot_wider(names_from="home_status", values_from=c("total", "percent"))



state_analysis <- results %>% 
  mutate(home_status=case_when(
    grepl("Eligible", haStatus) ~ "Eligible",
    grepl("IOWNV", haStatus) ~ "Ineligible, Ownership Not Verified",
    TRUE ~ "Other"
  )) %>% 
  group_by(state, home_status) %>% 
  summarize(total=n()) %>% 
  group_by(state) %>% 
  mutate(percent=round(total/sum(total, na.rm=T)*100,1)) %>% 
  pivot_wider(names_from="home_status", values_from=c("total", "percent"))




write_xlsx(list(states= state_analysis,  counties=county_analysis,
                cities=city_analysis), path="outputs/summarized_data/fema_2018_2021.xlsx")

