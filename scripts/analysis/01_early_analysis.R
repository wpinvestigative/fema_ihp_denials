library(tidyverse)
library(lubridate)
library(writexl)

#df <- readRDS("data/clean_data/20152021data.rds")
df <- readRDS("data/clean_data/20102021data.rds")

df <- df %>% 
  filter(year(ymd_hms(declarationDate))<2021) 
  
df2 <- readRDS("data/clean_data/20212021data.rds")

df <- rbind(df, df2)

territories <- c("AS", "GU", "MP", "PR", "VI")

county_analysis <- df %>% 
  filter(!state %in% territories) %>% 
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

write_csv(county_analysis, "outputs/county_analysis_updated.csv", na="")

# looking at it annually
county_analysis <- df %>% 
  filter(!state %in% territories) %>% 
  mutate(home_status=case_when(
    grepl("Eligible", haStatus) ~ "Eligible",
    grepl("IOWNV", haStatus) ~ "Ineligible, Ownership Not Verified",
    TRUE ~ "Other"
  )) %>% 
  mutate(year=year(ymd_hms(declarationDate))) %>% 
  group_by(year, state, county, home_status) %>% 
  summarize(total=n()) %>% 
  group_by(year, state, county) %>% 
  mutate(percent=round(total/sum(total, na.rm=T)*100,1)) %>% 
  pivot_wider(names_from="home_status", values_from=c("total", "percent"))

write_csv(county_analysis, "outputs/summarized_data/county_analysis_updated_annual.csv", na="")


# denials by incident type
incidentType_analysis <- df %>% 
  filter(!state %in% territories) %>% 
  mutate(home_status=case_when(
    grepl("Eligible", haStatus) ~ "Eligible",
    grepl("IOWNV", haStatus) ~ "Ineligible, Ownership Not Verified",
    TRUE ~ "Other"
  )) %>% 
  group_by(incidentType, home_status) %>% 
  summarize(total=n()) %>% 
  group_by(incidentType) %>% 
  mutate(percent=round(total/sum(total, na.rm=T)*100,1))

write_csv(incidentType_analysis, "outputs/summarized_data/incidentType_summary.csv", na="")

# denials by residence type
residenceType_analysis <- df %>% 
  filter(!state %in% territories) %>% 
  mutate(home_status=case_when(
    grepl("Eligible", haStatus) ~ "Eligible",
    grepl("IOWNV", haStatus) ~ "Ineligible, Ownership Not Verified",
    TRUE ~ "Other"
  )) %>% 
  group_by(residenceType, home_status) %>% 
  summarize(total=n()) %>% 
  group_by(residenceType) %>% 
  mutate(percent=round(total/sum(total, na.rm=T)*100,1))

write_csv(residenceType_analysis, "outputs/summarized_data/residenceType_summary.csv", na="")

# analysis by ownership
ownRent_analysis <- df %>% 
  filter(!state %in% territories) %>% 
  mutate(home_status=case_when(
    grepl("Eligible", haStatus) ~ "Eligible",
    grepl("IOWNV", haStatus) ~ "Ineligible, Ownership Not Verified",
    TRUE ~ "Other"
  )) %>% 
  group_by(ownRent, home_status) %>% 
  summarize(total=n()) %>% 
  group_by(ownRent) %>% 
  mutate(percent=round(total/sum(total, na.rm=T)*100,1))

write_csv(ownRent_analysis, "outputs/summarized_data/ownRent_summary.csv", na="")

# analysis by city
city_analysis <- df %>% 
  filter(!state %in% territories) %>% 
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


# analysis by state
state_analysis <- df %>% 
  filter(!state %in% territories) %>% 
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


#export as an Excel spreadsheet
write_xlsx(list(states= state_analysis,  counties=county_analysis,
                cities=city_analysis), path="outputs/summarized_data/fema_2015_2021.xlsx")

# analysis by income group
grossIncome_df <- df %>% 
  filter(!state %in% territories) %>% 
  mutate(home_status=case_when(
    grepl("Eligible", haStatus) ~ "Eligible",
    grepl("IOWNV", haStatus) ~ "Ineligible, Ownership Not Verified",
    TRUE ~ "Other"
  )) %>% 
  group_by(grossIncome, home_status) %>% 
  summarize(total=n()) %>% 
  group_by(grossIncome) %>% 
  mutate(percent=round(total/sum(total, na.rm=T)*100,1)) %>% 
  pivot_wider(names_from="home_status", values_from=c("total", "percent"))
