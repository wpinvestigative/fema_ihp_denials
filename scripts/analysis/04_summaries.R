library(tidyverse)
library(janitor)

# bringing in data created by 01_early_analysis.R and 03_geoid_crosswalk.R

counties_data <- read_csv("outputs/summarized_data/county_analysis_geoids_updated.csv") %>% 
  clean_names()

diversity <- read_csv("data/clean_data/mega_race_counties.csv")

counties <- left_join(counties_data, diversity, by=c("geoid"="GEOID"))


## what's the percent breakdown for white / non-white counties


counties %>% 
  group_by(majority) %>% 
  summarize(total_eligible=sum(total_eligible, na.rm=T),
            total_ineligible_ownership_not_verified=sum(total_ineligible_ownership_not_verified, na.rm=T),
            total_other=sum(total_other, na.rm=T)) %>% 
  mutate(percent_ineligible=round(total_ineligible_ownership_not_verified/(total_eligible+total_ineligible_ownership_not_verified+total_other)*100,2))


counties %>% 
  #ungroup() %>% 
  group_by(plurality) %>% 
  summarize(counties=n(),
            total_eligible=sum(total_eligible, na.rm=T),
            total_ineligible_ownership_not_verified=sum(total_ineligible_ownership_not_verified, na.rm=T),
            total_other=sum(total_other, na.rm=T)) %>% 
  mutate(percent_ineligible=round(total_ineligible_ownership_not_verified/(total_eligible+total_ineligible_ownership_not_verified+total_other)*100,2))


counties %>% 
  #group_by(plurality) %>% 
  summarize(counties=n(),
            total_eligible=sum(total_eligible, na.rm=T),
            total_ineligible_ownership_not_verified=sum(total_ineligible_ownership_not_verified, na.rm=T),
            total_other=sum(total_other, na.rm=T)) %>% 
  mutate(percent_ineligible=round(total_ineligible_ownership_not_verified/(total_eligible+total_ineligible_ownership_not_verified+total_other)*100,2))


counties %>% 
  group_by(plurality) %>% 
  summarize(counties=n(),
            total_eligible=sum(total_eligible, na.rm=T),
            total_ineligible_ownership_not_verified=sum(total_ineligible_ownership_not_verified, na.rm=T),
            total_other=sum(total_other, na.rm=T)) %>% 
  mutate(percent_ineligible=round(total_ineligible_ownership_not_verified/(total_eligible+total_ineligible_ownership_not_verified+total_other)*100,2))


state_summary <- counties %>% 
  group_by(state, plurality) %>% 
  summarize(counties=n(),
            total_eligible=sum(total_eligible, na.rm=T),
            total_ineligible_ownership_not_verified=sum(total_ineligible_ownership_not_verified, na.rm=T),
            total_other=sum(total_other, na.rm=T)) %>% 
  mutate(percent_ineligible=round(total_ineligible_ownership_not_verified/(total_eligible+total_ineligible_ownership_not_verified+total_other)*100,2))




## what's the per capita breakdown for white / non-white counties


counties %>% 
  group_by(majority) %>% 
  summarize(total_eligible=sum(total_eligible, na.rm=T),
            total_ineligible_ownership_not_verified=sum(total_ineligible_ownership_not_verified, na.rm=T),
            total_other=sum(total_other, na.rm=T)) %>% 
  mutate(percent_eligible=round(total_eligible/(total_eligible+total_ineligible_ownership_not_verified+total_other)*100,2),
         percent_ineligible=round(total_ineligible_ownership_not_verified/(total_eligible+total_ineligible_ownership_not_verified+total_other)*100,2),
         percent_other=round(total_other/(total_eligible+total_ineligible_ownership_not_verified+total_other)*100,2))


race_ineligible_percent <- counties %>% 
  group_by(plurality) %>% 
  summarize(counties=n(),
            total_eligible=sum(total_eligible, na.rm=T),
            total_ineligible_ownership_not_verified=sum(total_ineligible_ownership_not_verified, na.rm=T),
            total_other=sum(total_other, na.rm=T)) %>% 
  mutate(percent_eligible=round(total_eligible/(total_eligible+total_ineligible_ownership_not_verified+total_other)*100,2),
         percent_ineligible=round(total_ineligible_ownership_not_verified/(total_eligible+total_ineligible_ownership_not_verified+total_other)*100,2),
         percent_other=round(total_other/(total_eligible+total_ineligible_ownership_not_verified+total_other)*100,2)
         ) #%>% 
 # write_csv("outputs/summarized_data/race_ineligible_percent.csv", na="")


southern <- c("LA", "MS", "AL", "GA", "SC", "NC", "TX")


race_ineligible_southern_percent <- counties %>% 
  mutate(southern_states=case_when(
    state %in% southern ~ T,
    TRUE ~ F
  )) %>% 
  group_by(southern_states, plurality) %>% 
  summarize(counties=n(),
            total_eligible=sum(total_eligible, na.rm=T),
            total_ineligible_ownership_not_verified=sum(total_ineligible_ownership_not_verified, na.rm=T),
            total_other=sum(total_other, na.rm=T)) %>% 
  mutate(percent_eligible=round(total_eligible/(total_eligible+total_ineligible_ownership_not_verified+total_other)*100,2),
         percent_ineligible=round(total_ineligible_ownership_not_verified/(total_eligible+total_ineligible_ownership_not_verified+total_other)*100,2),
         percent_other=round(total_other/(total_eligible+total_ineligible_ownership_not_verified+total_other)*100,2)
  ) #%>% 
 # write_csv("outputs/summarized_data/race_ineligible_southern_percent.csv", na="")


## per capita

counties %>% 
  group_by(majority) %>% 
  summarize(population=sum(poverty_population, na.rm=T),
            total_eligible=sum(total_eligible, na.rm=T),
            total_ineligible_ownership_not_verified=sum(total_ineligible_ownership_not_verified, na.rm=T),
            total_other=sum(total_other, na.rm=T)) %>% 
  mutate(per_10k_eligible=round(total_eligible/population*10000,1),
         per_10k_ineligible=round(total_ineligible_ownership_not_verified/population*10000,1),
         per_10k_other=round(total_other/population*10000,1))
         

counties %>% 
  group_by(plurality) %>% 
  summarize(population=sum(poverty_population, na.rm=T),
            total_eligible=sum(total_eligible, na.rm=T),
            total_ineligible_ownership_not_verified=sum(total_ineligible_ownership_not_verified, na.rm=T),
            total_other=sum(total_other, na.rm=T))  %>% 
  mutate(per_10k_eligible=round(total_eligible/population*10000,1),
         per_10k_ineligible=round(total_ineligible_ownership_not_verified/population*10000,1),
         per_10k_other=round(total_other/population*10000,1)) %>% 
  write_csv("outputs/summarized_data/race_ineligible_per_capita.csv", na="")


poverty_ineligible_percent <- counties %>% 
  mutate(poverty_quantile=ntile(pctpov, 4)) %>% 
  group_by(poverty_quantile) %>% 
  summarize(population=sum(poverty_population, na.rm=T),
            total_eligible=sum(total_eligible, na.rm=T),
            total_ineligible_ownership_not_verified=sum(total_ineligible_ownership_not_verified, na.rm=T),
            total_other=sum(total_other, na.rm=T))  %>% 
  mutate(percent_eligible=round(total_eligible/(total_eligible+total_ineligible_ownership_not_verified+total_other)*100,2),
         percent_ineligible=round(total_ineligible_ownership_not_verified/(total_eligible+total_ineligible_ownership_not_verified+total_other)*100,2),
         percent_other=round(total_other/(total_eligible+total_ineligible_ownership_not_verified+total_other)*100,2)
  )# %>% 
  #write_csv("outputs/summarized_data/poverty_ineligible_percent.csv", na="")


poverty_ineligible_south_percent <- counties %>% 
  mutate(poverty_quantile=ntile(pctpov, 4)) %>% 
  mutate(southern_states=case_when(
    state %in% southern ~ T,
    TRUE ~ F
  )) %>% 
  group_by(southern_states, poverty_quantile) %>%   summarize(population=sum(poverty_population, na.rm=T),
            total_eligible=sum(total_eligible, na.rm=T),
            total_ineligible_ownership_not_verified=sum(total_ineligible_ownership_not_verified, na.rm=T),
            total_other=sum(total_other, na.rm=T))  %>% 
  mutate(percent_eligible=round(total_eligible/(total_eligible+total_ineligible_ownership_not_verified+total_other)*100,2),
         percent_ineligible=round(total_ineligible_ownership_not_verified/(total_eligible+total_ineligible_ownership_not_verified+total_other)*100,2),
         percent_other=round(total_other/(total_eligible+total_ineligible_ownership_not_verified+total_other)*100,2)
  ) #%>% 
  #write_csv("outputs/summarized_data/poverty_ineligible_south_percent.csv", na="")

#race_ineligible_percent 
#race_ineligible_southern_percent
#poverty_ineligible_south_percent
#poverty_ineligible_percent
#race_ineligible_percent




write_xlsx(list(`race percent`= race_ineligible_percent, `race percent southern`=race_ineligible_southern_percent,
                `poverty percent`=poverty_ineligible_percent,
                `poverty percent southern`=poverty_ineligible_south_percent,
                incidentType=incidentType_analysis,
                ownRent=ownRent_analysis,
                residenceType=residenceType_analysis), path="outputs/summarized_data/fema_ineligible_summary.xlsx")

counties %>% 
  mutate(poverty_quantile=ntile(pctpov, 4)) %>% 
  group_by(poverty_quantile) %>% 
  summarize(population=sum(poverty_population, na.rm=T),
            total_eligible=sum(total_eligible, na.rm=T),
            total_ineligible_ownership_not_verified=sum(total_ineligible_ownership_not_verified, na.rm=T),
            total_other=sum(total_other, na.rm=T))  %>% 
  mutate(per_10k_eligible=round(total_eligible/population*10000,1),
         per_10k_ineligible=round(total_ineligible_ownership_not_verified/population*10000,1),
         per_10k_other=round(total_other/population*10000,1)) %>% 
  write_csv("outputs/summarized_data/poverty_ineligible_per_capita.csv", na="")

