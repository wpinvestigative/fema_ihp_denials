library(tidycensus)
library(readxl)
library(janitor)

# pulls data from 2019 by county
county_diversity_2019 <- get_acs(geography = "county",
                                 variables = c("B03002_003", # white alone
                                               "B03002_004", # black alone
                                               "B03002_005", # amerian indian
                                               "B03002_006", # asian alone
                                               "B03002_007", # pi alone
                                               "B03002_012", # hispanic or latino
                                               "B03002_002" # not hispanic
                                 ),
                                 summary_var = "B03002_001", # total population
                                 survey="acs5",
                                 year=2019) %>% 
  mutate(pct = estimate/summary_est) %>% 
  mutate(race=case_when(
    variable=="B03002_003" ~"White",
    variable=="B03002_004" ~"Black",
    variable=="B03002_005" ~"American Indian",
    variable=="B03002_006" ~"Asian",
    variable=="B03002_007" ~"Pacific Islander",
    variable=="B03002_012" ~"Hispanic",
    variable=="B03002_002" ~"Not Hispanic",
    TRUE ~ "Other"
  )) %>% 
  select(GEOID, race, pct) %>% 
  pivot_wider(names_from="race", values_from="pct") %>% 
  mutate(diversity_index19= 1 - 
           ((White^2 + Black^2 + `American Indian`^2 + Asian^2 + `Pacific Islander`^2) * (Hispanic^2 + `Not Hispanic`^2))) %>% 
  select(geoid=GEOID, white_percent19=White, diversity_index19) 

write_csv(county_diversity_2019, "data/clean_data/county_diversity_2019.csv", na="")


#### majority race ---
county_race <- get_acs(geography = "county",
                       variables = c("B03002_003", 
                                     "B03002_004", 
                                     "B03002_006", 
                                     "B03002_012", 
                                     "B03002_007"),
                       summary_var = "B03002_001") %>% 
  mutate(pct = round(100 * (estimate/summary_est),2)) %>% 
  mutate(race=case_when(
    variable=="B03002_003" ~"White",
    variable=="B03002_004" ~"Black",
    variable=="B03002_006" ~"Asian",
    variable=="B03002_012" ~"Hispanic",
    variable=="B03002_007" ~"American Indian",
    TRUE ~ "Other"
  ))

write_csv(county_race, "data/clean_data/county_race.csv", na="")


 county_race_white <- county_race %>% 
   filter(race=="White") %>% 
   mutate(majority=case_when(
     pct >=50 ~ "White",
     TRUE ~"Non-White"
   )) %>% 
   select(GEOID, majority)

write_csv(county_race_white, "data/clean_data/county_race_white.csv", na="")

county_race_plurality <- county_race %>% 
  ungroup() %>% 
  select(GEOID, summary_est, pct, race) %>% 
  group_by(GEOID) %>% 
  arrange(desc(pct)) %>% 
  slice(1) %>% 
  select(GEOID, plurality=race)

write_csv(county_race_plurality, "data/clean_data/county_race_plurality.csv", na="")

county_race_wide <- county_race %>% 
  ungroup() %>% 
  select(GEOID, pct, race) %>% 
  pivot_wider(names_from="race", values_from="pct") %>% 
  clean_names() %>% 
  rename(GEOID=geoid) 
  

#### urban rural designations ----

urbru <- read_csv("data/clean_data/rural_urban_designations.csv")
urbru <- urbru %>% 
  mutate(urban_rural_cat=case_when(
    x2013_code==1 | x2013_code==2 ~ "urban",
    x2013_code==3 | x2013_code==4 ~ "suburban",
    x2013_code==5 | x2013_code==6 ~ "rural",
   )) %>% 
  select(GEOID=fips, urban_rural_code=x2013_code, urban_rural_cat)


mega_race_counties <- county_race_plurality %>% 
  left_join(county_race_white) %>% 
  left_join(county_race_wide) %>% 
  left_join(urbru)

write_csv(mega_race_counties, "data/clean_data/mega_race_counties.csv")
