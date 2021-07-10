library(tidycensus)

# bring in data created in 01_early_analysis.R
county_analysis <- read_csv("outputs/summarized_data/county_analysis_updated.csv", na="")

# add in DC and Puerto Rico
state_abb <- c(state.abb, "DC", "PR")
state_names <- c(state.name, "District of Columbia", "Puerto Rico")
states_df <- data.frame(state=state_abb, state_name=state_names)

# clean up the county names to join later
county_analysis <- left_join(county_analysis, states_df)
county_analysis <- county_analysis %>% 
   mutate(county_name=gsub(" \\(.*", "", county))



county_diversity <- function(year=2019) {
  # pulls population data from the Census by county
  county_diversity_df <- get_acs(geography = "county",
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
                                 year=year) %>%
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
    mutate(year=year)
  return(county_diversity_df)
}

census2019 <- county_diversity(year=2019)

# pulls in poverty figures by county from the Census
county_pov <- get_acs(geography = "county",
                      variables = "B17001_002",
                      summary_var = "B17001_001") %>% 
  mutate(pctpov = 100 * (estimate/summary_est))

county_pov <- county_pov %>% 
  select(GEOID, NAME, poverty_estimate=estimate, poverty_population=summary_est,
         pctpov)

# cleaning up the county names to join easier with the FEMA data
county_pov <- county_pov %>% 
  mutate(state_name=gsub(".*, ", "", NAME)) %>% 
  mutate(county_name=gsub(", .*", "", NAME)) %>% 
  mutate(county_name=gsub(" County.*", "", county_name)) %>% 
  mutate(county_name=gsub(" Parish.*", "", county_name)) %>% 
  mutate(county_name=gsub(" city.*", "", county_name))
  

# renaming FEMA counties so they match with Census counties
county_analysis <- county_analysis %>% 
  mutate(county_name=case_when(
    county_name=="Oglala Sioux Tribe of the Pine Ridge Reservation" ~ "Oglala Lakota",
    county_name=="La Salle" & state == "LA" ~ "LaSalle",
    county_name=="Big Cypress Indian Reservation" ~ "Hendry",
    county_name=="Tampa Reservation" ~ "Hendry",
    county_name=="Big Cypress Indian Reservation" ~ "Hillsborough",
    county_name=="Fort Pierce Indian Reservation" ~ "St. Lucie",
    county_name=="Hollywood Indian Reservation" ~ "Hendry",
    county_name=="Immokalee Indian Reservation" ~ "Collier",
    county_name=="Brighton Indian Reservation" ~ "Glades",
    county_name=="Anchorage" ~ "Anchorage Municipality",
    county_name=="Matanuska-Susitna" ~ "Matanuska-Susitna Borough",
    county_name=="Kenai Peninsula" ~ "Kenai Peninsula Borough",
    county_name=="Santee Indian Reservation" ~ "Knox",
    county_name=="La Salle" & state=="IL" ~ "LaSalle",
    county_name=="Mississippi Choctaw Indian Reservation" ~ "Neshoba",
    county_name=="Alaska Gateway Regional Educational Attendance Area" ~ "Ketchikan Gateway Borough",
    county_name=="Lower Yukon Regional Educational Attendance Area" ~ "Yukon-Koyukuk Census Area",
    county_name=="Yukon Flats Regional Educational Attendance Area" ~ "Yukon-Koyukuk Census Area",
    county_name=="Yukon Koyukuk Regional Educational Attendance Area" ~ "Yukon-Koyukuk Census Area",
    county_name=="Mashantucket Pequot Indian Reservation" ~ "New London",
    county_name=="Fort Belknap Indian Reservation" ~ "Blaine",
    county_name=="Fort Peck Indian Reservation" ~ "Roosevelt",
    county_name=="Spirit Lake Reservation" ~ "Benson",
    county_name=="Sauk-Suiattle Indian Reservation" ~ "Skagit",
    county_name=="Blackfeet Indian Reservation" ~ "Glacier",
    county_name=="Dona Ana" ~ "Doña Ana",
    county_name=="DeBaca" ~ "De Baca",
    county_name=="Bethel" ~ "Bethel Census Area",
    county_name=="Kodiak Island" ~ "Kodiak Island Borough",
    county_name=="Fairbanks North Star" ~ "Fairbanks North Star Borough",
    county_name=="Skagway" ~ "Skagway Municipality",
    county_name=="Ketchikan Gateway" ~ "Ketchikan Gateway Borough",
    county_name=="Juneau" & state=="AK" ~ "Juneau City and Borough",
    
    TRUE ~ county_name
    
  ))

# joining fema data with census data
county_analysis <- left_join(county_analysis, county_pov)
county_analysis <- county_analysis %>% 
  filter(!is.na(GEOID))

#exporting
write_csv(county_analysis, "outputs/summarized_data/county_analysis_geoids_updated.csv", na="0")
#write_csv(county_analysis, "outputs/summarized_data/county_analysis_geoids_updated_annual.csv", na="")
















