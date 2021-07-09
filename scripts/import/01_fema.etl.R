library(tidyverse) 
library(lubridate)
library(jsonlite)
library(httr)
library(purrr)
library(reshape2)
library(doParallel)

#### import IHP data in parallel ---

# or skip to looping one by one farther down

registerDoParallel() # registered the parallel backend?

#### How many cores do you want to use? ----

registerDoParallel(cores=16)


baseUrl <- "https://www.fema.gov/api/open/v1/IndividualsAndHouseholdsProgramValidRegistrations?"


# Determine record count. Specifying only 1 column here to reduce amount of data returned. 
date_start <- "2010-01-01"

result <- GET(paste0(baseUrl,"$inlinecount=allpages&$filter=declarationDate%20gt%20%27", date_start, "%27&$select=id"))

jsonData <- content(result)   
recCount <- jsonData$metadata$count


# calculate the number of calls we will need to get all of our data (using the maximum of 1000)
top <- 1000
loopNum <- ceiling(recCount / top)

# send some logging info to the console so we know what is happening
print(paste0("START ",Sys.time(),", ", recCount, " records, ", top, " returned per call, ", loopNum," iterations needed."),quote=FALSE)


# Loop and call the API endpoint changing the record start each iteration. Each call will
# return results in a JSON format. The metadata has been suppressed as we no longer need it.
skip <- 0


results <- foreach(i=0:loopNum, .combine=rbind) %dopar% {
  # As above, if you have filters, specific fields, or are sorting, add that to the base URL 
  #   or make sure it gets concatenated here.
  GET(paste0(baseUrl,"$inlinecount=allpages&$filter=declarationDate%20gt%20%272015-01-01%27&$metadata=off&$top=",top,"&$skip=",i * top)) %>%
  content() %>% 
    hoist(IndividualsAndHouseholdsProgramValidRegistrations,
                         incidentType = "incidentType", 
                         declarationDate = "declarationDate",
                         disasterNumber = "disasterNumber",
                         county = "county", 
                         state = "damagedStateAbbreviation",
                         city = "damagedCity", 
                         zip = "damagedZipCode", 
                         applicantAge = "applicantAge", 
                         householdComposition = "householdComposition",
                         occupantsUnderTwo = "occupantsUnderTwo",
                         occupants2to5 = "occupants2to5", 
                         occupants6to18 = "occupants6to18", 
                         occupants19to64 = "occupants19to64", 
                         occupants65andOver = "occupants65andOver",
                         grossIncome = "grossIncome", 
                         ownRent = "ownRent", 
                         primaryResidence = "primaryResidence", 
                         residenceType = "residenceType", 
                         homeOwnersInsurance = "homeOwnersInsurance",
                         floodInsurance = "floodInsurance",
                         registrationMethod = "registrationMethod",
                         ihpReferral = "ihpReferral",
                         ihpEligible = "ihpEligible",
                         ihpAmount = "ihpAmount",
                         fipAmount = "fipAmount",
                         haReferral = "haReferral",
                         haEligible = "haEligible",
                         haAmount = "haAmount",
                         haStatus = "haStatus",
                         onaReferral = "onaReferral",
                         onaEligible = "onaEligible",
                         onaAmount = "onaAmount",
                         utilitiesOut = "utilitiesOut",
                         homeDamage = "homeDamage",
                         autoDamage = "autoDamage",
                         emergencyNeeds = "emergencyNeeds",
                         foodNeed = "foodNeed",
                         shelterNeed = "shelterNeed",
                         accessFunctionalNeeds = "accessFunctionalNeeds",
                         sbaEligible = "sbaEligible",
                         sbaApproved = "sbaApproved",
                         inspnIssued = "inspnIssued",
                         inspnReturned = "inspnReturned",
                         habitabilityRepairsRequired = "habitabilityRepairsRequired",
                         rpfvl = "rpfvl",
                         ppfvl = "ppfvl",
                         renterDamageLevel = "renterDamageLevel",
                         destroyed = "destroyed",
                         waterLevel = "waterLevel",
                         highWaterLocation = "highWaterLocation",
                         floodDamage = "floodDamage",
                         floodDamageAmount = "floodDamageAmount",
                         foundationDamage = "foundationDamage",
                         foundationDamageAmount = "foundationDamageAmount",
                         roofDamage = "roofDamage",
                         roofDamageAmount = "roofDamageAmount",
                         tsaEligible = "tsaEligible",
                         tsaCheckedIn = "tsaCheckedIn",
                         rentalAssistanceEligible = "rentalAssistanceEligible",
                         rentalAssistanceAmount = "rentalAssistanceAmount",
                         repairAssistanceEligible = "repairAssistanceEligible",
                         repairAmount = "repairAmount",
                         replacementAssistanceEligible = "replacementAssistanceEligible",
                         replacementAmount = "replacementAmount",
                         personalPropertyEligible = "personalPropertyEligible",
                         personalPropertyAmount = "personalPropertyAmount",
                         ihpMax = "ihpMax",
                         haMax = "haMax",
                         onaMax = "onaMax",
                         lastRefresh = "lastRefresh",
                         id = "id") %>%
    as.data.frame()
  
}
 
                         
  

#---- Save out ----
write_rds(results, "data/clean_data/20102021data.rds")
#write_csv(results, "data/clean_data/20152021data.csv")


### if errors froma bove, use non parallel---
### loop one by one

i <- 0
what_year <- 2010

for (i in 0:loopNum) {
#while (what_year != 2015) {
  
results <-
  GET(paste0(baseUrl,"$inlinecount=allpages&$filter=declarationDate%20gt%20%27", start_date, "%27&$metadata=off&$top=",top,"&$skip=",i * top)) %>%
    content() %>% 
    hoist(IndividualsAndHouseholdsProgramValidRegistrations,
          incidentType = "incidentType", 
          declarationDate = "declarationDate",
          disasterNumber = "disasterNumber",
          county = "county", 
          state = "damagedStateAbbreviation",
          city = "damagedCity", 
          zip = "damagedZipCode", 
          applicantAge = "applicantAge", 
          householdComposition = "householdComposition",
          occupantsUnderTwo = "occupantsUnderTwo",
          occupants2to5 = "occupants2to5", 
          occupants6to18 = "occupants6to18", 
          occupants19to64 = "occupants19to64", 
          occupants65andOver = "occupants65andOver",
          grossIncome = "grossIncome", 
          ownRent = "ownRent", 
          primaryResidence = "primaryResidence", 
          residenceType = "residenceType", 
          homeOwnersInsurance = "homeOwnersInsurance",
          floodInsurance = "floodInsurance",
          registrationMethod = "registrationMethod",
          ihpReferral = "ihpReferral",
          ihpEligible = "ihpEligible",
          ihpAmount = "ihpAmount",
          fipAmount = "fipAmount",
          haReferral = "haReferral",
          haEligible = "haEligible",
          haAmount = "haAmount",
          haStatus = "haStatus",
          onaReferral = "onaReferral",
          onaEligible = "onaEligible",
          onaAmount = "onaAmount",
          utilitiesOut = "utilitiesOut",
          homeDamage = "homeDamage",
          autoDamage = "autoDamage",
          emergencyNeeds = "emergencyNeeds",
          foodNeed = "foodNeed",
          shelterNeed = "shelterNeed",
          accessFunctionalNeeds = "accessFunctionalNeeds",
          sbaEligible = "sbaEligible",
          sbaApproved = "sbaApproved",
          inspnIssued = "inspnIssued",
          inspnReturned = "inspnReturned",
          habitabilityRepairsRequired = "habitabilityRepairsRequired",
          rpfvl = "rpfvl",
          ppfvl = "ppfvl",
          renterDamageLevel = "renterDamageLevel",
          destroyed = "destroyed",
          waterLevel = "waterLevel",
          highWaterLocation = "highWaterLocation",
          floodDamage = "floodDamage",
          floodDamageAmount = "floodDamageAmount",
          foundationDamage = "foundationDamage",
          foundationDamageAmount = "foundationDamageAmount",
          roofDamage = "roofDamage",
          roofDamageAmount = "roofDamageAmount",
          tsaEligible = "tsaEligible",
          tsaCheckedIn = "tsaCheckedIn",
          rentalAssistanceEligible = "rentalAssistanceEligible",
          rentalAssistanceAmount = "rentalAssistanceAmount",
          repairAssistanceEligible = "repairAssistanceEligible",
          repairAmount = "repairAmount",
          replacementAssistanceEligible = "replacementAssistanceEligible",
          replacementAmount = "replacementAmount",
          personalPropertyEligible = "personalPropertyEligible",
          personalPropertyAmount = "personalPropertyAmount",
          ihpMax = "ihpMax",
          haMax = "haMax",
          onaMax = "onaMax",
          lastRefresh = "lastRefresh",
          id = "id") %>%
    as.data.frame()
    
    what_year <- year(ymd_hms(results$declarationDate[1]))
    
    if (i==0) {
      df <- results
    } else {
      df <- rbind(df, results)
    }
    
  print(paste0(i, " of ", loopNum))
  #i <- i +1
  
  }


#---- Save out ----
write_rds(df, "data/clean_data/20102021data.rds")
