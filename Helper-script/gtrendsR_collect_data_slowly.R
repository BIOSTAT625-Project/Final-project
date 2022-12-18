install.packages("gtrendsR")
library(gtrendsR)
library(tidyverse)

# key words
key_words<-c("feeling sad", "depressed","depression","empty","insomnia","fatigue","guilty","suicide")

#### test the API
res_TEST1 <- gtrends(c("depression"),
                     geo = c("US-AK","US"),
                     category = 437, #Mental Health
                     time = "2012-1-1 2013-1-1",
                     gprop = "web")$interest_over_time

Locations<-c("US-AK","US-HI","US-WA","US-MT","US-ND","US-MN","US-WI","US-MI","US-ME","US-VT",
             "US-NH","US-NY","US-MA","US-RI","US-ID","US-WY","US-SD","US-IA","US-IL","US-IN",
             "US-OH","US-PA","US-NJ","US-CT","US-OR","US-NV","US-CO","US-NE","US-MO","US-KY",
             "US-WV","US-VA","US-MD","US-DE","US-CA","US-UT","US-NM","US-KS","US-AR","US-TN",
             "US-NC","US-SC","US-AZ","US-OK","US-LA","US-MS","US-AL","US-GA","US-TX","US-FL","US-DC") # 51 states

### download raw data

download_google_all<-function(key,year){
  Locations<-c("US-DC"
               # "US-AK","US-HI","US-WA","US-MT","US-ND","US-MN","US-WI","US-MI","US-ME","US-VT",
               #  "US-NH","US-NY","US-MA","US-RI","US-ID","US-WY","US-SD","US-IA","US-IL","US-IN",
               #"US-OH","US-PA","US-NJ","US-CT","US-OR","US-NV","US-CO","US-NE","US-MO","US-KY",
               # "US-WV","US-VA","US-MD","US-DE","US-CA","US-UT","US-NM","US-KS","US-AR","US-TN",
               #  "US-NC","US-SC","US-AZ","US-OK","US-LA","US-MS","US-AL","US-GA","US-TX","US-FL"
  )
  n = length(Locations)
  State_data<-data.frame(matrix(ncol = 7, nrow = 0))
  for (i in 1:n) {
    tryCatch({
      print(Locations[i])
      State_search<-gtrends(key,geo = c(Locations[i],"US"),
                            category = 437, #Mental Health
                            time = year,
                            gprop = "web")$interest_over_time
      State_data<-rbind(State_data,State_search)
      Sys.sleep(runif(1, min=100, max=300))
    },
    error=function(e){})
  }
  return(State_data)
}

# for different key words, find the appropriate range of time to download the data
## example: 
## this period didn't work
depressed_12_16<-download_google_all("depressed","2012-1-1 2017-1-1")
## this one worked
depressed_12_16<-download_google_all("depressed","2012-1-1 2017-3-1")