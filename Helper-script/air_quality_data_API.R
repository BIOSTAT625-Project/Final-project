########Environment API: RAQSAPI package
install.packages(pkgs="RAQSAPI", dependencies = TRUE )
install.packages("keyring")
install.packages("remote")

remotes::install_github(repo = "USEPA/raqsapi",
                        dependencies = TRUE,
                        upgrade = "always",
                        build = TRUE,
                        #optional, set TRUE if the manual is desired,
                        #requires pandoc
                        build_manual = FALSE,
                        build_vignettes = TRUE,
                        force = TRUE
)

library(RAQSAPI)
library(keyring)
library(dplyr)

aqs_sign_up("sherli@umich.edu")
keyring::has_keyring_support()

kb <- keyring::backend_file$new()
system<-kb$keyring_create()
kb$keyring_list()


keyring::key_set(service = "AQSDatamart",username = "sherli@umich.edu")
datamartAPI_user <- "sherli@umich.edu"
server <- "AQSDatamart"
RAQSAPI::aqs_credentials(username = "sherli@umich.edu", key = key_get(service = server,
                                                                      username = datamartAPI_user) )


# get AQI values
class<-aqs_classes()
AQI_pollutants<-aqs_parameters_by_class(class = "AQI POLLUTANTS")
stateFIPS<-aqs_states()
stateFIPS <- subset(stateFIPS,state!=c("Guam","Virgin Islands","Country Of Mexico","Canada" ))
stateFIPS <- subset(stateFIPS,state!=c("Guam"))
stateFIPS$stateFIPS

# get the AQI at state level
mean_aqs<-function(state){
  aqs<-aqs_dailysummary_by_state(parameter = c(AQI_pollutants$code),
                                 bdate = as.Date("20040101",
                                                 format="%Y%m%d"),
                                 edate = as.Date("20221115",
                                                 format = "%Y%m%d"),
                                 stateFIPS = state)
  aqs2<-aqs %>%
    group_by(date_local,state_code) %>%
    dplyr::summarize(Mean_aqi = mean(aqi, na.rm=TRUE))

  return(aqs2)
}

#get AQI for every state and merge them together
get_aqi<-function(states){
  n<-length(states)
  empty<-data.frame(matrix(ncol = 3, nrow = 0))
  for (i in 1:n){
    aqi<-mean_aqs(states[i])
    empty<-rbind(empty,aqi)
  }
  return(empty)
}

aqi_complete<-get_aqi(stateFIPS$stateFIPS)

aqi_complete$stateFIPS<-aqi_complete$state_code
aqi_complete <- merge(aqi_complete,stateFIPS,by="stateFIPS")
