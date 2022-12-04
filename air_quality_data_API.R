########Environment API: RAQSAPI package
#install.packages(pkgs="RAQSAPI", dependencies = TRUE )
#install.packages("keyring")
library(RAQSAPI)
library(keyring)
library(dplyr)

aqs_sign_up("sherli@umich.edu")
keyring::has_keyring_support()
keyring::key_set(service = "AQSDatamart",username = "sherli@umich.edu")
#password:bluegazelle73

aqs_credentials(username = "sherli@umich.edu", key = "bluegazelle73" )

# use AQI POLLUTANTS
class<-aqs_classes()
AQI_pollutants<-aqs_parameters_by_class(class = "AQI POLLUTANTS")
stateFIPS<-aqs_states()
stateFIPS <- subset(stateFIPS,state!=c("Guam","Virgin Islands","Country Of Mexico","Canada" ))
stateFIPS <- subset(stateFIPS,state!=c("Guam"))




test<-aqs_dailysummary_by_state(parameter = c(AQI_pollutants$code),
                          bdate = as.Date("20040101",
                                          format="%Y%m%d"
                          ),
                          edate = as.Date("20040102",
                                          format = "%Y%m%d"
                          ),
                          stateFIPS = c(stateFIPS$stateFIPS)
)

