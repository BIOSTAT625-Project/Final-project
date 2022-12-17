library(tsibble)
library(fable)
library(dplyr)
library(lubridate)

trends = read.csv("~/Dropbox (University of Michigan)/Final project/BIO625_final_project_Rawdata/Clean_data/google_trends_all.csv")
#trends$week = as.Date(trends$week)
pred<- trends %>%
  na.omit() %>%
  as_tsibble(key = c(keyword, region), index = X) %>%
  model(arima = ARIMA(absolute)) %>% 
  forecast(h = 52)



d <-  as.Date("2022-11-27")
fut.days <- seq(d+1,d + 365,by='day')
fut.Sun <- fut.days[weekdays(fut.days)=='Sunday']

pred <- pred %>%
  bind_cols(fut.Sun = rep(fut.Sun, 52*8))

write.csv(pred, "~/Dropbox (University of Michigan)/Final project/BIO625_final_project_Rawdata/Clean_data/pred52_withDate.csv")

# pred <- pred %>%
#   select(X, .fitted)
# 
# trends <- trends %>%
#   left_join(pred, by='X')
# 
# trends <- trends[, 1:6]
# 
# write.csv(trends, "~/Dropbox (University of Michigan)/Final project/BIO625_final_project_Rawdata/Clean_data/trends.csv")
p = read.csv("~/Dropbox (University of Michigan)/Final project/BIO625_final_project_Rawdata/Clean_data/pred52_withDate.csv")
View(p)

UStrends = trends[trends$region == "US", ]

USpred <-  UStrends %>%
  as_tsibble(key = c(keyword, region), index = X) %>%
  model(arima = ARIMA(absolute)) %>% 
  forecast(h = 52)

USpred <- USpred %>%
  bind_cols(fut.Sun = rep(fut.Sun, 8))
write.csv(USpred, "~/Dropbox (University of Michigan)/Final project/BIO625_final_project_Rawdata/Clean_data/USpred52_withDate.csv")
