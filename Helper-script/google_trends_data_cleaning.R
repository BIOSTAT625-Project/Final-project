## read excels data (".xlsx")
#install.packages("readxl")
library(readxl)
library(dplyr)

## Function 1
## read a pair of excel files
read_pair_of_raw_files <- function(file1, file2) {
  folder_path = "C:/GoogleTrendData/"

  file1_fullpath = paste(folder_path, file1, sep="")
  file2_fullpath = paste(folder_path, file2, sep="")

  df1 = openxlsx::read.xlsx(
    file1_fullpath,
    sheet = "Timeseries"    )

  df2 = openxlsx::read.xlsx(
    file2_fullpath,
    sheet = "Timeseries")

  return(list(df1, df2))
}

read_one_raw_files <- function(file1) {
  folder_path = "C:/GoogleTrendData/"
  file1_fullpath = paste(folder_path, file1, sep="")
  df1 = openxlsx::read.xlsx(
    file1_fullpath,
    sheet = "Timeseries"    )
  return(df1)
}

## Function 2
## data cleaning: remove the text in the file can rename the columns
clean_pair<-function(df){
  df.1<- as.data.frame(do.call(cbind, df))

  df.2 <- df.1[!is.na(as.numeric(df.1[,2])), ]

  names(df.2)[1]<-"Week"
  names(df.2)[2]<-"Relative"
  names(df.2)[3]<-"Absolute"

  df.2$Week <- as.Date(df.2$Week)
  df.2$Relative <- as.numeric(df.2$Relative)
  df.2$Absolute <- as.numeric(df.2$Absolute)

  return(df.2)
}

## Function 3
## Find the common part of two files and return the correlation
transfor_rate<-function(df1,df2){
  paired<-merge(x= df1, y= df2, by= 'Week')
  paired<-paired[complete.cases(paired), ]

  paired$Relative.x <- as.numeric(paired$Relative.x)
  paired$Relative.y <- as.numeric(paired$Relative.y)

  paired$Absolute.x <- as.numeric(paired$Absolute.x)
  paired$Absolute.y <- as.numeric(paired$Absolute.y)

  r_standard<-summary(lm(as.numeric(Relative.x) ~ as.numeric(Absolute.x), paired))$coefficients[ , 1][2]
  r_trans <-summary(lm(as.numeric(Relative.x) ~ as.numeric(Relative.y), paired))$coefficients[ , 1][2]

 return(c(r_standard,r_trans))
}

## Function 4
## transform the data_2 and calculate the absolute values
get_absolute<-function(df2, rate){
  df2$Relative.trans <- df2$Relative * rate[2]
  df2$Absolute.new <- df2$Relative.trans / rate[1]

  return(df2)
}


### Final function
## calculate the absolute values in data_2 based on data_1
## merge two dataset into a bigger one
## for overlap, take the average
Transform<-function(name1,name2){
  pair<-read_pair_of_raw_files(name1,name2)
  data_1<-clean_pair(pair[1])
  data_2<-clean_pair(pair[2])
  rate<-transfor_rate(data_1,data_2)
  data_2_new<-get_absolute(data_2,rate)

  data_2_new$Relative<-data_2_new$Relative.trans
  data_2_new$Absolute<-data_2_new$Absolute.new
  data_2_new<-select(data_2_new, -4:-5)

  return(data_2_new)
}

Transform_1<-function(data,name2){
  data_1<-data
  data_2<-clean_pair(read_one_raw_files(name2))
  rate<-transfor_rate(data_1,data_2)
  data_2_new<-get_absolute(data_2,rate)

  data_2_new$Relative<-data_2_new$Relative.trans
  data_2_new$Absolute<-data_2_new$Absolute.new
  data_2_new<-select(data_2_new, -4:-5)

  return(data_2_new)
}


###### Depressed (US)
Depressed_US_17_22<-clean_pair(read_one_raw_files("Depressed (US) 每 Google Trends Export 每 Glimpse.xlsx"))

Depressed_US_16_20<-Transform("Depressed (US) 每 Google Trends Export 每 Glimpse.xlsx",
                      "Depressed (US) 每 Google Trends Export 每 Glimpse5.xlsx")

Depressed_US_12_16<-Transform_1(Depressed_US_16_20,"Depressed (US) 每 Google Trends Export 每 Glimpse4.xlsx")

Depressed_US_08_12<-Transform_1(Depressed_US_12_16,"Depressed (US) 每 Google Trends Export 每 Glimpse3.xlsx")

Depressed_US_04_08<-Transform_1(Depressed_US_08_12,"Depressed (US) 每 Google Trends Export 每 Glimpse2.xlsx")

