rm(list=ls())

#install.packages('dplyr')
library(dplyr)
#install.packages('sqldf')
library(sqldf)
#install.packages("lubridate")
library(lubridate)


# Current Path
getwd()

path <- 'D:\\資料分析彙整\\Project\\天然災害風險分析\\資料'
setwd(path)
getwd()


# Load Data
rain_data_v1 <- read.csv('rainfall_everyday_20201029.csv', header=TRUE)
rain_data_v2 <- read.csv('rainfall_everyday_20201105.csv', header=TRUE)

rain_data_v2 <- rain_data_v2[ , !names(rain_data_v2) %in% c("reach_pcpn24h_hours", "reach_slope_hours", 
                                                            "reach_landslip_hours", "pcpn1h_0.5",
                                                            "pcpn24h_0.5", "slope_0.5", "landslip_0.5","站名")]

colnames(rain_data_v1)
colnames(rain_data_v2)  


# Parameter 
input <- c("2018-08-23")

rain_data_v1 <- rain_data_v1[which(rain_data_v1$year==year(as.Date(as.character(input),format="%Y-%m-%d"))
                                   & rain_data_v1$month==month(as.Date(as.character(input),format="%Y-%m-%d"))
                                   & rain_data_v1$day==day(as.Date(as.character(input),format="%Y-%m-%d"))),]

rain_data_v2 <- rain_data_v2[which(rain_data_v2$year==year(as.Date(as.character(input),format="%Y-%m-%d"))
                                   & rain_data_v2$month==month(as.Date(as.character(input),format="%Y-%m-%d"))
                                   & rain_data_v2$day==day(as.Date(as.character(input),format="%Y-%m-%d"))),]



# 相同的結果
rain_data_v1 %>%
  bind_rows(rain_data_v2) %>%
  group_by(stno,year,month,day,縣市,鄉鎮市區) %>%
  filter(n() > 1) %>%
  summarise_all(~ ifelse(all(is.na(.)), NA, min(., na.rm = TRUE))) -> final


# 有差異的結果
temp1 <- rbind(rain_data_v1, rain_data_v2)
temp2 <- temp1[!duplicated(temp1),]
temp3 <- temp1[duplicated(temp1),]


#a <- merge(x=temp2, y=temp3, by=c("stno","year","month","day","縣市","鄉鎮市區"), all.x=TRUE, all.Y=FALSE)
a <- sqldf(" select a.*
             from temp2 a
             left join temp3 b
               on a.year=b.year
              and a.month=b.month
              and a.day=b.day
              and a.縣市=b.縣市
              and a.鄉鎮市區=b.鄉鎮市區
             where b.縣市 is null
")


write.csv(rain_data_v1, "rain_data_v1.csv")
write.csv(rain_data_v2, "rain_data_v2.csv")
write.csv(a, "a.csv")


