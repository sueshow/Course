library(shiny)
library(dplyr)
library(leaflet)
library(DT)
library(viridis)
library(plyr)
library(sqldf)
library(rgdal)
library(dplyr)
library(maps)
library(lubridate)


town.shp <- readOGR("data/TOWN_MOI_1090820.shp", use_iconv = TRUE, encoding = "UTF-8")
data <- readRDS(file = "data/clu1_km_op.rds")
colnames(data) = c("clu1_km_cluster","stno","year","month","day","坡地災害警戒值" ,"土石流災害警戒值","X1小時一級警戒",
                   "X1小時二級警戒" ,"X3小時一級警戒","X3小時二級警戒","X6小時一級警戒","X6小時二級警戒","X12小時一級警戒",
                   "X12小時二級警戒","X24小時一級警戒","X24小時二級警戒","pcpn_1h_max","pcpn_3h_max","pcpn_6h_max",
                   "pcpn_12h_max","pcpn_24h_max","L1_pcpn_1h","L2_pcpn_1h","L1_pcpn_3h","L2_pcpn_3h","L1_pcpn_6h",
                   "L2_pcpn_6h","L1_pcpn_12h","L2_pcpn_12h","L1_pcpn_24h","L2_pcpn_24h","landslip_warning","slope_warning",
                   "縣市","鄉鎮市區")

mapping <- readRDS(file = "data/mapping.rds")