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


# town.shp <- readOGR("data/TOWN_MOI_1090820.shp", use_iconv = TRUE, encoding = "UTF-8")
# data <- readRDS(file = "data/clu1_km_op.rds")
# colnames(data) = c("clu1_km_cluster","stno","year","month","day","坡地災害警戒值" ,"土石流災害警戒值","X1小時一級警戒",
#                    "X1小時二級警戒" ,"X3小時一級警戒","X3小時二級警戒","X6小時一級警戒","X6小時二級警戒","X12小時一級警戒",
#                    "X12小時二級警戒","X24小時一級警戒","X24小時二級警戒","pcpn_1h_max","pcpn_3h_max","pcpn_6h_max",
#                    "pcpn_12h_max","pcpn_24h_max","L1_pcpn_1h","L2_pcpn_1h","L1_pcpn_3h","L2_pcpn_3h","L1_pcpn_6h",
#                    "L2_pcpn_6h","L1_pcpn_12h","L2_pcpn_12h","L1_pcpn_24h","L2_pcpn_24h","landslip_warning","slope_warning",
#                    "縣市","鄉鎮市區")
# 
# mapping <- readRDS(file = "data/mapping.rds")

shinyServer(function(input, output) {
  
  # town.shp <- readOGR("data/TOWN_MOI_1090820.shp", use_iconv = TRUE, encoding = "UTF-8")
  # data <- readRDS(file = "data/clu1_km_op.rds")
  # colnames(data) = c("clu1_km_cluster","stno","year","month","day","坡地災害警戒值" ,"土石流災害警戒值","X1小時一級警戒",
  #                    "X1小時二級警戒" ,"X3小時一級警戒","X3小時二級警戒","X6小時一級警戒","X6小時二級警戒","X12小時一級警戒",
  #                    "X12小時二級警戒","X24小時一級警戒","X24小時二級警戒","pcpn_1h_max","pcpn_3h_max","pcpn_6h_max",
  #                    "pcpn_12h_max","pcpn_24h_max","L1_pcpn_1h","L2_pcpn_1h","L1_pcpn_3h","L2_pcpn_3h","L1_pcpn_6h",
  #                    "L2_pcpn_6h","L1_pcpn_12h","L2_pcpn_12h","L1_pcpn_24h","L2_pcpn_24h","landslip_warning","slope_warning",
  #                    "縣市","鄉鎮市區")
  
  # mapping <- readRDS(file = "data/mapping.rds")
  
  # fnl = sqldf("select b.TOWNCODE,a.clu1_km_cluster,a.stno,a.year,a.month,a.day,a.縣市,a.鄉鎮市區
  #     from data a
  #     left join mapping b on a.縣市=b.COUNTYNAME and a.鄉鎮市區=b.TOWNNAME
  #     where a.year='2020' and a.month='7' and a.day='31'
  #     ")
  # 
  # fnl = sqldf(paste0("select b.TOWNCODE,a.clu1_km_cluster,a.stno,a.year,a.month,a.day,a.縣市,a.鄉鎮市區
  #     from data a
  #     left join mapping b on a.縣市=b.COUNTYNAME and a.鄉鎮市區=b.TOWNNAME
  #     where a.year=","'",year(as.Date("2020-07-31",format="%Y-%m-%d")),"'"," and a.month=","'"
  #                    ,month(as.Date("2020-07-31",format="%Y-%m-%d")),"'"," and a.day=","'"
  #                    ,day(as.Date("2020-07-31",format="%Y-%m-%d")),"'"))
  
  # fnl = sqldf(paste0("select b.TOWNCODE,a.clu1_km_cluster,a.stno,a.year,a.month,a.day,a.縣市,a.鄉鎮市區
  #     from data a
  #     left join mapping b on a.縣市=b.COUNTYNAME and a.鄉鎮市區=b.TOWNNAME
  #     where a.year=","'",year(input$date1),"'"," and a.month=","'"
  #                    ,month(input$date1),"'"," and a.day=","'"
  #                    ,day(input$date1),"'"))

  # fnl = sqldf(paste0("select b.TOWNCODE,a.clu1_km_cluster,a.stno,a.year,a.month,a.day,a.縣市,a.鄉鎮市區
  #     from data a
  #     left join mapping b on a.縣市=b.COUNTYNAME and a.鄉鎮市區=b.TOWNNAME
  #     where a.year=","'",year(as.Date(as.character(input$date1),format="%Y-%m-%d")),"'"," and a.month=","'"
  #                    ,month(as.Date(as.character(input$date1),format="%Y-%m-%d")),"'"," and a.day=","'"
  #                    ,day(as.Date(as.character(input$date1),format="%Y-%m-%d")),"'"))
  
  happy = data[which(year==year(as.Date(as.character(input$date1),format="%Y-%m-%d"))
                     & month==month(as.Date(as.character(input$date1),format="%Y-%m-%d"))
                     & day==day(as.Date(as.character(input$date1),format="%Y-%m-%d"))),]
  fnl = sqldf("select b.TOWNCODE,a.clu1_km_cluster,a.stno,a.year,a.month,a.day,a.縣市,a.鄉鎮市區
      from happy a
      left join mapping b on a.縣市=b.COUNTYNAME and a.鄉鎮市區=b.TOWNNAME
      ")

town.shp <- merge(
  town.shp, fnl[,1:2], 
  by.x="TOWNCODE", by.y="TOWNCODE", all.x=TRUE, sort = FALSE
)

incumbPalette <- colorFactor(palette = "RdBu", domain = town.shp@data$clu1_km_cluste)

town.shp@data$mnpopup=paste0('<strong> 所在縣市: </strong>',town.shp@data$COUNTYNAME,
                             '<br><strong>所在鄉鎮:</strong> ',town.shp@data$TOWNNAME,
                             #'<br><strong>日期:</strong> ',input$date1,
                             '<br><strong>水災風險值:</strong> ',town.shp@data$clu1_km_cluste
) 

output$bbmap <- renderLeaflet({
  mapStates = map("state", fill = TRUE, plot = FALSE)
  leaflet(town.shp)%>% addTiles()%>%
    addPolygons(stroke=TRUE, 
                smoothFactor = 0.2,
                weight = 1,
                fillOpacity = .6, 
                popup=town.shp@data$mnpopup,
                color= ~incumbPalette(town.shp@data$clu1_km_cluste)
    )%>% addLegend("bottomright", pal = incumbPalette, values = ~town.shp@data$clu1_km_cluste
                   , title = "Risk")%>%
                                  setView(lng = 120.982024, lat = 37.45, zoom = 7)
})


})

