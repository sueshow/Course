rm(list=ls())

# Download Library
#install.packages("leaflet")
library(leaflet)
#install.packages("dplyr")
library(dplyr)
#install.packages("sqldf")
library(sqldf)
#install.packages("viridis")
library(viridis)
#install.packages("lubridate")
library(lubridate)
#install.packages("maps")
library(maps)
#install.packages("plyr")
library(plyr) 
#install.packages("ggplot2")
library(ggplot2)
#install.packages("RColorBrewer")
library(RColorBrewer)
#install.packages("rgdal")
library(rgdal)
#install.packages("sf")
library(sf)
#install.packages("magrittr")
library(magrittr)
#install.packages("mapproj")
library(mapproj)
#install.packages("wesanderson")
library(wesanderson)



sessionInfo()


# Current Path
getwd()

path <- "D:\\資料分析彙整\\Project\\天然災害風險分析\\資料"
setwd(path)
getwd()



# Parameter 
input <- c("2018-08-23")
input <- c("2019-07-19")


#mapping <- readRDS(file="map/mapping.rds")

data <- readRDS(file="cluster_kmedoids_actual.rds")
#20201025
colnames(data) <- c("cluster_kmedoids_actual","cluster_kmedoids_ori","stno","year","month","day",
                    "坡地災害警戒值" ,"土石流災害警戒值",
                    "X1小時一級警戒","X1小時二級警戒" ,"X3小時一級警戒","X3小時二級警戒","X6小時一級警戒","X6小時二級警戒",
                    "X12小時一級警戒","X12小時二級警戒","X24小時一級警戒","X24小時二級警戒",
                    "reach_pcpn24h_hours","reach_slope_hours",
                    "pcpn_1h_max","pcpn_3h_max","pcpn_6h_max","pcpn_12h_max","pcpn_24h_max",
                    "L1_pcpn_1h","L2_pcpn_1h","L1_pcpn_3h","L2_pcpn_3h","L1_pcpn_6h","L2_pcpn_6h",
                    "L1_pcpn_12h","L2_pcpn_12h","L1_pcpn_24h","L2_pcpn_24h",
                    "landslip_warning","slope_warning",
                    "縣市","鄉鎮市區",
                    "avg_rain_hours","reach_12_pcpn1h_rate","reach_12_pcpn24h_rate","pcpn_24h_max_one_day")

#20201105
colnames(data) <- c("cluster_kmedoids_actual","cluster_kmedoids_ori","stno","year","month","day",
                    "坡地災害警戒值" ,"土石流災害警戒值",
                    "X1小時一級警戒","X1小時二級警戒" ,"X3小時一級警戒","X3小時二級警戒","X6小時一級警戒","X6小時二級警戒",
                    "X12小時一級警戒","X12小時二級警戒","X24小時一級警戒","X24小時二級警戒",
                    "raining_hours","reach_pcpn1h_hours","reach_pcpn24h_hours","reach_slope_hours","reach_landslip_hours",
                    "pcpn_1h_max","pcpn_3h_max","pcpn_6h_max","pcpn_12h_max","pcpn_24h_max",
                    "pcpn1h_0.5","pcpn24h_0.5","slope_0.5","landslip_0.5",
                    "L1_pcpn_1h","L2_pcpn_1h","L1_pcpn_3h","L2_pcpn_3h","L1_pcpn_6h","L2_pcpn_6h",
                    "L1_pcpn_12h","L2_pcpn_12h","L1_pcpn_24h","L2_pcpn_24h",
                    "landslip_warning","slope_warning",
                    "站名","縣市","鄉鎮市區",
                    "avg_rain_hours","reach_12_pcpn1h_rate","reach_12_pcpn24h_rate","pcpn_24h_max_one_day")


#data$cluster_kmedoids_actual <- as.factor(data$cluster_kmedoids_actual)

happy <- data[which(data$year==year(as.Date(as.character(input),format="%Y-%m-%d"))
                    & data$month==month(as.Date(as.character(input),format="%Y-%m-%d"))
                    & data$day==day(as.Date(as.character(input),format="%Y-%m-%d"))),]


twn_shp <- readOGR(dsn=paste0(path,"\\map"), layer="TOWN_MOI_1091016", use_iconv=TRUE, encoding="UTF-8")
#twn_shp <- readOGR(dsn=paste0(path,"/map/TOWN_MOI_1091016.shp"), use_iconv=TRUE, encoding="UTF-8")

#【法一】
tt <- data.frame(twn_shp@data)
mapping <- unique(tt[,c(2:4)])


fnl <- sqldf(" select b.TOWNCODE, a.cluster_kmedoids_actual, a.stno, a.year, a.month, a.day, a.縣市, a.鄉鎮市區
               from happy a
               left join mapping b on a.縣市=b.COUNTYNAME and a.鄉鎮市區=b.TOWNNAME
            ")

#twn_shp@data <- sp::merge( x=twn_shp@data, y=fnl, 
#                           by.x=c("COUNTYNAME","TOWNNAME"), by.y=c("縣市","鄉鎮市區"), 
#                           all.x=TRUE, sort=FALSE )


#twn_shp@data <- sp::merge( x=twn_shp@data, y=fnl[,1:2], 
#                           by.x="TOWNCODE", by.y="TOWNCODE", 
#                           all.x=TRUE, sort=FALSE
#)

twn_shp <- merge( x=twn_shp, y=fnl[,1:2], 
                  by.x="TOWNCODE", by.y="TOWNCODE", 
                  all.x=TRUE, sort=FALSE
)


incumbPalette <- colorFactor(palette=viridis(10), twn_shp@data$cluster_kmedoids_actual, reverse=TRUE, levels=c(as.character(1:10)))
#incumbPalette <- colorFactor(palette="RdBu", domain=twn_shp@data$cluster_kmedoids_actual)

twn_shp@data$mnpopup=paste0( '<strong> 所在縣市: </strong>',twn_shp@data$COUNTYNAME,
                             '<br><strong>所在鄉鎮:</strong> ',twn_shp@data$TOWNNAME,
                             #'<br><strong>日期:</strong> ',input$date1,
                             '<br><strong>水災風險值:</strong> ',twn_shp@data$cluster_kmedoids_actual ) 


#mapStates <- map("state", fill=TRUE, plot=FALSE)
twn_shp %>% leaflet() %>% 
                  addTiles()%>%
                  addPolygons( stroke=TRUE, 
                               smoothFactor=0.2,
                               weight=1,
                               fillOpacity=0.6, 
                               popup=twn_shp@data$mnpopup,
                               color= ~incumbPalette(twn_shp@data$cluster_kmedoids_actual) ) %>%  
                  addLegend("bottomright", pal=incumbPalette, values= ~twn_shp@data$cluster_kmedoids_actual, title="Risk") %>%
                  setView(lng=120.982024, lat=24, zoom = 7)


#========================================
#cluster_kmedoids_year_mean <- readRDS(file='cluster_kmedoids_actual_year_mean.rds', refhook=NULL)
#cluster_kmedoids_year_mode <- readRDS(file='cluster_kmedoids_actual_year_mode.rds', refhook=NULL)
cluster_kmedoids_year_all <- readRDS(file='cluster_kmedoids_actual_year_all.rds', refhook=NULL)

twn_shp <- readOGR(dsn=paste0(path,"\\map"), layer="TOWN_MOI_1091016", use_iconv=TRUE, encoding="UTF-8")

tt <- data.frame(twn_shp@data)
mapping <- unique(tt[,c(2:4)])


fnl <- sqldf(" select b.TOWNCODE, a.fin_avg, a.Mode, a.ratio, a.year, a.縣市, a.鄉鎮市區
               from cluster_kmedoids_year_all a
               --left join cluster_kmedoids_year_mode v on a.縣市=v.縣市 and a.鄉鎮市區=v.鄉鎮市區 and v.year is null 
               left join mapping b on a.縣市=b.COUNTYNAME and a.鄉鎮市區=b.TOWNNAME
               where a.year is null
            ")

twn_shp <- merge( x=twn_shp, y=fnl[,1:4], 
                  by.x="TOWNCODE", by.y="TOWNCODE", 
                  all.x=TRUE, sort=FALSE
)

#pal <- wes_palette("Zissou1", 10, type = "continuous")
#incumbPalette <- colorFactor(pal, twn_shp@data$fin_avg, reverse = TRUE, levels=c(as.character(1:10)))
incumbPalette <- colorNumeric(c("#F21A00", "#D1C74C", "#3B9AB2"), twn_shp@data$fin_avg, reverse = TRUE)
#incumbPalette <- colorFactor(palette=viridis(5), twn_shp@data$fin_avg, reverse=TRUE, levels=c(as.character(1:10)))
#incumbPalette <- colorFactor(palette="RdBu", domain=twn_shp@data$cluster_kmedoids_actual)


twn_shp@data$mnpopup=paste0( '<strong> 所在縣市: </strong>', twn_shp@data$COUNTYNAME,
                             '<br><strong>所在鄉鎮:</strong> ', twn_shp@data$TOWNNAME,
                             #'<br><strong>日期:</strong> ',input$date1,
                             '<br><strong>水災風險值:</strong> ', twn_shp@data$fin_avg,
                             '<br><strong>Mode:</strong> ', twn_shp@data$Mode,
                             '<br><strong>Mode發生的頻率:</strong> ', twn_shp@data$ratio
                             )


twn_shp %>% leaflet() %>% 
  addTiles()%>%
  addPolygons( stroke=TRUE, 
               smoothFactor=0.2,
               weight=1,
               fillOpacity=0.6, 
               popup=twn_shp@data$mnpopup,
               color= ~incumbPalette(twn_shp@data$fin_avg) ) %>%  
  addLegend("bottomright", pal=incumbPalette, values= ~twn_shp@data$fin_avg, title="Risk") %>%
  setView(lng=120.982024, lat=24, zoom = 7)



##############################################################################################
#【法二】利用SHP檔中的CODE，新版的SHP檔
#library(rgdal)
library(maptools)
if (!require(gpclib)) install.packages("gpclib", type="source")
gpclibPermit()

twn_shp <- readOGR(dsn=paste0(path,"\\map"), layer="TOWN_MOI_1091016", use_iconv=TRUE, encoding="UTF-8")
twn_shp@proj4string <- CRS("+proj=longlat +ellps=GRS80 +towgs84=0,0,0,0,0,0,0 +no_defs")


townlist <- twn_shp@data[, c("TOWNCODE", "COUNTYNAME", "TOWNNAME")]


town.point <- fortify(twn_shp, region="TOWNCODE")
town.point$id <- as.integer(town.point$id)


df1 <- merge(x = merge(town.point, townlist, by.x = "id", by.y = "TOWNCODE"), 
             y = happy, by.x = c("COUNTYNAME", "TOWNNAME"), by.y = c("縣市","鄉鎮市區"), 
             duplicateGeoms = TRUE, all.x = T)

df1 <- df1[order(df1$order),]



pal <- wes_palette("Zissou1", 100, type = "continuous")


#cluster_kmedoids_actual 不可以轉格式
ggplot() +
  geom_polygon(data = df1, 
               aes(x = long, y = lat, group = group, fill = cluster_kmedoids_actual), 
               color = "black", size = 0.5) + 
  labs(title = "", fill = "") +
  scale_fill_gradientn( colours = pal, na.value = "transparent", 
                        breaks = seq(0,10,2), labels = seq(0,10,2), limits=c(0,10) ) +
  coord_map(xlim=c(118,124), ylim=c(21.6,26.3))+
  #theme_void() +  #去背
  theme(text = element_text(size=24))


#display.brewer.pal(n = 10, name = 'RdBu')
#col_list <- brewer.pal(10,"Reds") 
#col_list <- viridis(10)
#col_list <- c("#2c798c", "#f05988", "#fba26d", "#64bfc0", "#fae2ca")
#col_list <- c("#ee3c00", "#e5cb2f", "#459eb5")
col_list <- brewer.pal(10,"RdBu") 


#cluster_kmedoids_actual 要轉格式
df1$cluster_kmedoids_actual <- as.factor(df1$cluster_kmedoids_actual)

ggplot() +
  geom_polygon(data = df1, 
               aes(x = long, y = lat, group = group, fill = cluster_kmedoids_actual), 
               color = "black", size = 0.25) + 
  labs(title = "", fill = "") +
  ##scale_color_manual(values = col_list) +
  scale_fill_manual(values = c("1"=col_list[1],
                               "2"=col_list[2],
                               "3"=col_list[3],
                               "4"=col_list[4],
                               "5"=col_list[5],
                               "6"=col_list[6],
                               "7"=col_list[7],
                               "8"=col_list[8],
                               "9"=col_list[9],
                               "10"=col_list[10])) +
  coord_map(xlim=c(118,124), ylim=c(21.6,26.3))+
  theme_void()



#【法三】同法一，自訂ID
#library(rgdal)
library(maptools)
if (!require(gpclib)) install.packages("gpclib", type="source")
gpclibPermit()

twn_shp <- readOGR(dsn=paste0(path,"\\map"), layer="TOWN_MOI_1091016", use_iconv=TRUE, encoding="UTF-8")
twn_shp@proj4string <- CRS("+proj=longlat +ellps=GRS80 +towgs84=0,0,0,0,0,0,0 +no_defs")


twn_shp@data$id <- rownames(twn_shp@data)
shapefile_df <- fortify(twn_shp, region="id")


mapping02 <- data.frame(twn_shp@data)

fnl02 <- sqldf(" select b.TOWNCODE
                        , b.id
                        , a.cluster_kmedoids_actual
                        , a.year
                        , a.month
                        , a.day
                        , a.縣市
                        , a.鄉鎮市區
               from happy a
               left join mapping02 b on a.縣市=b.COUNTYNAME and a.鄉鎮市區=b.TOWNNAME
              ")

final.plot <- merge(x=shapefile_df, y=fnl02[,c("id","cluster_kmedoids_actual")], by.x="id", by.y="id", all.x=T)

final.plot <- final.plot[order(final.plot$order),]


#cluster_kmedoids_actual 要轉格式
df1$cluster_kmedoids_actual <- as.factor(df1$cluster_kmedoids_actual)

ggplot() +
  geom_polygon( data=final.plot, 
                aes(x=long, y=lat, group=group, 
                    fill=cluster_kmedoids_actual), 
                color="black", size=0.1) + 
  coord_map(xlim=c(118,124), ylim=c(21.6,26.3))+
  theme_void()




#【法四】同法二，利用SHP檔中的CODE，舊版的SHP檔
#library(rgdal)
library(maptools)
if (!require(gpclib)) install.packages("gpclib", type="source")
gpclibPermit()


#town_shp <- readOGR(dsn=paste0(path,"\\map"), layer="TOWN_MOI_1091016", use_iconv=TRUE, encoding="UTF-8")
#town_shp <- readOGR(".\\map\\Town_MOI_1041215.shp", encoding = "UTF-8")
twn_shp <- readOGR(dsn=paste0(path,"\\map\\Town"), layer="Town_MOI_1041215", use_iconv=TRUE, encoding="UTF-8")
twn_shp@proj4string <- CRS("+proj=longlat +ellps=GRS80 +towgs84=0,0,0,0,0,0,0 +no_defs")


townlist <- twn_shp@data[, c("Town_ID", "T_Name", "County_ID", "C_Name")]


town.point <- fortify(twn_shp, region="Town_ID")
town.point$id <- as.integer(town.point$id)


#【頻率分析】
freq_df <- read.csv(".\\頻率分析_20201105.csv")
freq_df <- read.csv(".\\hazard_index.csv")

df1 <- merge(x = merge(town.point, townlist, by.x = "id", by.y = "Town_ID"), 
             y = freq_df, by.x = c("C_Name", "T_Name"), by.y = c("縣市","鄉鎮市區"), 
             duplicateGeoms = TRUE, all.x = T)

df1 <- df1[order(df1$order),]

pal <- wes_palette("Zissou1", 100, type = "continuous")

ggplot() +
  geom_polygon(data = df1, 
               aes(x = long, y = lat, group = group, fill = weighted_mean), 
               color = "black", size = 0.1) + 
  coord_fixed(ratio = 1)



#【風險指標】
df1 <- merge(x = merge(town.point, townlist, by.x = "id", by.y = "Town_ID"), 
             y = happy, by.x = c("C_Name", "T_Name"), by.y = c("縣市","鄉鎮市區"), 
             duplicateGeoms = TRUE, all.x = T)

df1 <- df1[order(df1$order),]

pal <- wes_palette("Zissou1", 100, type = "continuous")

#png(filename="./Risk_20200823_m.png")
#win.metafile("./Risk_20200823_m.wmf")

#cluster_kmedoids_actual 不可以轉格式
ggplot() +
  geom_polygon(data = df1, 
               aes(x = long, y = lat, group = group, fill = cluster_kmedoids_actual), 
               color = "black", size = 0.5) + 
  labs(title = "", fill = "") +
  scale_fill_gradientn( colours = pal, na.value = "transparent", 
                        breaks = seq(0,10,2), labels = seq(0,10,2), limits=c(0,10) ) +
  coord_map(xlim=c(118,124), ylim=c(21.6,26.3))+
  #theme_void() +
  theme(text = element_text(size=24))

#dev.off()

#cluster_kmedoids_actual 要轉格式
df1$cluster_kmedoids_actual <- as.factor(df1$cluster_kmedoids_actual)

ggsave("./Picture_20190719_20201105_D20201202.png", height = 10, width = 14, dpi = 300)



#【總風險】
cluster_kmedoids_year_all <- readRDS(file='cluster_kmedoids_actual_year_all.rds', refhook=NULL)

fnl <- sqldf(" select b.TOWNCODE, a.fin_actual, a.Mode, a.ratio, a.year, a.縣市, a.鄉鎮市區
               from cluster_kmedoids_year_all a
               --left join cluster_kmedoids_year_mode v on a.縣市=v.縣市 and a.鄉鎮市區=v.鄉鎮市區 and v.year is null 
               left join mapping b on a.縣市=b.COUNTYNAME and a.鄉鎮市區=b.TOWNNAME
               where a.year is null
            ")

df1 <- merge(x = merge(town.point, townlist, by.x = "id", by.y = "Town_ID"), 
             y = fnl, by.x = c("C_Name", "T_Name"), by.y = c("縣市","鄉鎮市區"), 
             duplicateGeoms = TRUE, all.x = T)

df1 <- df1[order(df1$order),]

pal <- wes_palette("Zissou1", 100, type = "continuous")

#png(filename="./Risk_20200823_m.png")
#win.metafile("./Risk_20200823_m.wmf")

#cluster_kmedoids_actual 不可以轉格式
ggplot() +
  geom_polygon(data = df1, 
               aes(x = long, y = lat, group = group, fill = fin_actual), 
               color = "black", size = 0.5) + 
  labs(title = "", fill = "") +
  scale_fill_gradientn( colours = pal, na.value = "transparent", 
                        breaks = seq(0,10,2), labels = seq(0,10,2), limits=c(0,10) ) +
  coord_map(xlim=c(118,124), ylim=c(21.6,26.3))+
  #theme_void() +
  theme(text = element_text(size=24))
