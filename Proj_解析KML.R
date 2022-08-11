rm(list=ls())
install.packages('sf')
install.packages('leaflet')
install.packages('sqldf')
install.packages('RPostgreSQL')
install.packages('sp')

library(sf)
library(leaflet)
library(sqldf)
library(RPostgreSQL)
library(sp)

conn = dbConnect(dbDriver("PostgreSQL"), dbname = "dwprod",
                 host = "XXX.XX.X.XX", port = 5432,
                 user = "XXXXX", password = "XXXX")

#基站
data <- readLines("D:/XXXXX/ALL.csv", encoding='UTF-8')
final = read.csv(textConnection(gsub('(\\b\\d{1,3}),(?=\\d{3}(\\D|$))','\\1',data,perl=TRUE)), header = TRUE, stringsAsFactors = FALSE, colClasses="character")

#轉數字
final$wgs84_long=as.double(final$wgs84_long)
final$wgs84_lat=as.double(final$wgs84_lat)

#匯入KML
#確認檔案數
first_file_name <- list.files("D:/michellechuang/Desktop/import_kml", "\\.kml{1,}")    
length(first_file_name) 
i <- 1
file_name <- first_file_name[i]
dir_file <- paste("D:/michellechuang/Desktop/import_kml/", file_name, sep = "")            #用paste命令構建路徑變數dir
#匯入KML
mitake <- st_read(dir_file)
#mitake <- st_read("D:/XXXXX/三竹特篩_LBS(20210426).kml")

#轉地圖格式
mitake2 <- st_zm(mitake)
mitake3 = as(mitake2, 'Spatial')

#驗證用地圖
leaflet(mitake3) %>%
  addProviderTiles("CartoDB.Positron") %>%
  addPolygons(color = "green")

#確認基站點(單區)
bound <- 0.001  #100公尺
xmin <- st_bbox(mitake)$xmin
xmax <- st_bbox(mitake)$xmax
ymin <- st_bbox(mitake)$ymin
ymax <- st_bbox(mitake)$ymax
final_test <- final[which((final[,9] >= xmin  - bound & final[,9] <= xmax + bound ) &
                            (final[,10] >= ymin  - bound & final[,10]<= ymax  + bound )),]
final_test

#確認基站點(多區)
bound <- 0.001  #100公尺
xmin <- 120.3701
xmax <- 120.3967
ymin <- 23.92794
ymax <- 23.94900
final_test <- final[which((final[,9] >= xmin  - bound & final[,9] <= xmax + bound ) &
                            (final[,10] >= ymin  - bound & final[,10]<= ymax  + bound )),]
final_test

leaflet(mitake3) %>%
  addProviderTiles("CartoDB.Positron") %>%
  addPolygons(color = "green") %>% 
  addTiles() %>%
  addMarkers(data=final_test, lat=~wgs84_lat, lng=~wgs84_long)


#轉成 SpatialPointsDataFrame
xy <- final[,c(9,10)]
pt_final <- SpatialPointsDataFrame(coords = xy, data = final,
                                   proj4string = CRS("+proj=longlat +datum=WGS84 +no_defs"))


#### 合併 ####

res <- over(pt_final, mitake3)
res2 <- over(mitake3, pt_final,returnList = TRUE)

mapping = data.frame(ID = character(), caseId=character(),caseName=character(),longitude=double(),latitude=double())
for(i in 1:length(res2)){
  if(nrow(res2[[i]])==0){
    next
  }else{
    mapping = rbind(mapping,data.frame(ID=names(res2)[i],res2[[i]]))
  }
}

map = data.frame(ID=1:length(mitake3@data$Name),Name=mitake3@data$Name)

#mapping 記得改時間
mappingfnl = sqldf("select a.ID,b.Name,a.site_id,a.ntwk_type,a.base_nm,a.site_stat,a.cobuild_cde,a.city,a.dtrct,a.regn_nm,a.wgs84_long,a.wgs84_lat,'20210426' as create_dt
                    from mapping a
                    left join map b on a.ID=b.ID",drv="SQLite")

#寫入DB
dbWriteTable(conn
             , name=c("ttemp","site_dist_mapping")
             , value=mappingfnl[,c(2:4,6:10,13)]
             , append=TRUE
             , row.names=FALSE
             , overwrite=FALSE
             # , field.types = c(
             #   id="bpchar(13)"
             #   ,year="bpchar(13)"
             #   ,session="integer"
             #   ,township="varchar(6)"
             #   ,transaction_sign="varchar(20)"
             #   ,target_position="varchar(100)"
             #   ,land_area="numeric(17,2)"
             #   ,dtrct="bpchar(2)"
             #   ,non_metropolis_land_use_dtrct="varchar(10)"
             #   ,non_metropolis_land_use="varchar(20)"
             #   ,transaction_dttm="bpchar(7)"
             #   ,transaction_number="varchar(20)"
             #   ,shifting_level="varchar(100)"
             #   ,tot_floor="varchar(6)"
             #   ,building_state="varchar(100)"
             #   ,main_use="varchar(10)"
             #   ,main_building_materials="varchar(20)"
             #   ,cmplt_dt="bpchar(7)"
             #   ,building_shifting_tot_area="numeric(17,2)"
             #   ,room="integer"
             #   ,hall="integer"
             #   ,health="integer"
             #   ,compartmented="bpchar(1)"
             #   ,manage_org="bpchar(1)"
             #   ,tot_prc="numeric(20,2)"
             #   ,unit_prc="numeric(20,2)"
             #   ,berth_catg="varchar(6)"
             #   ,berth_tot_area="numeric(17,2)"
             #   ,berth_tot_prc="numeric(20,2)"
             #   ,note="text"
             #   ,serial_number="bpchar(20)"
             #   ,datanm="bpchar(12)"
             #   ,main_buid_area="numeric(20,2)"
             #   ,subs_buid_area="numeric(20,2)"
             #   ,balcony_area="numeric(20,2)"
             #   ,elevator="bpchar(2)"
             #   ,number="varchar(30)"
             #   ,number2="varchar(30)"
             #   ,addr_without_number="varchar(100)"
             #   ,X1="integer"
             #   ,X2="integer"
             #   ,addr_without_number2="varchar(100)"
             # )                
)



dbDisconnect(conn)
