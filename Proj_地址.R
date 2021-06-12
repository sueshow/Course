#################### 地址 ####################
rm(list=ls())
#install.packages("RPostgreSQL")
#install.packages("jiebaR")
## Crawl
#library(rvest)
#library(magrittr)
# Read data
library(readxl)
library(stringr)
library(stringi)
library(sqldf)
library(RPostgreSQL)
library(dplyr)
#library(plyr)
library(jiebaR)


# Environment
setwd("D:\\資料分析彙整\\Project\\TS_Crawl\\Data")
#conn = dbConnect(dbDriver("PostgreSQL"), dbname="dwprod",
#                 host="172.16.7.33", port=5432,
#                 user="", password="")

# Parameter
path <- getwd()
file_name <- '社區大樓.xlsx'
#file_name <- list.files(path, pattern='*.xlsx')

# Read Original Data
data_set <- read_excel( paste0(path, "/", file_name), sheet=1 )
data_set$addr <- NA

for(i in 1:nrow(data_set)){
  if(substr(data_set$路段[i], 3, 3) %in% c("市","縣")){
    data_set$addr[i] <- data_set$路段[i]
    
  }else if(substr(data_set$路段[i], 3, 3) %in% c("區", "鄉", "鎮")){
    data_set$addr[i] <- with( data_set[i,], paste0(縣市, 路段) )
    
  }else{
    data_set$addr[i] <- with( data_set[i,], paste0(縣市, 行政區, 路段) )
  }
}

names(data_set) <- c( "bldg_id", "bldg_nm", "county", "town", "neighborhood", "road", "addr" )

# 轉編碼
for(col in colnames(data_set)){
  if(!is.character(data_set[[col]])) next
  Encoding(data_set[[col]]) <- "UTF-8"
}



data_manual <- data_set
# 手動處理
#轉換台
data_manual$addr <- gsub( "臺", "台", data_manual$addr)

#轉換段
CN <- c("一", "二", "三", "四", "五", "六", "七", "八", "九", "十")

for(i in 1:nrow(data_manual)){
  
  if(length(grep("段", data_manual$addr[i], value=FALSE))==1){
    if(regexpr("\\D{1}段", data_manual$addr[i])[1]!=-1){
      loc <- regexpr("\\D{1}段", data_manual$addr[i])[1]
      num <- match(substr(data_manual$addr[i], loc, loc), CN)
      substr(data_manual$addr[i], loc, loc) <- as.character(num)
    }else{
      next
    }
  }else{
    next
  }
  
  if(length(grep("巷", data_manual$addr[i], value=FALSE))==1){
    if(regexpr("\\D{1}巷", data_manual$addr[i])[1]!=-1){
      loc <- regexpr("\\D{1}巷", data_manual$addr[i])[1]
      num <- match(substr(data_manual$addr[i], loc, loc), CN)
      substr(data_manual$addr[i], loc, loc) <- as.character(num)
    }else{
      next
    }
  }else{
    next
  }
}

  
# 分詞
file_path <- list.files( path, pattern="raddr_road.txt" )
seg <- worker( bylines=F, symbol=T, user=file_path)
word <- lapply( data_manual$addr, function(x) paste(segment(x, seg), collapse=" ") )

#i <- 17
data_manual[, "word"] <- NA
for(i in 1:nrow(data_manual)){
  data_manual$word[i] <- paste( segment(data_manual$addr[i], seg), collapse=" ")    
}

#路段
data_manual[, "road"] <- NA
data_manual[, "road_oth"] <- NA
data_manual[, "other"] <- NA
for(i in 1:length(data_manual$word)){

  road <- ""
  tmp <- ""
  road_oth <- ""
  other <- ""
  #路
  road <- gsub( " .*$", "", data_manual$word[i] )
  #段
  tmp <- unlist( regmatches(data_manual$word[i], gregexpr("[[:digit:]]+\\s\\.*[段]", data_manual$word[i] )))
  #其他
  road_oth <- gsub( " ", "", sub(".*? ", "", word[i]), fixed=TRUE )
  #額外地址
  other <- unlist( regmatches(road_oth, gregexpr("\uff1b\\.*", road_oth )))
  
  if(length(tmp)==0){
    data_manual$road[i] <- road
    
    if(length(other)==0){
      data_manual$road_oth[i] <- road_oth
      data_manual$other[i] <- ""
    }else{
      data_manual$road_oth[i] <- unlist( strsplit(road_oth, other) )[1]
      data_manual$other[i] <- unlist( strsplit(road_oth, other) )[2]
    }

  }else{
    section <- ""
    section <- gsub( pattern = "\\s", replacement = "", x=tmp )
    #路段
    data_manual$road[i] <- gsub( pattern = "\\s", replacement = "", x=paste( road, section, collapse="" )) 
    
    if(length(other)==0){
      data_manual$road_oth[i] <- unlist( strsplit(road_oth, section) )[2]
      data_manual$other[i] <- ""
    }else{
      data_manual$road_oth[i] <- unlist( strsplit(road_oth, other) )[1]
      data_manual$other[i] <- unlist( strsplit(road_oth, other) )[2]
    }
  }
  
}


#其他
data_manual[, "alley"] <- NA
data_manual[, "road_alley"] <- NA
data_manual[, "number"] <- NA
data_manual[, "num_int"] <- NA
for(i in 1:nrow(data_manual)){
  
  tmp <- unlist( regmatches(data_manual$road_oth[i], gregexpr("[[:digit:]]+\\.*[巷|弄]", data_manual$road_oth[i] )))
  
  if(length(tmp)==0){
    data_manual$alley[i] <- NA
    data_manual$road_alley[i] <- data_manual$road[i]
  }else{
    data_manual$alley[i] <- paste(tmp, collapse="")
    data_manual$road_alley[i] <- paste0(data_manual$road[i], data_manual$alley[i])
  }
  
#}
#號
#for(i in 1:nrow(data_manual)){
  
  if(nchar(data_manual$road[i])==3 | is.na(data_manual$alley[i])){
    if(nchar(data_manual$road[i])==3){
      data_manual$number[i] <- NA
    }else{
      data_manual$number[i] <- data_manual$road_oth[i]
      #data_manual$road_oth[i] <- NA
    }
  }else{
    data_manual$number[i] <- gsub( data_manual$alley[i], "", data_manual$road_oth[i] )
    #data_manual$road_oth[i] <- NA
  }
  
#}
#號
#for(i in 1:nrow(data_manual)){
  data_manual$num_int[i] <- str_extract( data_manual$number[i], "(\\d)*.+(?=號)" )
}


#去除「之X」，「之X~X」
data_manual[, "num_int2"] <- NA
for(i in 1:nrow(data_manual)){
  tmp01 <- unlist( regmatches(data_manual$num_int[i], gregexpr("\\.*[之][[:digit:]]+", data_manual$num_int[i] )))
  tmp02 <- unlist( regmatches(data_manual$num_int[i], gregexpr("\\.*[之][[:digit:]]+[~][[:digit:]]+", data_manual$num_int[i] )))
  
  if(length(tmp01)!=0 && length(tmp02)==0){
    num_int <- data_manual$num_int[i]
    for(j in 1:length(tmp01)){
      num <- sub(tmp01[j], "", num_int)
      num_int <- num
    }
    data_manual$num_int2[i] <- paste( unlist(regmatches(num_int, gregexpr("[[:digit:]]+\\.*[[:digit:]]*", num_int ))), collapse="," )
  
  }else if(length(tmp02)!=0){
    num_int <- data_manual$num_int[i]
    for(j in 1:length(tmp02)){
      num <- sub(tmp02[j], "", num_int)
      num_int <- num
    }
    data_manual$num_int2[i] <- paste( unlist(regmatches(num_int, gregexpr("[[:digit:]]+\\.*[[:digit:]]*", num_int ))), collapse="," )
  }else{
    data_manual$num_int2[i] <- paste( unlist(regmatches(data_manual$num_int[i], gregexpr("[[:digit:]]+\\.*[[:digit:]]*", data_manual$num_int[i] ))), collapse="," )
  }

}


data_manual[, "sign"] <- NA
for(i in 1:nrow(data_manual)){
  data_manual$sign[i] <- str_extract( data_manual$number[i], "單|雙|獨|全" )
}


#i <- 21
#待確認
data_manual[, "num_array"] <- NA
for(i in 1:nrow(data_manual)){
  
  sn <- data_manual$sign[i]
  tmp <- unlist( strsplit(data_manual$num_int2[i], ",") )
  tmp_dup <- tmp[!duplicated(tmp)]
  #tmp_con <- ""
  #for(i in 1:length(tmp_dup)){
  #  if(i==1){
  #    tmp_con <- paste0(tmp_dup[i])
  #  }else{
  #    tmp_con <- paste0(tmp_con, ",", tmp_dup[i])
  #  }
  #}
   
  mn <- min( as.numeric(tmp_dup) )
  mx <- max( as.numeric(tmp_dup) )
  
  if(is.na(sn)){
    if(length(tmp_dup)==0){
      data_manual$num_array[i] <- ""
    }else if(length(tmp_dup)==1 ){
      data_manual$num_array[i] <- paste0( "[", tmp_dup, "]" )
    }else if(length(tmp_dup)==2){
      if(as.numeric(tmp_dup[2]) <= as.numeric(tmp_dup[1])){
        data_manual$num_array[i] <- paste0( "[", tmp_dup[1], "]" )
      }else{
        data_manual$num_array[i] <- paste0( "[", paste(seq(mn, mx, by=1), collapse=","), "]" )
      }
    }else{
      data_manual$num_array[i] <- ifelse(data_manual$num_int2[i]=="", NA, paste0( "[", data_manual$num_int2[i], "]") )
    }
  }else{
    if(length(tmp_dup)==0){
      data_manual$num_array[i] <- ""
    }else if(length(tmp_dup)==1){
      data_manual$num_array[i] <- paste0( "[", tmp_dup, "]" )
    }else if(length(tmp_dup)==2){

      sn <- data_manual$sign[i]
      data_manual$num_array[i] <- ifelse( sn=="單", paste0( "[", paste0(seq(mn, mx, by=2), collapse=","), "]" ),
                                          ifelse( sn=="雙", paste0( "[", paste0(seq(mn, mx, by=2), collapse=","), "]" ),
                                                  ifelse( sn=="全", paste0( "[", paste0(seq(mn, mx, by=1), collapse=","), "]") 
                                                                  , paste0("[", tmp, "]"))) )
    }else{
      data_manual$num_array[i] <- ifelse(data_manual$num_int2[i]==0, NA, paste0("[", data_manual$num_int2[i], "]") )
    }
  }
}


colnames(data_manual)
data_manual %>%

  select( bldg_id, bldg_nm, county, town, neighborhood, road_alley, number, road_oth, other, num_array ) -> final



#==================================================================#
#################### 正規劃後地址 (NOT)####################
#file_name <- "to_tgos.txt"
file_name <- list.files( path, pattern="from_tgos.txt" )
file_path <- file.path( getwd(), file_name )
#write.table( data_set[, c(1:7)], paste0(path, "/", file_name), sep=","
#             , col.names=F, row.name=F
#             , fileEncoding="Big5", quote=F )

raddr <- readlines( file_path, encoding="UTF-8" )
raddr_dt <- as.data.frame( str_split(raddr, ",", simplify=TRUE) )

names(raddr_dt) <- c( "id"
                      , "addr_m"
                      , "addr_raddr"
                      , "county"
                      , "town"
                      , "village"
                      , "neighborhood"
                      , "road"
                      , "section"
                      , "lane"
                      , "alley"
                      , "sub_alley"
                      , "tong"
                      , "number"
                      , "x"
                      , "y")

raddr_dt$id <- as.character(raddr_dt$id)
raddr_dt2 <- sqldf(" select a.bldg_id
                            , a.county as m_country
                            , a.town as m_town
                            , a.bldg_addr
                            , b.addr_m
                            , b.addr_raddr
                            , b.county
                            , b.town
                            , b.village
                            , b.neighborhood
                            , b.road
                            , b.section
                            , b.lane
                            , b.alley
                            , b.sub_alley
                            , b.tong
                            , b.number
                            , b.x
                            , b.y
                     from data_set a
                     left join raddr_dt b on a.bldg_id=b.id", drv="SQLite")

raddr_dt2$raddr_road <- with( raddr_dt2, paste0(road, ifelse(is.null(section), 
                                                             paste0(section, "段"), ""), lane, alley, sub_alley, tong, number))

raddr_dt2$raddr_full_wo_village <- with( raddr_dt2, paste0(county, town, road
                                                           , ifelse(is.null(section),"", paste0(section, "段"))
                                                           , lane, alley, number))


raddr_dt2$raddr_full_w_village <- with( raddr_dt2, paste0(county, town, road, village
                                                          , ifelse(is.null(section),"", paste0(section, "段"))
                                                          , lane, alley, number))

raddr_dt2$raddr_full_wo_village_A_number <- with( raddr_dt2, paste0(county, town, road
                                                           , ifelse(is.null(section),"", paste0(section, "段"))
                                                           , lane, alley))


raddr_dt2$raddr_full_w_village_wo_number <- with( raddr_dt2, paste0(county, town, road, village
                                                          , ifelse(is.null(section),"", paste0(section, "段"))
                                                          , lane, alley))

# 切號
for(i in 1:nrow(raddr_dt)){
  raddr_dt2$x1[i] <- stri_extract_first_regex( raddr_dt2$number[i], "[0-9+]" )
  raddr_dt2$x2[i] <- stri_extract_first_regex( raddr_dt2$number[i], "[0-9+]" )
}

# 換字
raddr_dt2$raddr_full <- gsub( "臺", "台", raddr_dt2$raddr_full_wo_village)
raddr_dt2$raddr_full_wo_village_A_number <- gsub( "臺", "台", raddr_dt2$raddr_full_wo_village_A_number)

write.table( unique(raddr_dt2$raddr_full_wo_village_A_number), file.path(getwd(), "tw_road_raddr.txt")
             , quote=FALSE, row.names=FALSE, col.names=FALSE
             , fileEncoding="UTF-8" )

# 轉編碼
for(col in colnames(data)){
  Encoding(data[[col]]) <- "UTF-8"
}

## 參考資料
# 全國路名資料：https://data.gov.tw/dataset/35321