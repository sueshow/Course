#################### 爬實價登錄資料 ####################
rm(list=ls())
## Crawl
#library(rvest)
#library(magrittr)
# Read data
library(stringr)
library(readxl)
#library(RPostgreSQL)
#library(dplyr)
#library(plyr)

# Environment
setwd("D:\\資料分析彙整\\Project\\TS_Crawl\\Raw Data")
#conn = dbConnect(dbDriver("PostgreSQL"), dbname="dwprod",
#                 host="XXX.XXX.XXX.XXX", port=XXXX,
#                 user="", password="")

# Parameter
work_path <- getwd()
name <- "109S3"

# Set zip path
#csv
#file_path <- file.path(work_path, paste0("lvr_langcsv_", name))
#xls
file_path <- file.path(work_path, paste0("lvr_langxls_", name))

# Download
res <- tryCatch({
  #csv
  #url <- paste("https://plvr.land.moi.gov.tw//DownloadSeason?season=", name, "&type=zip&fileName=lvr_landcsv.zip", sep="")
  #xls
  url <- paste("https://plvr.land.moi.gov.tw//DownloadSeason?season=", name, "&type=zip&fileName=lvr_landxls.zip", sep="")
  download.file(url, paste0(file_path, ".zip"))
},
#遇到 warning 時的自訂處理函數
warning = function(msg){
  message("Original warning message:")
  message(paste0(msg, "\n"))
  return(NULL)
},
#遇到 error 時的自訂處理函數
error = function(msg){
  message("Original error message:")
  message(paste0(msg, "\n"))
  return(NA)
}
)

# Download failure if true 
class(res)=="try-error"

# Unzip
if(res != "NA"){
  con <- unzip(paste0(file_path, ".zip"), exdir=file_path)  
}

# Read data for sell(新成屋、預售屋、土地)
file_list <- list.files(file_path, pattern='^[f|a].*_[a-z].csv')

# Column name of Table
##109S2以前
rented_extate <- c("township", "transaction_sign", "target_position", "land_area", "dtrct",
                   "non_metropolis_land_use_dtrct", "non_metropolis_land_use", "rented_dttm", "rented_nummber", "rented_level",
                   "tot_floor", "building_state", "main_use", "main_building_materials", "cmplt_dt", 
                   "building_shifting_tot_area", "room", "hall", "health", "compartmented",
                   "manage_org", "furniture", "tot_prc", "unit_prc", "berth_catg", 
                   "berth_tot_area", "berth_tot_prc", "note", "serial_number")

selled_land_extate <- c("township", "transaction_sign", "target_position", "land_area", "dtrct",
                   "non_metropolis_land_use_dtrct", "non_metropolis_land_use", "transcation_dttm", "transcation_nummber", "shifting_level",
                   "tot_floor", "building_state", "main_use", "main_building_materials", "cmplt_dt", 
                   "building_shifting_tot_area", "room", "hall", "health", "compartmented",
                   "manage_org", "tot_prc", "unit_prc", "berth_catg", "berth_tot_area",
                   "berth_tot_prc", "note", "serial_number")
selled_build_extate <- selled_land_extate

##109S3以後
#C
rented_extate <- c("township", "transaction_sign", "target_position", "land_area", "dtrct",
                   "non_metropolis_land_use_dtrct", "non_metropolis_land_use", "rented_dttm", "rented_nummber", "rented_level",
                   "tot_floor", "building_state", "main_use", "main_building_materials", "cmplt_dt", 
                   "building_shifting_tot_area", "room", "hall", "health", "compartmented",
                   "manage_org", "furniture", "tot_prc", "unit_prc", "berth_catg", 
                   "berth_tot_area", "berth_tot_prc", "note", "serial_number")
#B
selled_land_extate <- c("township", "transaction_sign", "target_position", "land_area", "dtrct",
                   "non_metropolis_land_use_dtrct", "non_metropolis_land_use", "transcation_dttm", "transcation_nummber", "shifting_level",
                   "tot_floor", "building_state", "main_use", "main_building_materials", "cmplt_dt", 
                   "building_shifting_tot_area", "room", "hall", "health", "compartmented",
                   "manage_org", "tot_prc", "unit_prc", "berth_catg", "berth_tot_area",
                   "berth_tot_prc", "note", "serial_number")
#A
selled_build_extate <- c("township", "transaction_sign", "target_position", "land_area", "dtrct",
                   "non_metropolis_land_use_dtrct", "non_metropolis_land_use", "transcation_dttm", "transcation_nummber", "shifting_level",
                   "tot_floor", "building_state", "main_use", "main_building_materials", "cmplt_dt", 
                   "building_shifting_tot_area", "room", "hall", "health", "compartmented",
                   "manage_org", "tot_prc", "unit_prc", "berth_catg", "berth_tot_area",
                   "berth_tot_prc", "note", "serial_number",
                   "building_area", "att_building_area", "balcony_area", "elevator")


# Read CSV
#colname <- selled_extate
#file <- str_remove(file_list[1], '.csv')
read_csv_file = function(file, colname){
  data <- readLines(file.path(file_path, paste0(file, ".csv")), encoding='UTF-8')
  data <- data[-2]
  final <- read.csv(textConnection(data), header=TRUE, stringsAsFactors=FALSE )
  names(final) <- colname
  # Give id by row
  id <- paste0(name, toupper(substr(file, 1, 1))
               ,toupper(substr(file, nchar(file), nchar(file)))
               ,str_pad(rep(1:nrow(final)), 6, pad="0"))
  final$datanm <- rep(file, nrow(final)) 
  year <- as.character(substr(name,1,3))
  session <- substr(name, nchar(name), nchar(name))
  final <- data.frame(id, year, session, final)
  return(final)
}

# Read XLS
read_xls_file = function(file, colname){
  if (toupper(substr(file, nchar(file), nchar(file)))==toupper('C')){
    data <- read_excel(file.path(filepath,paste0(file, ".xls")), sheet="不動產租賃")
  }else if (toupper(substr(file, nchar(file), nchar(file)))==toupper('B')){
    data <- read_excel(file.path(filepath,paste0(file, ".xls")), sheet="預售屋買賣")
  }else{
    data <- read_excel(file.path(filepath,paste0(file, ".xls")), sheet="不動產買賣")
  }
  data <- data[-1,]                                          ## Drop englinsh column
  #final = read.csv(textConnection(data), header = TRUE, stringsAsFactors = FALSE)
  final <- data
  names(final) <- colname                                   ## Give English name
  #myLetters <- letters[1:26]
  #id <- paste0(name, str_pad(match(substr(file, nchar(file), nchar(file)), myLetters),2, pad = "0"),str_pad(rep(1:nrow(final)), 6, pad = "0"))
  id <- paste0(name , toupper(substr(file, 1, 1))
               , toupper(substr(file, nchar(file), nchar(file)))
               , str_pad(rep(1:nrow(final)), 6, pad = "0")) ## Give id by row
  final$datanm <- rep(file, nrow(final))                    ## add column of source file
  year <- as.character(substr(name, 1, 3))
  session <- substr(name, nchar(name), nchar(name))
  final <- data.frame(id, year, session, final)
  return(final)
  
}


# 給定欄位型態
##如放在同一個Table這一段不需執行
final_rented <- data.frame()
final_selled_land <- data.frame()
final_selled_build <- data.frame()

#i <- 1
for(i in c(1:length(file_list))){
  
  #file <- str_remove(file_list[i], '.csv')
  file <- str_remove(file_list[i], '.xls')
  
  if(toupper(substr(file, nchar(file), nchar(file)))==toupper('C')){
    # For Rented Real Estate Category 'C'
    # Without the column of whether there is attaches the furniture
    final <- read_csv_file(file, rented_extate)
    final_rented <- rbind(final_rented, final)
  }else if(toupper(substr(file, nchar(file), nchar(file)))==toupper('B')){
    final <- read_csv_file(file, selled_land_extate)
    final_selled_land <- rbind(final_selled_land, final)    
  }else{
    final <- read_csv_file(file, selled_build_extate)
    final_selled_build <- rbind(final_selled_build, final)       
  }
}

# Insert into DB
#dbWriteTable(conn
#             , name=c("ttemp"."atul_rent_prc_of_real_estate")
#             , value=final_rented
#             , append=TRUE
#             , row.names=FALSE
#             , overwrite=FALSE
#             , field.type=c(
#                             id="bpchar(13)"
#                             , year="bpchar(13)"
#                             , session="integer"
#                             , township="varchar(6)"
#                             , transaction_sign="varchar(20)"
#                             , target_position="varchar(100)"
#                             , land_area="numeric(20,2)"
#                             , dtrct="bpchar(2)"
#                             , non_metropolis_land_use_dtrct="varchar(10)"
#                             , non_metropolis_land_use="varchar(20)"
#                             , rented_dttm="bpchar(7)" 
#                             , rented_nummber="varchar(20)"
#                             , rented_level="varchar(100)"
#                             , tot_floor="varchar(6)"
#                             , building_state="varchar(100)"
#                             , main_use="varchar(10)"
#                             , main_building_materials="varchar(20)"
#                             , cmplt_dt="bpchar(7)"
#                             , building_shifting_tot_area="numeric(20,2)"
#                             , room="integer"
#                             , hall="integer"
#                             , health="integer"
#                             , compartmented="bpchar(1)"
#                             , manage_org="bpchar(1)"
#                             , furniture="bpchar(1)"
#                             , tot_prc="numeric(20,2)"
#                             , unit_prc="numeric(20,2)"
#                             , berth_catg="varchar(6)"
#                             , berth_tot_area="numeric(20,2)"
#                             , berth_tot_prc="numeric(20,2)"
#                             , note="text"
#                             , serial_number="bpchar(20)"
#                             , datanm="bpchar(12)"
#
#                             , main_buid_area="numeric(20,2)"
#                             , subs_buid_area="numeric(20,2)"
#                             , balcony_area="numeric(20,2)"
#                             , elevator="bpchar(2)"
#                           )
#)

#dbWriteTable(conn
#             , name=c("ttemp"."atul_sell_prc_of_real_estate")
#             , value=final_selled
#             , append=TRUE
#             , row.names=FALSE
#             , overwrite=FALSE
#             , field.type=c(
#                             id="bpchar(13)"
#                             , year="bpchar(13)"
#                             , session="integer"
#                             , township="varchar(6)"
#                             , transaction_sign="varchar(20)"
#                             , target_position="varchar(100)"
#                             , land_area="numeric(20,2)"
#                             , dtrct="bpchar(2)"
#                             , non_metropolis_land_use_dtrct="varchar(10)"
#                             , non_metropolis_land_use="varchar(20)"
#                             , transcation_dttm="bpchar(7)" 
#                             , transcation_nummber="varchar(20)"
#                             , shifting_level="varchar(100)"
#                             , tot_floor="varchar(6)"
#                             , building_state="varchar(100)"
#                             , main_use="varchar(10)"
#                             , main_building_materials="varchar(20)"
#                             , cmplt_dt="bpchar(7)"
#                             , building_shifting_tot_area="numeric(20,2)"
#                             , room="integer"
#                             , hall="integer"
#                             , health="integer"
#                             , compartmented="bpchar(1)"
#                             , manage_org="bpchar(1)"
#                             , tot_prc="numeric(20,2)"
#                             , unit_prc="numeric(20,2)"
#                             , berth_catg="varchar(6)"
#                             , berth_tot_area="numeric(20,2)"
#                             , berth_tot_prc="numeric(20,2)"
#                             , note="text"
#                             , serial_number="bpchar(20)"
#                             , datanm="bpchar(12)"
#                           )
#)

#dbGetQuery(conn, "grant all on table ttemp.atul_rent_prc_of_real_estate to dw_dev_group")
#dbGetQuery(conn, "grant all on table ttemp.atul_sell_prc_of_real_estate to dw_dev_group")

#dbDisconnect(conn)


#==================================================================#
#################### 清理地址資料 #################### 
# 切號
#data <- final_selled_build
#col <- "id"
#i <- 3
cln_real_estate_dt <- function(data){
  # 編碼問題
  #for(col in colnames(data)){
  #  if(!is.character(data[[col]])) next
  #  Encoding(data[[col]]) <- "UTF-8"
  #}
  
  # Find interval(號段) to list
  tras <- lapply(data$target_position, 
                 function(x) unlist(regmatches(x, 
                                               gregexpr("[0-9~0-9]+號|[0-9]+號|[0-9~0-9]+地號|[0-9]+地號", x))))
  
  for(i in 1:length(tras)){
    if(length(tras[[i]])==0){
      cat(tras[[i]])
      tras[[i]] <- '號'
    }else{
      next
    }
  }
  
  number <- data.frame(number=matrix(unlist(tras), nrow=length(tras), byrow=T))
  data <- data.frame(data, number)
  
  
  # Extract number and combine with comma
  for(i in 1:nrow(data)){
    
    tmp <- unlist(regmatches(data$number[i], 
                             gregexpr("[[:digit:]]+\\.*[[:digit:]]*", data$number[i])))
    if(length(tmp)==0){
      cat(tmp)
      data$number2[i] <- ","
    }else{
      data$number2[i] <- paste0(tmp, collapse=',')
    }
    #data$number2[i] <- unlist(regmatches(data$number[i], gregexpr("[[:digit:]]+\\.*[[:digit:]]*", data$number[i])))
    
  }
  
  
  # Subset address without number
  for(i in 1:nrow(data)){
    data$addr_w_num[i] <- gsub("[0-9~0-9]+號|[0-9]+號|[0-9~0-9]+地號|[0-9]+地號", "", data$target_position[i])
  }

  
  # Split number interval list to upper and lower number
  data <- data.frame(data, str_split_fixed(data$number2, ",", 2))
  data$X1 <- as.numeric(as.character(data$X1))
  data$X2 <- as.numeric(as.character(data$X2))
  
  
  data$addr_w_num2 <- data$addr_w_num
  #data$addr_w_num2 <- gsub("臺", "台", data$addr_w_num)
  CN <- c("一", "二", "三", "四", "五", "六", "七", "八", "九", "十")
  for(i in 1:nrow(data)){

    if(length(grep("地號", data$target_position[i], value=FALSE))==0 
       && length(grep("段", data$addr_w_num2[i], value=FALSE))==1){
      
      loc <- regexpr("\\D{1}段", data$addr_w_num2[i])[1]
      num <- match(substr(data$addr_w_num2[i], loc, loc), CN)
      substr(data$addr_w_num2[i], loc, loc) <- as.character(num)
      
    }else{
      next
    }
  }
  
  return(data)
}

cln_rented <- cln_real_estate_dt(final_rented)
cln_selled_land <- cln_real_estate_dt(final_selled_land)
cln_selled_build <- cln_real_estate_dt(final_selled_build)
#dim(selled)

rm(final_rented, final_selled_land, final_selled_build)

# Insert into DB
#dbWriteTable(conn
#             , name=c("ttemp"."atul_rent_prc_of_real_estate_cln")
#             , value=cln_rented
#             , append=TRUE
#             , row.names=FALSE
#             , overwrite=FALSE
#             , field.type=c(
#                             id="bpchar(13)"
#                             , year="bpchar(13)"
#                             , session="integer"
#                             , township="varchar(6)"
#                             , transaction_sign="varchar(20)"
#                             , target_position="varchar(100)"
#                             , land_area="numeric(20,2)"
#                             , dtrct="bpchar(2)"
#                             , non_metropolis_land_use_dtrct="varchar(10)"
#                             , non_metropolis_land_use="varchar(20)"
#                             , rented_dttm="bpchar(7)" 
#                             , rented_nummber="varchar(20)"
#                             , rented_level="varchar(100)"
#                             , tot_floor="varchar(6)"
#                             , building_state="varchar(100)"
#                             , main_use="varchar(10)"
#                             , main_building_materials="varchar(20)"
#                             , cmplt_dt="bpchar(7)"
#                             , building_shifting_tot_area="numeric(20,2)"
#                             , room="integer"
#                             , hall="integer"
#                             , health="integer"
#                             , compartmented="bpchar(1)"
#                             , manage_org="bpchar(1)"
#                             , furniture="bpchar(1)"
#                             , tot_prc="numeric(20,2)"
#                             , unit_prc="numeric(20,2)"
#                             , berth_catg="varchar(6)"
#                             , berth_tot_area="numeric(20,2)"
#                             , berth_tot_prc="numeric(20,2)"
#                             , note="text"
#                             , serial_number="bpchar(20)"
#                             , datanm="bpchar(12)"
#                             , number="varchar(30)"
#                             , number2="varchar(30)"
#                             , addr_w_num="varchar(100)"
#                             , X1="integer"
#                             , X2="integer"
#                             , addr_w_num2="varchar(100)"
#                           )
#)

#dbWriteTable(conn
#             , name=c("ttemp"."atul_sell_prc_of_real_estate_cln")
#             , value=cln_selled
#             , append=TRUE
#             , row.names=FALSE
#             , overwrite=FALSE
#             , field.type=c(
#                             id="bpchar(13)"
#                             , year="bpchar(13)"
#                             , session="integer"
#                             , township="varchar(6)"
#                             , transaction_sign="varchar(20)"
#                             , target_position="varchar(100)"
#                             , land_area="numeric(20,2)"
#                             , dtrct="bpchar(2)"
#                             , non_metropolis_land_use_dtrct="varchar(10)"
#                             , non_metropolis_land_use="varchar(20)"
#                             , transcation_dttm="bpchar(7)" 
#                             , transcation_nummber="varchar(20)"
#                             , shifting_level="varchar(100)"
#                             , tot_floor="varchar(6)"
#                             , building_state="varchar(100)"
#                             , main_use="varchar(10)"
#                             , main_building_materials="varchar(20)"
#                             , cmplt_dt="bpchar(7)"
#                             , building_shifting_tot_area="numeric(20,2)"
#                             , room="integer"
#                             , hall="integer"
#                             , health="integer"
#                             , compartmented="bpchar(1)"
#                             , manage_org="bpchar(1)"
#                             , tot_prc="numeric(20,2)"
#                             , unit_prc="numeric(20,2)"
#                             , berth_catg="varchar(6)"
#                             , berth_tot_area="numeric(20,2)"
#                             , berth_tot_prc="numeric(20,2)"
#                             , note="text"
#                             , serial_number="bpchar(20)"
#                             , datanm="bpchar(12)"
#                             , number="varchar(30)"
#                             , number2="varchar(30)"
#                             , addr_w_num="varchar(100)"
#                             , X1="integer"
#                             , X2="integer"
#                             , addr_w_num2="varchar(100)"
#                           )
#)

#dbGetQuery(conn, "grant all on table ttemp.atul_rent_prc_of_real_estate_cln to dw_dev_group")
#dbGetQuery(conn, "grant all on table ttemp.atul_sell_prc_of_real_estate_cln to dw_dev_group")

# 砍連線
#dbDisconnect(conn)



#==================================================================#
