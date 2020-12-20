#################### 視覺化(bar) ####################
rm(list=ls())
library(data.table)
library(dplyr)
library(ggplot2)
library(ggrepel)
windowsFonts(A=windowsFont("微軟正黑體"))
options(scipen=999)


# Environment
setwd("D:\\資料分析彙整\\Project\\TS_Crawl\\Raw Data")

# Parameter
work_path <- getwd()
name <- "109S3"

# Set zip path
file_path <- file.path( work_path, paste0("lvr_langcsv_", name) )
data_list <- list.files( file_path, pattern="^[f|a].*_[a].csv" )


#
all_csv_list <- lapply( data_list, function(x) fread(paste0(file_path, "/", x), encoding="UTF-8") )
all_csv_list <- lapply( all_csv_list, function(x) x[-1,] )
base_data <- do.call( rbind.data.frame, all_csv_list )
dim(base_data)

#非租屋
#色號：ec008c
#條件：
##交易標的：「房地(土地+建物)」,「房地(土地+建物)+車位」,「建物」
base_data <- subset( base_data, 交易標的 %in% c("房地(土地+建物)","房地(土地+建物)+車位","建物") )
dim(base_data)

##排除：交易筆棟數為「建物0」
#base_data <- base_data[ -grep("建物0", base_data$交易筆棟數), ]
#dim(base_data)

#排除：單價元平方公尺為0
base_data <- base_data[ -which(base_data$單價元平方公尺 %in% c("","0")), ]
dim(base_data)

#單價：平方公尺 →> 坪
##3.305785 : 1
base_data$單價元平方公尺 <- as.numeric( base_data$單價元平方公尺 )
base_data$單價元坪 <- base_data$單價元平方公尺*3.305785

g_data <- base_data %>% 
               group_by(鄉鎮市區) %>% 
               summarise(筆數=n(), 平均單價=mean(單價元坪))

Taipei <- c("中正區","大同區","中山區","松山區","大安區",
            "萬華區","信義區","士林區","北投區","內湖區",
            "南港區","文山區")

g_data$鄉鎮市區 <- factor( g_data$鄉鎮市區, levels=g_data$鄉鎮市區[order(g_data$平均單價)] )
class(g_data$平均單價)

Taipei_data <- subset( g_data, 鄉鎮市區 %in% Taipei )
#Taipei_data_frame <-as.data.frame( Taipei_data )

Taipei_plot <- 
  Taipei_data %>%
  ggplot( aes(x=鄉鎮市區, y=平均單價) ) +
  geom_bar( stat="identity", aes(fill=平均單價) ) +
  geom_text( aes(label=paste0(round(平均單價/10000, 2), "萬")),
             hjust=1, col="white", family="A" ) +
  scale_fill_gradient( low="#D999BF", high="#ec008c", na.value=NA ) +
  labs( title="「台北市」各區平均單價(坪)",
       subtitle="類別：「實際成交」") +
  coord_flip() +
  theme(
      axis.text.x = element_text(face="bold", size=12, angle=360, color="#ffffff"), 
      axis.text.y = element_text(face="bold", size=10, color="#ffffff"),
      axis.title.x = element_text(hjust=0.5, color="#87286E", face="bold", size=15, family="A"), 
      axis.title.y = element_text(hjust=0.5, color="#87286E", angle=90, face="bold", size=15, family="A"),
      panel.background = element_rect(fill="#ffffff", color="#000000", col="red"),
      plot.background = element_rect("#87286E"),
      #panel.grid.major = element_blank(),
      plot.title = element_text(hjust=0.5, face="bold", size=16, family="A", color="#ffffff"),
      plot.subtitle = element_text(hjust=0.5, face="bold", size=10, family="A", color="#ffffff"),
      legend.text = element_text(family="A", color="#ffffff", face="bold"),
      legend.title = element_text(family="A", color="#ffffff", face="bold"),
      legend.background = element_rect("#D04EA2") 
  )


NewTaipei_data <- g_data[ -which(g_data$鄉鎮市區 %in% Taipei), ]

NewTaipei_plot <- 
  NewTaipei_data %>%
  ggplot( aes(x=鄉鎮市區,y=平均單價) ) +
  geom_bar( stat="identity", aes(fill=平均單價) ) +
  geom_text( aes(label=paste0(round(平均單價/10000, 2), "萬")),
             hjust=1, col="white", family="A" )
  #geom_text( label=paste0(round(NewTaipei_data$平均單價/10000,2),"萬"),
  #           nudge_y=-min(NewTaipei_data$平均單價)/2.5, col="white", family="A" ) +
  scale_fill_gradient( low="#D999BF", high="#ec008c", na.value=NA ) +
  labs( title="「新北市」各區平均單價(坪)",
        subtitle="類別：「實際成交」") +
  coord_flip() +
  theme(
    axis.text.x = element_text(face="bold", size=12, angle=360, color="#ffffff"), 
    axis.text.y = element_text(face="bold", size=10, color="#ffffff"),
    axis.title.x = element_text(hjust=0.5, color="#87286E", face="bold", size=15, family="A"), 
    axis.title.y = element_text(hjust=0.5, color="#87286E", angle=90, face="bold", size=15, family="A"),
    panel.background = element_rect(fill="#ffffff", color="#000000", col="red"),
    plot.background = element_rect("#87286E"),
    #panel.grid.major = element_blank(),
    plot.title = element_text(hjust=0.5, face="bold", size=16, family="A", color="#ffffff"),
    plot.subtitle = element_text(hjust=0.5, face="bold", size=10, family="A", color="#ffffff"),
    legend.text = element_text(family="A", color="#ffffff", face="bold"),
    legend.title = element_text(family="A", color="#ffffff", face="bold"),
    legend.background = element_rect("#D04EA2") 
  )

Taipei_plot
NewTaipei_plot