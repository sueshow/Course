rm(list=ls())



# Download Library
#install.packages('dplyr')
library(dplyr)
#install.packages('sqldf')
library(sqldf)
#install.packages('tidyr')
library(tidyr)
#install.packages('tidyverse')
#library(tidyverse)
#install.packages('factoextra')
library(factoextra)
#install.packages('NbClust')
#library(NbClust)
#install.packages("fpc")
#library(fpc)
#install.packages("dbscan")
#library(dbscan)
#install.packages("WeightedCluster")
library(WeightedCluster)

library(ggpubr)



#memory.limit()
memory.limit(size=5600000)

# Current Path
getwd()

path <- 'D:\\資料分析彙整\\Project\\天然災害風險分析\\資料'
setwd(path)
getwd()



# Load Data
#rain_data <- read.csv('rainfall_history_alert.csv', header=TRUE)
#rain_data <- read.csv('rainfall_everyday_20201029.csv', header=TRUE)
rain_data <- read.csv('rainfall_everyday_20201105.csv', header=TRUE)



# Length of rain_data
len <- nrow(rain_data)
length(rain_data)

# Check Data
sapply(rain_data, function(x) sum(is.infinite(x))/len)  #Check Inf
## Result -> 僅有「pcpn_1h_max」有異常值
sapply(rain_data, function(x) sum(is.na(x))/len)        #Check NA
#sapply(rain_data, function(x) sum(is.na(x))/length(x))        
## Result -> 「landslip_warning」、「slope_warning」缺漏值偏高

# Replace -Inf as 0
rain_data_modify <- do.call(data.frame, lapply(rain_data, function(x) replace(x, is.infinite(x),0)))


# Double Check「pcpn_1h_max」
sapply(rain_data_modify, function(x) sum(is.infinite(x))/len)  #Check Inf



#################################### Check NA Data ####################################
dim(rain_data_modify)

data <- rain_data_modify[,c('landslip_warning','slope_warning','縣市','鄉鎮市區')]

over_all <- data %>% 
                 group_by(data$'縣市', data$'鄉鎮市區') %>% 
                 summarize(count=n()) 

colnames(over_all) <- c('縣市', '鄉鎮市區', '筆數')

#landslip_warning
landslip <- data %>%                                                                                         ## The source dataset
                 group_by(data$'縣市', data$'鄉鎮市區', data$'landslip_warning') %>%                         ## Grouping variables
                 summarize(count=n()) %>%                                                                    ## aggregation of the c2 column
                 ungroup() ##%>%                                                                             ## spread doesn't seem to like groups
                 ##spread(data$'landslip_warning', count)                                                    ## spread makes the data wide                 
#class(landslip)
colnames(landslip) <- c('縣市', '鄉鎮市區', 'landslip_warning_type', '筆數')

landslip_all <- merge(x=landslip, y=over_all, by=c('縣市','鄉鎮市區'), all.x=TRUE)

#county_landslip_all_na <- landslip[which(is.na(landslip$landslip_warning_type) & landslip$筆數 > 3000),]
landslip_all_na <- landslip_all[which(is.na(landslip_all$landslip_warning_type) & landslip_all$筆數.x==landslip_all$筆數.y),]

#slope_warning
slope <- data %>% 
              group_by(data[,'縣市'],data[,'鄉鎮市區'],data[,'slope_warning']) %>% 
              summarize(n()) %>%                 
              ungroup()

colnames(slope) <- c('縣市', '鄉鎮市區', 'slope_warning_type', '筆數')

slope_all <- merge(x=slope, y=over_all, by=c('縣市','鄉鎮市區'), all.x=TRUE)

#county_slope_all_na <- slope[which(is.na(slope$slope_warning_type) & slope$筆數 > 3000),]
slope_all_na <- slope_all[which(is.na(slope_all$slope_warning_type) & slope_all$筆數.x==slope_all$筆數.y),]


na_data <- merge(x=landslip_all_na, y=slope_all_na, by=c('縣市','鄉鎮市區'), all=TRUE)

colnames(rain_data_modify)

data %>% distinct(landslip_warning)
data %>% distinct(slope_warning)
#######################################################################################



# Replace NA as -1
rain_data_modify <- do.call(data.frame, lapply(rain_data_modify, function(x) replace(x, is.na(x),-1)))

rain_data_modify %>% distinct(landslip_warning)
rain_data_modify %>% distinct(slope_warning)



#20201029
#rain_data_modify$avg_rain_hours <- ifelse(rain_data_modify$raining_hours==0, 0, rain_data_modify$pcpn_24h_max/rain_data_modify$raining_hours)

rain_data_modify$pcpn_1h_max <- as.numeric(rain_data_modify$pcpn_1h_max)

# Replace NA as 0
rain_data_modify <- do.call(data.frame, lapply(rain_data_modify, function(x) replace(x, is.na(x),0)))

#rain_data_modify$compare <- rain_data_modify$raining_hours * rain_data_modify$pcpn_1h_max
#20201105
rain_data_modify$avg_rain_hours <- ifelse(rain_data_modify$raining_hours==0, 0, ifelse(rain_data_modify$raining_hours * rain_data_modify$pcpn_1h_max >= rain_data_modify$pcpn_24h_max, rain_data_modify$pcpn_24h_max/rain_data_modify$raining_hours, rain_data_modify$pcpn_1h_max))

rain_data_modify$reach_12_pcpn1h_rate <- ifelse(rain_data_modify$raining_hours==0, 0, rain_data_modify$reach_pcpn1h_hours/rain_data_modify$raining_hours*100)
rain_data_modify$reach_12_pcpn24h_rate <- ifelse(rain_data_modify$raining_hours==0, 0, rain_data_modify$reach_pcpn24h_hours/rain_data_modify$raining_hours*100)
rain_data_modify$pcpn_24h_max_one_day <- ifelse(rain_data_modify$raining_hours * rain_data_modify$pcpn_1h_max >= rain_data_modify$pcpn_24h_max, '0', '1')
#20201105 V4
rain_data_modify$avg_all_factor <- apply(rain_data_modify[27:30], 1, mean)


rain_data_modify$slope_0.5 <- ifelse(rain_data_modify$slope_warning==-1, -1, rain_data_modify$slope_0.5)
rain_data_modify$landslip_0.5 <- ifelse(rain_data_modify$landslip_warning==-1, -1, rain_data_modify$landslip_0.5)


max(rain_data_modify$avg_rain_hours)
colnames(rain_data_modify)
sapply(rain_data_modify, function(x) sum(is.na(rain_data_modify$pcpn_1h_max))/len)        #Check NA


#################################### 畫Scatter Plot ####################################
plot(rain_data_modify$pcpn_1h_max, rain_data_modify$type, main="Scatterplot Example",
     xlab="pcpn_1h_max", ylab="type", pch=19)

cor(rain_data_modify$pcpn_1h_max, rain_data_modify$avg_rain_hours, method="spearman")
########################################################################################



# Extract data without slope
##含(坡地災害警戒值、土石流災害警戒值)、X1小時二級警戒、X24小時二級警戒
#20201029
data_cluster <- rain_data_modify[,c(8,16,18:22,24:(dim(rain_data_modify)[2]-6),(dim(rain_data_modify)[2]-3))]
#20201105_V1
data_cluster <- rain_data_modify[,c(8,16,18,22:25,31:(dim(rain_data_modify)[2]-8),(dim(rain_data_modify)[2]-3))]



#20201105
data_cluster <- rain_data_modify[,c(8,16,22:25,31:(dim(rain_data_modify)[2]-7),(dim(rain_data_modify)[2]-3):(dim(rain_data_modify)[2]))]

#20201105_V2
data_cluster <- rain_data_modify[,c(8,16,18,22,27:30,(dim(rain_data_modify)[2]-3):(dim(rain_data_modify)[2]-3))]

#20201105_V3
data_cluster <- rain_data_modify[,c(18,22,27:30,(dim(rain_data_modify)[2]-3):(dim(rain_data_modify)[2]-2))]

#20201105_V4
data_cluster <- rain_data_modify[,c(18,22,27:30,(dim(rain_data_modify)[2]-4):(dim(rain_data_modify)[2]-3),dim(rain_data_modify)[2])]



##不含
data_cluster <- rain_data_modify[,c(18,22:25,31:(dim(rain_data_modify)[2]-7),(dim(rain_data_modify)[2]-3),(dim(rain_data_modify)[2]-2),(dim(rain_data_modify)[2]))]

colnames(data_cluster)
dim(data_cluster)



# Kmeans 
cluster_kmeans <- kmeans(data_cluster, centers=10, nstart=dim(data_cluster)[2], iter.max=20)
#warnings()
##畫圖
fviz_cluster(cluster_kmeans, data_cluster, frame=FALSE, geom='point')

cluster_kmeans_ori <- data.frame(cluster_kmeans$cluster, rain_data)
saveRDS(object=cluster_kmeans_ori, file='cluster_kmeans_ori.rds')


# kmedoids
# (NOT RUN -> 錯誤：無法配置大小...)
#d <- dist(data_cluster) 
#cluster_kmedoids <- wcKMedoids(d, k=10, method='KMedoids')

# (NOT RUN -> 筆數太多)
#cluster_kmedoids <- pam(data_cluster, k=10)

cluster_kmedoids <- clara(data_cluster, k=10, stand=T, samples=500, pamLike=TRUE)
##畫圖
fviz_cluster(cluster_kmedoids, data_cluster, frame=FALSE, geom='point')

cluster_kmedoids_ori <- data.frame(cluster_kmedoids$cluster, rain_data)
saveRDS(object=cluster_kmedoids_ori, file='cluster_kmedoids_ori.rds')


##################################### DBSCAN ####################################
# (NOT RUN -> 跑2天還沒跑出來)
kNNdistplot(data_cluster, k=4)
abline(h=0.5, col='red', lty=2)
cluster_DBSCAN <- fpc::dbscan(data_cluster, eps=0.15)
fviz_cluster(cluster_DBSCAN, data_cluster, stand=FALSE, frame=FALSE, geom='point')

cluster_dbscan_ori <- data.frame(cluster_dbscan$cluster, rain_data)
saveRDS(object=cluster_dbscan_ori, file='cluster_dbscan_ori.rds')
#################################################################################



#############################################################################

# Kmeans
cluster_kmeans_ori <- readRDS(file='cluster_kmeans_ori.rds', refhook=NULL)

cluster_kmeans_ori_2 <- do.call(data.frame, lapply(cluster_kmeans_ori, function(x) replace(x, is.infinite(x), 0)))
cluster_kmeans_ori_2 <- do.call(data.frame, lapply(cluster_kmeans_ori_2, function(x) replace(x, is.na(x),-1)))
#cluster_kmeans_ori_2$avg_rain_hours <- ifelse(cluster_kmeans_ori_2$raining_hours==0, 0, cluster_kmeans_ori_2$pcpn_24h_max/cluster_kmeans_ori_2$raining_hours)
cluster_kmeans_ori_2$avg_rain_hours <- ifelse(cluster_kmeans_ori_2$raining_hours==0, 0, ifelse(cluster_kmeans_ori_2$raining_hours * cluster_kmeans_ori_2$pcpn_1h_max >= cluster_kmeans_ori_2$pcpn_24h_max, cluster_kmeans_ori_2$pcpn_24h_max/cluster_kmeans_ori_2$raining_hours, cluster_kmeans_ori_2$pcpn_1h_max))

cluster_kmeans_ori_2$reach_12_pcpn1h_rate <- ifelse(cluster_kmeans_ori_2$raining_hours==0, 0, cluster_kmeans_ori_2$reach_pcpn1h_hours/cluster_kmeans_ori_2$raining_hours)
cluster_kmeans_ori_2$reach_12_pcpn24h_rate <- ifelse(cluster_kmeans_ori_2$raining_hours==0, 0, cluster_kmeans_ori_2$reach_pcpn24h_hours/cluster_kmeans_ori_2$raining_hours)
#cluster_kmeans_ori_2$pcpn_24h_max_one_day <- ifelse(cluster_kmeans_ori_2$raining_hours * cluster_kmeans_ori_2$pcpn_1h_max >= cluster_kmeans_ori_2$pcpn_24h_max, '1', '0')


## case 1 ##
cluster_kmeans_ori_2 %>%
  arrange(desc(pcpn_1h_max), desc(avg_rain_hours), desc(pcpn_24h_max))%>%
  rename(cluster_kmeans_cluster=cluster_kmeans.cluster) -> final

#cluster_kmeans_ori_2 %>%
#  arrange(desc(pcpn_1h_max))%>%
#  rename(cluster_kmeans_cluster=cluster_kmeans.cluster) -> final

final %>% 
  group_by(cluster_kmeans_cluster) %>%
  summarise(avg_1_amount=mean(pcpn_1h_max), rain_hours_amount=mean(avg_rain_hours), avg_24_amount=mean(pcpn_24h_max)) %>%
  arrange(desc(avg_1_amount), desc(rain_hours_amount), desc(avg_24_amount)) -> a

## case 2 ## 結果相同
cluster_kmeans_ori_2 %>%
  arrange(desc(pcpn_1h_max), desc(avg_rain_hours))%>%
  rename(cluster_kmeans_cluster=cluster_kmeans.cluster) -> final

final %>% 
  group_by(cluster_kmeans_cluster) %>%
  summarise(avg_1_amount=mean(pcpn_1h_max), rain_hours_amount=mean(avg_rain_hours)) %>%
  arrange(desc(avg_1_amount), desc(rain_hours_amount)) -> a

a

b <- sqldf('select a.cluster_kmeans_cluster
            from a')
b <- cbind(rownames(b), b) 
colnames(b) <- c('rank', 'cluster_kmeans_cluster')

final_over <- sqldf('select (11-b.rank) as cluster_kmedoids_actual,
                     final.*
                     from final
                     left join b on b.cluster_kmeans_cluster=final.cluster_kmeans_cluster')

amax <- sqldf('select (11-b.rank) as cluster_kmeans_actual,
                      final.*
               from final
               left join b on b.cluster_kmeans_cluster=final.cluster_kmeans_cluster
               where b.rank=1')

a100 <- sqldf('select (11-b.rank) as cluster_kmeans_actual,
                      final.*
               from final
               left join b on b.cluster_kmeans_cluster=final.cluster_kmeans_cluster
               where final.pcpn_1h_max>=100')             

saveRDS(object=final_over, file='cluster_kmeans_actual.rds')
#====================================================================================#
final_over <- sqldf('select case when cluster_kmeans_cluster=5 then 10
                            when cluster_kmeans_cluster=6 then 9
                            when cluster_kmeans_cluster=2 then 8
                            when cluster_kmeans_cluster=8 then 7
                            when cluster_kmeans_cluster=9 then 6
                            when cluster_kmeans_cluster=4 then 5
                            when cluster_kmeans_cluster=1 then 4
                            when cluster_kmeans_cluster=3 then 3
                            when cluster_kmeans_cluster=10 then 2
                            when cluster_kmeans_cluster=7 then 1
                       end as cluster_kmeans_actual
                       ,*
              from final')
#====================================================================================#



#############################################################################

# kmedoids
cluster_kmedoids_ori <- readRDS(file='cluster_kmedoids_ori.rds', refhook=NULL)

cluster_kmedoids_ori_2 <- do.call(data.frame, lapply(cluster_kmedoids_ori, function(x) replace(x, is.infinite(x), 0)))
cluster_kmedoids_ori_2 <- do.call(data.frame, lapply(cluster_kmedoids_ori_2, function(x) replace(x, is.na(x),-1)))

cluster_kmedoids_ori_2$pcpn_1h_max <- as.numeric(cluster_kmedoids_ori_2$pcpn_1h_max)
# Replace NA as 0
rain_data_modify <- do.call(data.frame, lapply(rain_data_modify, function(x) replace(x, is.na(x),0)))

#cluster_kmedoids_ori_2$compare <- cluster_kmedoids_ori_2$raining_hours * cluster_kmedoids_ori_2$pcpn_1h_max

#cluster_kmedoids_ori_2$avg_rain_hours <- ifelse(cluster_kmedoids_ori_2$raining_hours==0, 0, cluster_kmedoids_ori_2$pcpn_24h_max/cluster_kmedoids_ori_2$raining_hours)
cluster_kmedoids_ori_2$avg_rain_hours <- ifelse(cluster_kmedoids_ori_2$raining_hours==0, 0, ifelse(cluster_kmedoids_ori_2$raining_hours * cluster_kmedoids_ori_2$pcpn_1h_max >= cluster_kmedoids_ori_2$pcpn_24h_max, cluster_kmedoids_ori_2$pcpn_24h_max/cluster_kmedoids_ori_2$raining_hours, cluster_kmedoids_ori_2$pcpn_1h_max))

cluster_kmedoids_ori_2$reach_12_pcpn1h_rate <- ifelse(cluster_kmedoids_ori_2$raining_hours==0, 0, cluster_kmedoids_ori_2$reach_pcpn1h_hours/cluster_kmedoids_ori_2$raining_hours)
cluster_kmedoids_ori_2$reach_12_pcpn24h_rate <- ifelse(cluster_kmedoids_ori_2$raining_hours==0, 0, cluster_kmedoids_ori_2$reach_pcpn24h_hours/cluster_kmedoids_ori_2$raining_hours)
#cluster_kmedoids_ori_2$pcpn_24h_max_one_day <- ifelse(cluster_kmedoids_ori_2$raining_hours * cluster_kmedoids_ori_2$pcpn_1h_max >= cluster_kmedoids_ori_2$pcpn_24h_max, '1', '0')


## case 1 ##
cluster_kmedoids_ori_2 %>%
  arrange(desc(pcpn_1h_max), desc(avg_rain_hours), desc(pcpn_24h_max)) %>%
  rename(cluster_kmedoids_cluster=cluster_kmedoids.cluster) -> final

final %>% 
  group_by(cluster_kmedoids_cluster) %>%
  summarise(avg_1_amount=mean(pcpn_1h_max), rain_hours_amount=mean(avg_rain_hours), avg_24_amount=mean(pcpn_24h_max)) %>%
  arrange(desc(avg_1_amount), desc(rain_hours_amount), desc(avg_24_amount)) -> a


## case 2 ## 結果相同
cluster_kmedoids_ori_2 %>%
  arrange(desc(pcpn_1h_max), desc(avg_rain_hours))%>%
  rename(cluster_kmedoids_cluster=cluster_kmedoids.cluster) -> final

final %>% 
  group_by(cluster_kmedoids_cluster) %>%
  summarise(avg_1_amount=mean(pcpn_1h_max), rain_hours_amount=mean(avg_rain_hours)) %>%
  arrange(desc(avg_1_amount), desc(rain_hours_amount)) -> a


a

b <- sqldf('select a.cluster_kmedoids_cluster
            from a')
b <- cbind(rownames(b), b) 
colnames(b) <- c('rank', 'cluster_kmedoids_cluster')

final_over <- sqldf('select (11-b.rank) as cluster_kmedoids_actual,
                     final.*
                     from final
                     left join b on b.cluster_kmedoids_cluster=final.cluster_kmedoids_cluster')

amax <- sqldf('select (11-b.rank) as cluster_kmeans_actual,
                      final.*
               from final
               left join b on b.cluster_kmedoids_cluster=final.cluster_kmedoids_cluster
               where b.rank=1')

a100 <- sqldf('select (11-b.rank) as cluster_kmedoids_cluster,
                      final.*
               from final
               left join b on b.cluster_kmedoids_cluster=final.cluster_kmedoids_cluster
               where final.pcpn_1h_max>=100')             

saveRDS(object=final_over, file='cluster_kmedoids_actual.rds')

#====================================================================================#
final_over <- sqldf('select case when cluster_kmedoids_cluster=3 then 10
                            when cluster_kmedoids_cluster=6 then 9
                            when cluster_kmedoids_cluster=5 then 8
                            when cluster_kmedoids_cluster=4 then 7
                            when cluster_kmedoids_cluster=7 then 6
                            when cluster_kmedoids_cluster=2 then 5
                            when cluster_kmedoids_cluster=10 then 4
                            when cluster_kmedoids_cluster=8 then 3
                            when cluster_kmedoids_cluster=1 then 2
                            when cluster_kmedoids_cluster=9 then 1
                       end as cluster_kmedoids_actual
                       ,*
              from final')
#====================================================================================#

#############################################################################

cluster_kmedoids_actual <- readRDS(file='cluster_kmedoids_actual.rds', refhook=NULL)
#write.csv(cluster_kmedoids_actual, 'cluster_kmedoids_actual_aa.csv')

cluster_kmedoids_actual <- final_over

######################### to ############################
a <- cluster_kmedoids_actual[,c(1,4:6,46:47)]

write.csv(a, "cluster_kmedoids_actual_v.csv", )
saveRDS(object=a, file='cluster_kmedoids_actual_v.rds')

########################################################
#Mean
final_mean_year <- cluster_kmedoids_actual[,c('cluster_kmedoids_actual', 'year', '縣市', '鄉鎮市區')] %>% 
                   group_by(year, 縣市, 鄉鎮市區) %>%
                   summarise( avg_actual=mean(cluster_kmedoids_actual) )


final_mean_all <- cluster_kmedoids_actual[,c('cluster_kmedoids_actual', '縣市', '鄉鎮市區')] %>% 
                   group_by(縣市, 鄉鎮市區) %>%
                   summarise( avg_actual=mean(cluster_kmedoids_actual) )


#mode
final_mode_year <- cluster_kmedoids_actual[,c('cluster_kmedoids_actual', 'year', '縣市', '鄉鎮市區')] %>% 
  group_by(year, 縣市, 鄉鎮市區) %>%                                               # Groups by the Name column
  count(cluster_kmedoids_actual) %>%                                               # Counts each Color by Name, creates a new column n
  mutate(max=max(n), total=sum(n)) %>%          # Creates a new column for the max(n) by Name
  subset(n==max) %>%                                                               # Keeps only those rows where n equals max(n)
  mutate(ratio=n/total) %>%
  select(縣市, 鄉鎮市區, year, Mode=cluster_kmedoids_actual, n, total, ratio) # Keeps only the Name, Color, and n columns and
                                                                                   # renames Color as Mode


final_mode_all <- cluster_kmedoids_actual[,c('cluster_kmedoids_actual', 'year', '縣市', '鄉鎮市區')] %>% 
  group_by(縣市, 鄉鎮市區) %>%                                                     # Groups by the Name column
  count(cluster_kmedoids_actual) %>%                                               # Counts each Color by Name, creates a new column n
  mutate(max=max(n), total=sum(n)) %>%          # Creates a new column for the max(n) by Name
  subset(n==max) %>%                                                               # Keeps only those rows where n equals max(n)
  mutate(ratio=n/total) %>%
  select(縣市, 鄉鎮市區, Mode=cluster_kmedoids_actual, n, total, ratio)




final_over_year <- sqldf('select a.year 
                                 , a.縣市, a.鄉鎮市區
                                 , a.avg_actual as avg
                                 , case when avg_actual>=9.5 and avg_actual<=10 then 10
                                        when avg_actual>=8.5 and avg_actual<9.5 then 9
                                        when avg_actual>=7.5 and avg_actual<8.5 then 8
                                        when avg_actual>=6.5 and avg_actual<7.5 then 7
                                        when avg_actual>=5.5 and avg_actual<6.5 then 6
                                        when avg_actual>=4.5 and avg_actual<5.5 then 5
                                        when avg_actual>=3.5 and avg_actual<4.5 then 4
                                        when avg_actual>=2.5 and avg_actual<3.5 then 3
                                        when avg_actual>=1.5 and avg_actual<2.5 then 2
                                        when avg_actual>=1 and avg_actual<1.5 then 1
                                        end as fin_avg
                                 , b.Mode
                                 , b.ratio
                                 , b.n
                                 , b.total
                    from final_mean_year a
                    left join final_mode_year b on a.縣市=b.縣市 and a.鄉鎮市區=b.鄉鎮市區 and a.year=b.year')



final_over_all <- sqldf('select a.縣市, a.鄉鎮市區
                                 , a.avg_actual as avg
                                 , case when avg_actual>=9.5 and avg_actual<=10 then 10
                                        when avg_actual>=8.5 and avg_actual<9.5 then 9
                                        when avg_actual>=7.5 and avg_actual<8.5 then 8
                                        when avg_actual>=6.5 and avg_actual<7.5 then 7
                                        when avg_actual>=5.5 and avg_actual<6.5 then 6
                                        when avg_actual>=4.5 and avg_actual<5.5 then 5
                                        when avg_actual>=3.5 and avg_actual<4.5 then 4
                                        when avg_actual>=2.5 and avg_actual<3.5 then 3
                                        when avg_actual>=1.5 and avg_actual<2.5 then 2
                                        when avg_actual>=1 and avg_actual<1.5 then 1
                                        end as fin_avg
                                 , b.Mode
                                 , b.ratio
                                 , b.n
                                 , b.total
                        from final_mean_all a
                        left join final_mode_all b on a.縣市=b.縣市 and a.鄉鎮市區=b.鄉鎮市區')

final_over <- union_all(final_over_year, final_over_all)

saveRDS(object=final_over, file='cluster_kmedoids_actual_year_all.rds')
saveRDS(object=final_over_all, file='cluster_kmedoids_actual_all.rds')
write.csv(final_over, 'cluster_kmedoids_actual_year_all_ab.csv')

#############################################################################

flow_torrent <- read.csv(file=".\\Data\\其他資料_土石流潛勢溪流資料_20200107.csv", fileEncoding="big5")




#############################################################################


#參考資料
##【pivot table】https://stackoverflow.com/questions/18622854/how-to-create-a-pivot-table-in-r-with-multiple-3-variables
##【merge】https://stackoverflow.com/questions/1299871/how-to-join-merge-data-frames-inner-outer-left-right
##【read KML】https://gis.stackexchange.com/questions/58131/reading-kml-file-into-r
##https://officeguide.cc/r-hcpc-hierarchical-clustering-on-principal-components-factominer-tutorial/
##http://www.sthda.com/english/wiki/correlation-test-between-two-variables-in-r


data <- load(paste0(path, '\\rainfall_today.RData'))
head(data)









