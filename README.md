# Project

## 趨勢：In R
* Mann Kendall Trend Test：
  * 相關資料：https://github.com/sueshow/R_Data-Visualization
  * Code：Proj_Mann Kendall Tend Test.R
* 參考資料
  * Appendix Mann-Kendall Trend Tests
  * https://www.statisticshowto.com/mann-kendall-trend-test/
  * https://cran.r-project.org/web/packages/trend/vignettes/trend.pdf
<br>

## 實價登錄資料：In R
* Crawl：
  * 資料網址：https://plvr.land.moi.gov.tw/DownloadOpenData
  * Code：Proj_實價登錄資料.R
* Text Mining：
  * 重點回顧：https://github.com/sueshow/R_Text-Mining
  * Code：Proj_地址.R
* 視覺化(Bar)：
  * Code：Proj_視覺化.R
* 參考資料
  * 全國路名資料：https://data.gov.tw/dataset/35321
<br>

## Crosstab Query：
* 範例：json_object_agg In PostgreSQL  
```
CREATE TEMP TABLE t (
  section   text
, status    text
, ct        integer  -- don't use "count" as column name.
);

INSERT INTO t VALUES 
  ('A', 'Active', 1), ('A', 'Inactive', 2)
, ('B', 'Active', 4), ('B', 'Inactive', 5)
                   , ('C', 'Inactive', 7); 


SELECT section,
       (obj ->> 'Active')::int AS active,
       (obj ->> 'Inactive')::int AS inactive
FROM (SELECT section, json_object_agg(status,ct) AS obj
      FROM t
      GROUP BY section
     )X
```
<br>

## 參考資料：
* https://stackoverflow.com/questions/3002499/postgresql-crosstab-query
