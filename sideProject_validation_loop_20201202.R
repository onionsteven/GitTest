library(ncdf4) #讀取nc檔
library(fields) #繪製image圖
library(RColorBrewer) #產生範例用color bar 顏色


#20201102 #實際使用

# #讀入月的篩選矩陣
# load("C:/Users/rslab/BU/2018NJ/sideProject/sideProjectWorkPlace/pick_up_mat_for_month.RData")
# #讀入旬的篩選矩陣
# load("C:/Users/rslab/BU/2018NJ/sideProject/sideProjectWorkPlace/pick_up_mat_for_tenday.RData")

# #讀入360專用 月的篩選矩陣
# load("C:/Users/rslab/BU/2018NJ/sideProject/sideProjectWorkPlace/pick_up_mat_for_month_for_360.RData")
# #讀入360專用 旬的篩選矩陣
# load("C:/Users/rslab/BU/2018NJ/sideProject/sideProjectWorkPlace/pick_up_mat_for_tenday_for_360.RData")

# #讀入365 月 篩選矩陣 30年
# load("C:/Users/rslab/BU/2018NJ/sideProject/sideProjectWorkPlace/pick_up_mat_for_month_dayz365_yrz30.RData")
# #讀入360 月 篩選矩陣 30年
# load("C:/Users/rslab/BU/2018NJ/sideProject/sideProjectWorkPlace/pick_up_mat_for_month_dayz360_yrz30.RData")

#讀入365 月 篩選矩陣 10年
load("C:/Users/rslab/BU/2018NJ/sideProject/sideProjectWorkPlace/pick_up_mat_for_month_dayz365_yrz10.RData")
#讀入360 月 篩選矩陣 10年
load("C:/Users/rslab/BU/2018NJ/sideProject/sideProjectWorkPlace/pick_up_mat_for_month_dayz360_yrz10.RData")

#指定挑選矩陣為計算程式中的名稱 [載入的年份長度可能改變]
pick_up_mat_dayz365=pick_up_mat_for_month_dayz365_yrz10
pick_up_mat_dayz360=pick_up_mat_for_month_dayz360_yrz10

#遍歷功能: 

#先從一個條件下的所有模式開始
#cite_path="I:/1RSLABdata/松勳備份/CMIP5日資料NC檔/pr_BCSD_rcp26/"

#驗證1
#cite_path="I:/1RSLABdata/松勳備份/CMIP5日資料NC檔/tas_BCSD_historical/"
# #驗證2
# cite_path="I:/1RSLABdata/松勳備份/CMIP5日資料NC檔/tasmax_BCSD_historical/"
# #驗證3
#cite_path="I:/1RSLABdata/松勳備份/CMIP5日資料NC檔/tasmin_BCSD_historical/"
#驗證4
#cite_path="I:/1RSLABdata/松勳備份/CMIP5日資料NC檔/pr_BCSD_historical/"

#驗證5 大於小於給定值 日雨量降雨大於80mm
#cite_path="I:/1RSLABdata/松勳備份/CMIP5日資料NC檔/pr_BCSD_historical/"
#驗證6 大於小於給定值 低溫小於10度
#cite_path="I:/1RSLABdata/松勳備份/CMIP5日資料NC檔/tasmin_BCSD_historical/"
#驗證7 大於小於給定值 高溫大於35度
#cite_path="I:/1RSLABdata/松勳備份/CMIP5日資料NC檔/tasmax_BCSD_historical/"

#驗證8 大於小於給定值 低溫小於10度 10年 2050:2041~2055
#cite_path="I:/1RSLABdata/松勳備份/CMIP5日資料NC檔/tasmin_BCSD_rcp85/"
#驗證9 大於小於給定值 低溫小於10度 10年 2050:2041~2055
#cite_path="I:/1RSLABdata/松勳備份/CMIP5日資料NC檔/tasmax_BCSD_rcp85/"
#驗證10 大於小於給定值 日雨量降雨大於80mm 10年 2030:2036~2045
cite_path="I:/1RSLABdata/松勳備份/CMIP5日資料NC檔/pr_BCSD_rcp45/"
#驗證11 大於小於給定值 日雨量降雨大於80mm 10年 2050:2046~2055
#cite_path="I:/1RSLABdata/松勳備份/CMIP5日資料NC檔/pr_BCSD_rcp85/"


#各模式 名字 放置於向量中
modelz_vector=list.files(cite_path)

#輸出存放位置
setwd("I:/1RSLABdata/松勳備份/NCfile_OC")

# #待分析資料為前Y年的資料
# yrz_num_to_process=30
# #待分析資料年分範圍
# yrz_range_to_process=1:30

#依據需求挑選資料

#氣候時段基期(1976-2005) 為45年中的 1-30
#氣候時段2030(2026-2035) 為95年中的51-60
#氣候時段2050(2046-2055) 為95年中的71-80

#需求資料起始年 ; #需求資料末端年
quest_head_yr = 51 ;quest_tail_yr = 60 

#20201130 改動紀錄：設置一個空間存放跨模式計算結果 迴圈處理各模式 於結束後計算跨模式平均
oc_over_modelz=c()
ptm_over_model=proc.time()
for(i in 1:length(modelz_vector)){
  
  input_fn=list.files(paste(cite_path,modelz_vector[i],"/r1i1p1/",sep=""))
  #檢查檔案是否存在---- 
  if(length(input_fn)==0){
    print(c(i,"no file inside")) 
    next
  }
  #檢查檔案是否存在 end----
  fn_condition=gsub(".nc","_",input_fn);fn_condition
  #檔案路徑
  input_path=paste(cite_path,modelz_vector[i],"/r1i1p1/",input_fn,sep="")
  
  ncfile=nc_open(input_path)
  
  ###估計整體時間 
  total_ptm=proc.time()
  
  #讀取資料nc檔案----
  #讀入資料
  
  data=ncvar_get(ncfile)
  print(c("迴圈子","資料維度"))
  print(c(i,dim(data)))
  
  
  #讀取資料nc檔案end----
  
  #依照第三維度不同(整除於365 或 360) 給予不同的篩選矩陣 及資料截取
  if(dim(data)[3]%%365==0){
    #資料年數提醒
    print("每年365日")
    print("資料年數")
    print(dim(data)[3]/365)
    
    pick_up_mat_for_month_follow_dim3=pick_up_mat_dayz365
    #pick_up_mat_for_tenday_follow_dim3=pick_up_mat_for_tenday_for_360
    
    #365天
    dayz_of_month=c(31,28,31,30,31,30,31,31,30,31,30,31)
    dayz_in_yr=sum(dayz_of_month)
    
    #取前(yrz_num_to_process)年
    #data=data[,,1:(dayz_in_yr*yrz_num_to_process)]
    
    #依據需求擷取資料 #取代上行
    data=data[,,(quest_head_yr*dayz_in_yr-dayz_in_yr+1):(quest_tail_yr*dayz_in_yr)]
                #需求資料起始                           #需求資料末端
  }else{
    
    #資料年數提醒
    print("----------每年360日")
    print("資料年數")
    print(dim(data)[3]/360)
    
    pick_up_mat_for_month_follow_dim3=pick_up_mat_dayz360
    #pick_up_mat_for_tenday_follow_dim3=pick_up_mat_for_tenday
    
    #每個月的天數
    #360天
    dayz_of_month=rep(30,12)
    dayz_in_yr=sum(dayz_of_month)
    
    #取前30(yrz_num_to_process)年
    #data=data[,,1:(dayz_in_yr*yrz_num_to_process)]
    
    #依據需求擷取資料 #取代上行
    data=data[,,(quest_head_yr*dayz_in_yr-dayz_in_yr+1):(quest_tail_yr*dayz_in_yr)]
                #需求資料起始                           #需求資料末端
  }
  #print(dim(pick_up_mat_for_month_follow_dim3))
  #dim(data)
  
  
  #工具：給定氣候值 給定連續天數 回傳發生次數----
  #給定：氣候值/連續天數 
  #求取：符合次數
  #參數設定----
  #給定某值
  given_value = 80
  #設定大於給定值
  #數值array轉維T/F array
  #設定大於 小於
  tel_array = data >= given_value 
  #設定連續天數
  continu_number = 1
  #參數設定end----
  
  #20201118 藉篩選矩陣切割資料 計算次數
  #計算 各月
  ptm=proc.time() #12個月時間約90秒
  count_month_list = lapply(seq(1,12), function(x) apply(tel_array[,,pick_up_mat_for_month_follow_dim3[,x]],c(1,2),mean))
  proc.time()-ptm 
  
  #所有日數(每月日數*年數)平均 * 每月日數 => 跨30年的月平均
  count_month_mean_list = lapply(seq(1,12),function(x) count_month_list[[x]]*dayz_of_month[x])
  
  #將次數結果排成原先影像矩陣的樣子
  #mat_of_countz=matrix(countz_of_tel_mat,nrow=60)
  
  #計算結果另存為csv
  #write.csv(mat_of_countz,"mat_of_countz.csv")
  
  #工具：給定氣候值 給定連續天數 回傳發生次數end----
  
  #PS: 直接用array彙整各模式之後 輸出各模式計算結果的功能先停用
  #存檔：以RData格式 方便比較跨模式時讀取----
  #save(count_month_list,file=paste(fn_condition,"count_month_list.RData",sep=""))
  #存檔：以RData格式 方便比較跨模式時讀取 end----
  
  # #存檔：以迴圈輸出csv檔----
  # da_for_csv=count_month_list
  # for(ci in 1:length(da_for_csv)){
  #   fn=paste(fn_condition,"count_month_",ci,".csv",sep="");fn
  #   write.csv(da_for_csv[[ci]],fn)
  # }
  #存檔：以迴圈輸出csv檔 end----
  
  #計算結果排成array格式
  count_month_mean_array = array(as.numeric(unlist(count_month_mean_list)), dim=c(60, 81, 12))
  
  #合併各模式計算結果 以array格式並排
  if(length(oc_over_modelz)==0) dim_4th=1 else {dim_4th=dim(oc_over_modelz)[4]+1}
  oc_over_modelz = array(c(oc_over_modelz, count_month_mean_array), dim=c(60, 81, 12, dim_4th))
  
  print(proc.time()-total_ptm)
}
proc.time() - ptm_over_model
#各模式計算結果輸出成RData
#檔案命名用名稱
file_name=tail(strsplit(cite_path,"/")[[1]],1);file_name
save(oc_over_modelz,file=paste(file_name,"_oc_over_modelz.RData",sep=""))

#設置空間存放跨模式計算結果 並且計算跨模式平均
#確認跨模式結果的資料維度[四維 矩陣 時期(月/旬) 模式數目]
dim(oc_over_modelz)
#計算跨模式平均
oc_modelz_mean = apply(oc_over_modelz,c(1,2,3),mean)
#確認跨模式平均的資料維度[三維 矩陣 時期(月/旬)]
dim(oc_modelz_mean)
#檢視 12月份 次數範圍
apply(oc_over_modelz,3,function(x) range(x,na.rm = T))


# 輸出組合圖片 ------------------------------------------------------------------


#繪製並輸出圖片[12個月 置於4*3的組合圖]----

#圖片檔名：和路徑資料匣相同
fig_name=tail(strsplit(cite_path,"/")[[1]],1);fig_name

#color bar數值範圍設定----
#bks=seq(50,1050,50)
#bks=seq(-2,90,2)
bks=seq(0,10,.1) #降雨80+ mm
#bks=seq(0,31,.1) #低溫10- C #高溫35+ C
#bks=seq(-1,4)
#bks=seq(-2,140,2)

#color bar顏色調整
#colz=tim.colors(length(bks)-1)
colz=rev(colorRampPalette(c(rev(tim.colors(55)),rev(brewer.pal(9,"Blues"))))(length(bks)-1))

#呈現color bar顏色測試
col_show=matrix(rep(bks,each=10),nrow=10)
image.plot(col_show,col=colz,legend.shrink = 1)

#月份縮寫
month_name=c("Jan","Feb","Mar","Apr","May","Jun","Jul","Aug","Sep","Oct","Nov","Dec")

jpeg(paste(fig_name,".jpg",sep=""),width = 480*3*1.5,height = 480*4*1.5)

#圖片依據12個月切割
layout(t(matrix(1:12, 3, 4)))
layout.show(12)

#待輸出物件
dada = oc_modelz_mean 
for(mi in 1:12){
  sda=dada[,,mi]
  image.plot(sda,legend.shrink = 1,legend.width=1.2*6,
             breaks = bks,
             col=colz,xaxt= "n", yaxt= "n",
             axis.args=list(cex.axis=1.5*2)
  )
  #添加月份於圖片左上角
  legend("topleft",legend = month_name[mi],cex=1.5*2)
}
dev.off()

#繪製圖片end----


# 輸出CSV -------------------------------------------------------------------



#輸出成CSV檔案----
#將跨模式 月平均 次數array 轉為矩陣 方便csv輸出
array_to_da_for_csv = matrix(unlist(oc_modelz_mean),ncol = 12)
#檢視for csv檔案的維度
dim(array_to_da_for_csv)

#將NA以-99.9取代
array_to_da_for_csv[is.na(array_to_da_for_csv)]=-99.9

#讀入一個範例csv檔案
csv_format=read.csv("C:/Users/rslab/BU/2018NJ/sideProject/AR5_pr_daily/pr-rcp26/BNU-ESM/BNU-ESM_pr_2006.csv",header = T)
#取網格位置部分
for_csv_format=cbind(csv_format[,3:4],array_to_da_for_csv)
write.table(for_csv_format,paste(fig_name,".csv",sep=""),row.names=F,sep=",",  col.names=FALSE)
#輸出成CSV檔案 end----



# 後為備用區 -------------------------------------------------------------------

#圖片輸出測試 之後加入台灣地圖----
#函數：給定資料及檔案名稱
jpeg_export=function(fig_da,fn="test"){
  jpeg(paste(fn,".jpg",sep=""))
  image.plot(x=seq(119.2,122.15,0.05),y=seq(21.5,25.5,0.05),fig_da,legend.shrink = 1,
             xlab="經度",ylab="緯度")
  legend("bottomright",legend = c("",""))
  
  dev.off()
}




#讀入需要設為NA的網格點
load("C:/Users/rslab/BU/2018NJ/sideProject/sideProjectWorkPlace/grid_coord_of_na.RData")
head(grid_coord_of_na)
dim(grid_coord_of_na)

#將影像矩陣中該空白處設為NA
dada=mat_of_countz
for(i in 1:nrow(grid_coord_of_na)){
  dada[grid_coord_of_na[i,1],grid_coord_of_na[i,2]]=NA
}

#設定圖片名稱 並以函數出圖
fn="fig_test"
jpeg_export(dada,fn)

#圖片輸出測試 之後加入台灣地圖 end----

#創資料夾及路徑 資料夾名稱為產生時間
#完成一次小結果 要輸出時進行此功能：產生資料夾並指定為路徑 
#之後要再變更路徑 指定為處理資料路徑

#例：
#setwd("分析用路徑")
# 分析結果完成

#創資料夾並指定路徑

#輸出結果[圖/csv]

#setwd("下個分析路徑")

cr_path=paste("I:/1RSLABdata/松勳備份/NCfile_OC/",substr(Sys.time(),1,16),"_Cal_OC/",sep="")
#檢視路徑
cr_path
#產生路徑
dir.create(cr_path)
#設為路徑 放置產出資料
setwd(cr_path)

#20210111 測試加上地圖
library(rgdal) #read shp file
library(lattice) #調用levelplot

#讀入地圖用shapefile，並取出基隆/台北市/新北市
tw_map=readOGR("C:/Users/rslab/BU/2018NJ/visualize/報告用圖片/190328/TWmap/COUNTY_MOI_1070516.shp")

#登瑋卉敏的座標轉換程式碼
wgs84=CRS("+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs")
twd97=CRS("+proj=tmerc +lat_0=0 +lon_0=121 +k=0.9999 +x_0=250000 +y_0=0 +ellps=GRS80 +units=m +no_defs")
a=SpatialPoints(cbind(station298[,4],station298[,5]),wgs84)
b=spTransform(a,twd97)

#tw_map_tm2=spTransform(tw_map,twd97)

plot(tw_map)


tt=t(matrix(csv_format[,6],ncol=60))
image.plot(x=seq(119.2,122.15,0.05),y=seq(21.5,25.5,0.05),tt,legend.shrink = 1,
           col=colz,breaks = bks,
           xlab="經度",ylab="緯度")
plot(tw_map,add=T)