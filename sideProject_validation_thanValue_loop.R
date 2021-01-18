library(ncdf4) #讀取nc檔
library(fields) #繪製image圖
#library(arm) #array append

#20201102 #實際使用

# #讀入月的篩選矩陣
# load("C:/Users/rslab/BU/2018NJ/sideProject/sideProjectWorkPlace/pick_up_mat_for_month.RData")
# #讀入旬的篩選矩陣
# load("C:/Users/rslab/BU/2018NJ/sideProject/sideProjectWorkPlace/pick_up_mat_for_tenday.RData")

# #讀入360專用 月的篩選矩陣
# load("C:/Users/rslab/BU/2018NJ/sideProject/sideProjectWorkPlace/pick_up_mat_for_month_for_360.RData")
# #讀入360專用 旬的篩選矩陣
# load("C:/Users/rslab/BU/2018NJ/sideProject/sideProjectWorkPlace/pick_up_mat_for_tenday_for_360.RData")

#讀入365 月 篩選矩陣 10年
load("C:/Users/rslab/BU/2018NJ/sideProject/sideProjectWorkPlace/pick_up_mat_for_month_dayz365_yrz10.RData")
#讀入360 月 篩選矩陣 10年
load("C:/Users/rslab/BU/2018NJ/sideProject/sideProjectWorkPlace/pick_up_mat_for_month_dayz360_yrz10.RData")

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
#cite_path="I:/1RSLABdata/松勳備份/CMIP5日資料NC檔/pr_BCSD_historical/"
#驗證7 大於小於給定值 高溫大於35度
#cite_path="I:/1RSLABdata/松勳備份/CMIP5日資料NC檔/tasmax_BCSD_historical/"

#驗證8 大於小於給定值 低溫小於10度 10年 2050:2041~2055
cite_path="I:/1RSLABdata/松勳備份/CMIP5日資料NC檔/tasmin_BCSD_rcp85/"



#各模式 名字 放置於向量中
modelz_vector=list.files(cite_path)

#輸出存放位置
setwd("I:/1RSLABdata/松勳備份/NCfile_OC")



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
  ptm=proc.time() #約10秒
  data=ncvar_get(ncfile)
  dim(data)
  proc.time()-ptm
  print(dim(data))
  #資料年數
  print(dim(data)[3]/365)
  
  #讀取資料nc檔案end----
  
  #依照第三維度不同(整除於365 或 360) 給予不同的篩選矩陣 及資料截取
  if(dim(data)[3]%%365==0){
    pick_up_mat_for_month_follow_dim3=pick_up_mat_for_month_dayz365_yrz30
    #pick_up_mat_for_tenday_follow_dim3=pick_up_mat_for_tenday_for_360
    #取前30年
    data=data[,,1:(365*30)]
  }else{
    pick_up_mat_for_month_follow_dim3=pick_up_mat_for_month_dayz360_yrz30
    #pick_up_mat_for_tenday_follow_dim3=pick_up_mat_for_tenday
    #取前30年
    data=data[,,1:(360*30)]
  }
  print(dim(pick_up_mat_for_month_follow_dim3))
  dim(data)
  
  
  
  #工具：給定氣候值 給定連續天數 回傳發生次數----
  #給定：氣候值/連續天數 
  #求取：符合次數
  #參數設定----
  #給定某值
  given_value=10
  #設定大於給定值
  #數值array轉維T/F array
  #設定大於 小於
  tel_array=data <= given_value 
  #設定連續天數
  continu_number = 1
  #參數設定end----
  
  #20201118 藉篩選矩陣切割資料 計算次數[sum 總數]
  #20201130 藉篩選矩陣切割資料 計算平均次數[mean 再乘上年數]
  #計算 各月
  ptm=proc.time() #12個月時間約90秒
  #加總
  #count_month_mean_list=lapply(seq(1,12), function(x) apply(tel_array[,,pick_up_mat_for_month_follow_dim3[,x]],c(1,2),sum))
  #平均
  count_mean_list=lapply(seq(1,12), function(x) apply(tel_array[,,pick_up_mat_for_month_follow_dim3[,x]],c(1,2),mean))
  proc.time()-ptm 
  
  #將次數結果排成原先影像矩陣的樣子
  #mat_of_countz=matrix(countz_of_tel_mat,nrow=60)
  
  #計算結果另存為csv
  #write.csv(mat_of_countz,"mat_of_countz.csv")
  
  #工具：給定氣候值 給定連續天數 回傳發生次數end----
  
  #存檔：以RData格式 方便比較跨模式時讀取----
  save(count_mean_month_list,file=paste(fn_condition,"count_mean_month_list.RData",sep=""))
  #存檔：以RData格式 方便比較跨模式時讀取 end----
  
  #存檔：以迴圈輸出csv檔----
  da_for_csv=count_month_list
  for(ci in 1:length(da_for_csv)){
    fn=paste(fn_condition,"count_mean_month_",ci,".csv",sep="");fn
    write.csv(da_for_csv[[ci]],fn)
  }
  
  #存檔：以迴圈輸出csv檔 end----
  print(proc.time()-total_ptm)
}




#20201103 跨模式計算氣候值----

#讀取資料匣中的檔案名稱 並選取所要的部分
form_RData=list.files("I:/1RSLABdata/松勳備份/NCfile_OC",pattern=".RData")
form_RData
#RData數目 mean/median* month/tenday 有四種組合
length(form_RData) 
#月的平均
mean_pt=form_RData
mean_pt
length(mean_pt)
# #月的平均
# mean_month_pt=form_RData[sort(c(seq(1,length(form_RData)/4)*4-3,seq(1,length(form_RData)/4)*4-3+1))][seq(1,44,2)]
# mean_month_pt

#***此處選 tenday mean (旬的平均)為例

#設定氣候值mean/median 所在路徑
setwd("I:/1RSLABdata/松勳備份/NCfile_OC")

#從RData 讀進mean/median 並置於list中
over_modelz=list()
#旬檔名
#dada=mean_tenday_pt
#月檔名
dada=mean_pt
for(i in 1:length(dada)){
  load(dada[i])
  #旬平均
  #over_modelz[[i]]=mean_tenday_list_v1
  #月平均
  over_modelz[[i]]=count_month_list
}

#將跨模式氣候值的資料依照旬/月 區分資料
dada=over_modelz
#最小單位為60 81矩陣
form_by_month=list()
for(i in 1:length(dada[[1]])){  #loop for 36旬
  s_arrary=array(NA,c(dim(dada[[1]][[1]]),length(dada)))
  for(i2 in 1:length(over_modelz)){ #loop for 13模式
    s_arrary[,,i2] = over_modelz[[i2]][[i]]
  }
  form_by_month[[i]]=s_arrary
}

#計算count 橫跨模式的mean
input_da=form_by_month
ptm=proc.time() #12個月時間約90秒
list_of_mean_over_countz=lapply(input_da, function(x) apply(x,c(1,2),mean))
proc.time()-ptm 

save(list_of_mean_over_countz,file="list_of_mean_over_countz.RData")

# #初步繪製36旬的影像 
 lapply(list_of_mean_over_countz,image.plot)

# #輸出另存為csv檔
# da_for_csv=list_of_mean_over_meanz
# for(ci in 1:length(da_for_csv)){
#   fn=paste("mean_over_meanz_month_",ci,".csv",sep="");fn
#   write.csv(da_for_csv[[ci]],fn)
# }


#20201103 跨模式計算氣候值 end----





#圖片輸出----
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

#將影像矩陣中該空白處設為NA
dada=mat_of_countz
for(i in 1:nrow(grid_coord_of_na)){
  dada[grid_coord_of_na[i,1],grid_coord_of_na[i,2]]=NA
}

#設定圖片名稱 並以函數出圖
fn="fig_test"
jpeg_export(dada,fn)

#圖片輸出end----

#20201103 跨模式計算氣候值----

#讀取資料匣中的檔案名稱 並選取所要的部分
form_RData=list.files("I:/1RSLABdata/松勳備份/NCfile_OC",pattern=".RData")
form_RData
#RData數目 mean/median* month/tenday 有四種組合
length(form_RData) 
#月的平均
mean_pt=form_RData[seq(1,length(form_RData),2)]
mean_pt
length(mean_pt)
# #月的平均
# mean_month_pt=form_RData[sort(c(seq(1,length(form_RData)/4)*4-3,seq(1,length(form_RData)/4)*4-3+1))][seq(1,44,2)]
# mean_month_pt

#***此處選 tenday mean (旬的平均)為例

#設定氣候值mean/median 所在路徑
setwd("I:/1RSLABdata/松勳備份/NCfile_OC")

#從RData 讀進mean/median 並置於list中
over_modelz=list()
#旬檔名
#dada=mean_tenday_pt
#月檔名
dada=mean_pt
for(i in 1:length(dada)){
  load(dada[i])
  #旬平均
  #over_modelz[[i]]=mean_tenday_list_v1
  #月平均
  over_modelz[[i]]=mean_month_list_v1
}

#將跨模式氣候值的資料依照旬/月 區分資料
dada=over_modelz
#最小單位為60 81矩陣
form_by_month=list()
for(i in 1:length(dada[[1]])){  #loop for 36旬
  s_arrary=array(NA,c(dim(dada[[1]][[1]]),length(dada)))
  for(i2 in 1:length(over_modelz)){ #loop for 13模式
    s_arrary[,,i2] = over_modelz[[i2]][[i]]
  }
  form_by_month[[i]]=s_arrary
}

#計算mean 橫跨模式的mean
input_da=form_by_month
ptm=proc.time() #12個月時間約90秒
list_of_mean_over_meanz=lapply(input_da, function(x) apply(x,c(1,2),mean))
proc.time()-ptm 

save(list_of_mean_over_meanz,file="list_of_mean_over_meanz.RData")

#初步繪製36旬的影像
lapply(list_of_mean_over_meanz,image.plot)

# #輸出另存為csv檔
# da_for_csv=list_of_mean_over_meanz
# for(ci in 1:length(da_for_csv)){
#   fn=paste("mean_over_meanz_month_",ci,".csv",sep="");fn
#   write.csv(da_for_csv[[ci]],fn)
# }


#20201103 跨模式計算氣候值 end----



modelz_vector=list.files(cite_path)

for(i in 1:length(modelz_vector)){
  input_fn=list.files(paste(cite_path,modelz_vector[i],"/r1i1p1/",sep=""))
  if(length(input_fn)==0){
    print(c(i,"no file inside")) 
    next
    }
    fn_condition=gsub(".nc","_",input_fn);fn_condition
    #檔案路徑
    input_path=paste(cite_path,modelz_vector[i],"/r1i1p1/",input_fn,sep="")
    ncfile=nc_open(input_path)

    #讀取資料nc檔案----
    #讀入資料
    ptm=proc.time() #約10秒
    data=ncvar_get(ncfile)
    dim(data)
    proc.time()-ptm
    print(dim(data))
    print(c(i,dim(data)[3]/365))
    #讀取資料nc檔案end----
}


# #20201118 下班時間後的檢查 另一種做法
# #檢視後暫緩
# #載入月平均低溫
# load("I:/1RSLABdata/松勳備份/my_validation_box/tasmin_hist_base_mean/list_of_mean_over_meanz.RData")
# length(list_of_mean_over_meanz)
# dim(list_of_mean_over_meanz[[1]])

#20201120 整理產出資料
#設定資料匣路徑
#轉存1
#oc_path="I:/1RSLABdata/松勳備份/my_validation_box/tasmin_hist_base_mean"
#轉存2
#oc_path="I:/1RSLABdata/松勳備份/my_validation_box/pr_hist_base_80_count"
#轉存3
#oc_path="I:/1RSLABdata/松勳備份/my_validation_box/tas_hist_base_mean"
#轉存4
#oc_path="I:/1RSLABdata/松勳備份/my_validation_box/tasmax_hist_base_mean"
#轉存5
oc_path="I:/1RSLABdata/松勳備份/my_validation_box/pr_hist_base_mean"



#載入list 存放12個月計算結果
load(paste(oc_path,"/",list.files(oc_path,pattern="list_of"),sep=""))
length(list_of_mean_over_meanz)
sapply(list_of_mean_over_meanz,dim)

sapply(list_of_mean_over_meanz,function(x) range(x,na.rm = T))


#圖片檔案名稱和路徑資料匣相同
fig_name=tail(strsplit(oc_path,"/")[[1]],1)

#設color bar數值範圍
#bks=seq(50,1050,50)
bks=seq(-2,40,2)
#bks=seq(-1,10)
colz=tim.colors(length(bks)-1)

#月份縮寫
month_name=c("Jan","Feb","Mar","Apr","May","Jun","Jul","Aug","Sep","Oct","Nov","Dec")

jpeg(paste(fig_name,".jpg",sep=""),width = 480*3*1.5,height = 480*4*1.5)
layout(t(matrix(1:12, 3, 4)))
layout.show(12)

dada=list_of_mean_over_meanz
for(mi in 1:12){
  sda=dada[[mi]]
  image.plot(sda,legend.shrink = 1,legend.width=1.2*6,
             breaks = bks,
             col=colz,xaxt= "n", yaxt= "n",
             axis.args=list(cex.axis=1.5*2)
             #legend.args=list( text="num",cex=1.5*3, side=4, font=2)
             )
  legend("topleft",legend = month_name[mi],cex=1.5*2)
}

dev.off()

#輸出成CSV檔案----
dada=list_of_mean_over_meanz
da_for_csv=c()
for(mi in 1:12){
  sda=dada[[mi]]
  da_for_csv=cbind(da_for_csv,as.vector(sda))

}
#將NA以-99.9取代
da_for_csv[is.na(da_for_csv)]=-99.9


csv_format=read.csv("C:/Users/rslab/BU/2018NJ/sideProject/AR5_pr_daily/pr-rcp26/BNU-ESM/BNU-ESM_pr_2006.csv",header = T)
for_csv_format=cbind(csv_format[,3:4],da_for_csv)
write.table(for_csv_format,paste(fig_name,".csv",sep=""),row.names=F,sep=",",  col.names=FALSE)
#輸出成CSV檔案 end----

