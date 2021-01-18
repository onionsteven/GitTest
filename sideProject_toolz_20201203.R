library(ncdf4) #讀取nc檔
library(fields) #繪製image圖

#文件敘述：本檔案為sideProject工具集合內有以下工具：
#1.由挑選矩陣計算 氣候值 某種時期 之 平均
#2.由挑選矩陣計算 氣候值 大於或小於給定值 之 次數
#3.挑選矩陣產生器

#輸出路徑
setwd("C:/Users/rslab/Documents/sideProExport/")

###估計整體時間 
total_ptm=proc.time()

#讀取資料nc檔案----

#檔案路徑
#input_path="I:/1RSLABdata/松勳備份/CMIP5日資料NC檔/pr_BCSD_historical/ACCESS1-0/r1i1p1/pr_BCSD_ACCESS1-0.nc"
input_path="I:/1RSLABdata/松勳備份/CMIP5日資料NC檔/pr_BCSD_rcp26/bcc-csm1-1/r1i1p1/pr_BCSD_bcc-csm1-1.nc"
ncfile=nc_open(input_path)
#ncfile=nc_open("C:/Users/rslab/Downloads/AR5_統計_日_V2 -rcp85/AR5_統計_日_V2/pr_BCSD_rcp85/ACCESS1-3/r1i1p1/pr_BCSD_ACCESS1-3.nc")

#讀入資料
ptm=proc.time() #約10秒
data=ncvar_get(ncfile)
dim(data)
proc.time()-ptm

#讀取資料nc檔案end----


#20201029 藉由篩選矩陣 將95年的同一個旬/月 進行計算

#根據資料年數不同 給予不同的篩選矩陣----
#讀入月的篩選矩陣
load("C:/Users/rslab/BU/2018NJ/sideProject/sideProjectWorkPlace/pick_up_mat_for_month.RData")
#讀入旬的篩選矩陣
load("C:/Users/rslab/BU/2018NJ/sideProject/sideProjectWorkPlace/pick_up_mat_for_tenday.RData")

#讀入360專用 月的篩選矩陣
load("C:/Users/rslab/BU/2018NJ/sideProject/sideProjectWorkPlace/pick_up_mat_for_month_for_360.RData")
#讀入360專用 旬的篩選矩陣
load("C:/Users/rslab/BU/2018NJ/sideProject/sideProjectWorkPlace/pick_up_mat_for_tenday_for_360.RData")



#根據資料年數不同 給予不同的篩選矩陣 end----

#讀入月的篩選矩陣
load("C:/Users/rslab/BU/2018NJ/sideProject/sideProjectWorkPlace/pick_up_mat_for_month.RData")
#藉由篩選矩陣切割資料出來做計算
#計算 各月 中位數
ptm=proc.time() #12個月時間約90秒
median_month_list_v1=lapply(seq(1,12), function(x) apply(data[,,pick_up_mat_for_month[,x]],c(1,2),median))
proc.time()-ptm 

#計算 各月 平均
ptm=proc.time() #12個月時間約90秒
mean_month_list_v1=lapply(seq(1,12), function(x) apply(data[,,pick_up_mat_for_month[,x]],c(1,2),mean))
proc.time()-ptm 

#讀入旬的篩選矩陣
load("C:/Users/rslab/BU/2018NJ/sideProject/sideProjectWorkPlace/pick_up_mat_for_tenday.RData")
#藉由篩選矩陣切割資料出來做計算
#計算 各旬 中位數
ptm=proc.time() #36個旬時間約168秒
median_tenday_list_v1=lapply(seq(1,36), function(x) apply(data[,,pick_up_mat_for_tenday[,x]],c(1,2),median))
proc.time()-ptm 

#計算 各旬 平均
ptm=proc.time() #36個旬時間約168秒
mean_tenday_list_v1=lapply(seq(1,36), function(x) apply(data[,,pick_up_mat_for_tenday[,x]],c(1,2),mean))
proc.time()-ptm

#存檔：以RData格式 方便比較跨模式時讀取----
save(median_month_list_v1,file="median_month_list_v1.RData")
save(mean_month_list_v1,file="mean_month_list_v1.RData")

save(median_tenday_list_v1,file="median_tenday_list_v1.RData")
save(mean_tenday_list_v1,file="mean_tenday_list_v1.RData")

#存檔：以RData格式 方便比較跨模式時讀取 end----

#存檔：以迴圈輸出csv檔----
da_for_csv=median_tenday_list_v1
for(ci in 1:length(da_for_csv)){
  fn=paste("median_tenday_",ci,".csv",sep="");fn
  write.csv(da_for_csv[[ci]],fn)
}

da_for_csv=mean_tenday_list_v1
for(ci in 1:length(da_for_csv)){
  fn=paste("mean_tenday_",ci,".csv",sep="");fn
  write.csv(da_for_csv[[ci]],fn)
}

da_for_csv=median_month_list_v1
for(ci in 1:length(da_for_csv)){
  fn=paste("median_month_",ci,".csv",sep="");fn
  write.csv(da_for_csv[[ci]],fn)
}

da_for_csv=mean_month_list_v1
for(ci in 1:length(da_for_csv)){
  fn=paste("mean_month_",ci,".csv",sep="");fn
  write.csv(da_for_csv[[ci]],fn)
}
#存檔：以迴圈輸出csv檔 end----

#銜接：將list物件擺放成array的格式 以便後續做氣候值/連續天數篩選
dada=median_tenday_list_v1
sda=dada[[1]]
array_form=array(numeric(),c(dim(sda),length(dada)))
for(pi in 1:length(dada)){
  array_form[,,pi]=dada[[pi]]
}

#20201030 list to array
da_to_array=median_tenday_list_v1
array_form_v1=array(as.numeric(unlist(da_to_array)), dim=c(dim(da_to_array[[1]]),length(da_to_array)))

#工具：給定氣候值 給定連續天數 回傳發生次數----
#給定：氣候值/連續天數 
#求取：符合次數
#參數設定----
#給定某值
given_value=3
#設定大於給定值
#數值array轉維T/F array
#設定大於 小於
tel_array=array_form >= given_value 
#設定連續天數
continu_number = 2
#參數設定end----

#依照第三維度 列出來 
tel_by_dim_3rd=apply(tel_array,c(3),function(x) x) 
#計算T/F 的差值 並依此找符合氣候值的起迄位置
diff_of_dim_3rd = tel_by_dim_3rd[,-1] - tel_by_dim_3rd[,-ncol(tel_by_dim_3rd)]

#函數：藉由第三維度的diff矩陣 找出"連續天數" 之 "次數"
happen_countz_function=function(x,continu_number){
  
  quali_value_show_up=which(x==1) #符合值出現
  quali_value_ended=which(x==-1) #符合值在前一個位置出現
  
  #出現與結束是否成雙出現 若是則長度一樣  [若長度不一樣則 頭尾可能出問題]
  if(length(quali_value_show_up)!=length(quali_value_ended)){
    continuous_dayz=abs(quali_value_show_up[-length(quali_value_show_up)] - quali_value_ended) 
  } else {
    #連續天數
    continuous_dayz=abs(quali_value_show_up - quali_value_ended )
  }

  #發生次數
  oc=sum(continuous_dayz==continu_number)
  
  return(oc)
} 

ptm=proc.time() #約156秒
countz_of_tel_mat=apply(diff_of_dim_3rd,1,function(x) happen_countz_function(x,continu_number = continu_number))
proc.time()-ptm

#將次數結果排成原先影像矩陣的樣子
mat_of_countz=matrix(countz_of_tel_mat,nrow=60)

#計算結果另存為csv
write.csv(mat_of_countz,"mat_of_countz.csv")

#工具：給定氣候值 給定連續天數 回傳發生次數end----


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

proc.time()-total_ptm


#20201103 跨模式計算氣候值----

#讀取資料匣中的檔案名稱 並選取所要的部分
form_RData=list.files("I:/1RSLABdata/松勳備份/NCfile_OC",pattern=".RData")
form_RData
#RData數目 mean/median* month/tenday 有四種組合
length(form_RData) 
#平均
mean_pt=form_RData[sort(c(seq(1,length(form_RData)/4)*4-3,seq(1,length(form_RData)/4)*4-3+1))]
mean_pt
#月的平均
mean_month_pt=form_RData[sort(c(seq(1,length(form_RData)/4)*4-3,seq(1,length(form_RData)/4)*4-3+1))][seq(1,44,2)]
mean_month_pt
#旬的平均
mean_tenday_pt=form_RData[sort(c(seq(1,length(form_RData)/4)*4-3,seq(1,length(form_RData)/4)*4-3+1))][seq(2,44,2)]
mean_tenday_pt
#***此處選 tenday mean (旬的平均)為例

#設定氣候值mean/median 所在路徑
setwd("I:/1RSLABdata/松勳備份/NCfile_OC")

#從RData 讀進mean/median 並置於list中
over_modelz=list()
#旬檔名
#dada=mean_tenday_pt
#月檔名
dada=mean_month_pt
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
form_by_tenday=list()
for(i in 1:length(dada[[1]])){  #loop for 36旬
  s_arrary=array(NA,c(dim(dada[[1]][[1]]),length(dada)))
  for(i2 in 1:length(over_modelz)){ #loop for 13模式
    s_arrary[,,i2] = over_modelz[[i2]][[i]]
  }
  form_by_tenday[[i]]=s_arrary
}

#計算mean 橫跨模式的mean
input_da=form_by_tenday
ptm=proc.time() #12個月時間約90秒
list_of_mean_over_meanz=lapply(input_da, function(x) apply(x,c(1,2),mean))
proc.time()-ptm 

#初步繪製36旬的影像 
lapply(list_of_mean_over_meanz,image.plot)

#輸出另存為csv檔
da_for_csv=list_of_mean_over_meanz
for(ci in 1:length(da_for_csv)){
  fn=paste("mean_over_meanz_month_",ci,".csv",sep="");fn
  write.csv(da_for_csv[[ci]],fn)
}


#20201103 跨模式計算氣候值 end----


# 20201203 挑選矩陣產生器及檢視工具 ---------------------------------------------------

#函數：挑選矩陣產生器
#regular_dayz_in_yr= T 或 F
#period_to_cal有 "mon" / "tenday" 兩種

pick_up_mat_generator=function(regular_dayz_in_yr=T,period_to_cal="mon",data_over_yrz=30){
  
  #regular_dayz_in_yr == T 則每年日數為365 其他則是360
  if(regular_dayz_in_yr==T){
    #每個月的天數
    dayz_in_yr=c(31,28,31,30,31,30,31,31,30,31,30,31)
    #一年日數提醒
    print(c("一年日數",sum(dayz_in_yr)))
  }else{
    #每個月的天數 ***** 第二種是每個月皆30天
    dayz_in_yr=rep(30,12)
    #一年日數提醒
    print(c("一年日數",sum(dayz_in_yr)))
  }
  
  #分析單位：月----
  #選月資料時的尾
  month_tail_in_yr=cumsum(dayz_in_yr)
  month_tail_in_yr
  #選月資料時的頭
  month_head_in_yr=month_tail_in_yr-dayz_in_yr+1
  month_head_in_yr
  
  #分析單位：旬----
  #旬的起始日整理----
  #上旬起始日
  month_head_in_yr
  #中旬起始日
  month_head_in_yr+10
  #下旬起始日
  month_head_in_yr+20
  
  #一年中旬的起始日
  tenday_head_in_yr=as.vector(rbind(month_head_in_yr,month_head_in_yr+10,month_head_in_yr+20))
  #依照旬切割 一年有多少旬
  length(tenday_head_in_yr)
  
  #旬的結尾日整理----
  #上旬結尾日
  month_head_in_yr+9
  #中旬結尾日
  month_head_in_yr+19
  #下旬結尾日
  month_tail_in_yr=cumsum(dayz_in_yr)
  month_tail_in_yr
  
  #一年中旬的結尾日
  tenday_tail_in_yr=as.vector(rbind(month_head_in_yr+9,month_head_in_yr+19,month_tail_in_yr))
  
  #依照月旬 產生不同的切取頭尾
  if(period_to_cal=="mon"){
    
    cutting_head=month_head_in_yr
    cutting_tail=month_tail_in_yr
    
    #時期數提醒
    print(c("時期數",length(cutting_head)))
  }else if(period_to_cal=="tenday"){
    
    cutting_head=tenday_head_in_yr
    cutting_tail=tenday_tail_in_yr
    
    #時期數提醒
    print(c("時期數",length(cutting_head)))
  }else{return("out of setting")}
  
  #資料的年數 ***** 第二種是依照要分析的資料年數不同改變
  #data_over_yrz=30
  yrz=seq(1,data_over_yrz)
  
  #分析年數提醒
  print(c("分析年數",length(yrz)))
  
  #設置一個篩選矩陣(T/F) 每個column是第三維度要選出來的部分 row是旬數/月數 (36/12)
  pick_up_mat=c()
  for(pi in 1:length(cutting_head)){
    
    head_num = sum(dayz_in_yr) * yrz - sum(dayz_in_yr) + cutting_head[pi]
    tail_num = sum(dayz_in_yr) * yrz - sum(dayz_in_yr) + cutting_tail[pi]
    #F向量長度和第三維度一致
    #length_of_dim_3=dim(data)[3] ###<-------------------向量長度設定
    length_of_dim_3= sum(dayz_in_yr) * length(yrz)  ###<-------------------向量長度設定 #一年天數*資料年數
    #設置一F向量 與第三維度等長 將想擷取位置標上T
    long_F = rep(F,length_of_dim_3)
    for(ri in 1:length(head_num)){
      long_F[head_num[ri]:tail_num[ri]] = T
    }
    pick_up_mat = cbind(pick_up_mat,long_F)
  }
  return(pick_up_mat)
}

#使用
wtd_pick_up_mat=pick_up_mat_generator(regular_dayz_in_yr=T,period_to_cal="tenday",data_over_yrz=10)

pick_up_mat_for_month_dayz365_yrz10=pick_up_mat_generator(regular_dayz_in_yr=T,period_to_cal="mon",data_over_yrz=10)
pick_up_mat_for_month_dayz360_yrz10=pick_up_mat_generator(regular_dayz_in_yr=F,period_to_cal="mon",data_over_yrz=10)

#挑選矩陣命名 並且另存

save(pick_up_mat_for_month_dayz365_yrz10,file="pick_up_mat_for_month_dayz365_yrz10.RData")

save(pick_up_mat_for_month_dayz360_yrz10,file="pick_up_mat_for_month_dayz360_yrz10.RData")

# 檢查挑選矩陣(汎) ---------------------------------------------------------------

#確認篩選矩陣的維度
dim(wtd_pick_up_mat)
365*30
360*30

#影像確認
bmp("pick_up_mat_image_check.bmp",width = 480*5,height = 480*5)
image.plot(wtd_pick_up_mat)
dev.off()

#檢視各時期內天數
apply(wtd_pick_up_mat,2,sum)

#檢視挑選矩陣週期狀況
plot(wtd_pick_up_mat[1:(365*3),12],ty="l",xlab="時間軸",ylab="0/1表是否選取")
#檢視部分挑選矩陣
wtd_pick_up_mat[1:70,1:5]


