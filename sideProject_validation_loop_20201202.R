library(ncdf4) #Ū��nc��
library(fields) #ø�simage��
library(RColorBrewer) #���ͽd�ҥ�color bar �C��


#20201102 #��ڨϥ�

# #Ū�J�몺�z��x�}
# load("C:/Users/rslab/BU/2018NJ/sideProject/sideProjectWorkPlace/pick_up_mat_for_month.RData")
# #Ū�J�����z��x�}
# load("C:/Users/rslab/BU/2018NJ/sideProject/sideProjectWorkPlace/pick_up_mat_for_tenday.RData")

# #Ū�J360�M�� �몺�z��x�}
# load("C:/Users/rslab/BU/2018NJ/sideProject/sideProjectWorkPlace/pick_up_mat_for_month_for_360.RData")
# #Ū�J360�M�� �����z��x�}
# load("C:/Users/rslab/BU/2018NJ/sideProject/sideProjectWorkPlace/pick_up_mat_for_tenday_for_360.RData")

# #Ū�J365 �� �z��x�} 30�~
# load("C:/Users/rslab/BU/2018NJ/sideProject/sideProjectWorkPlace/pick_up_mat_for_month_dayz365_yrz30.RData")
# #Ū�J360 �� �z��x�} 30�~
# load("C:/Users/rslab/BU/2018NJ/sideProject/sideProjectWorkPlace/pick_up_mat_for_month_dayz360_yrz30.RData")

#Ū�J365 �� �z��x�} 10�~
load("C:/Users/rslab/BU/2018NJ/sideProject/sideProjectWorkPlace/pick_up_mat_for_month_dayz365_yrz10.RData")
#Ū�J360 �� �z��x�} 10�~
load("C:/Users/rslab/BU/2018NJ/sideProject/sideProjectWorkPlace/pick_up_mat_for_month_dayz360_yrz10.RData")

#���w�D��x�}���p��{�������W�� [���J���~�����ץi�����]
pick_up_mat_dayz365=pick_up_mat_for_month_dayz365_yrz10
pick_up_mat_dayz360=pick_up_mat_for_month_dayz360_yrz10

#�M���\��: 

#���q�@�ӱ���U���Ҧ��Ҧ��}�l
#cite_path="I:/1RSLABdata/�Q���ƥ�/CMIP5����NC��/pr_BCSD_rcp26/"

#����1
#cite_path="I:/1RSLABdata/�Q���ƥ�/CMIP5����NC��/tas_BCSD_historical/"
# #����2
# cite_path="I:/1RSLABdata/�Q���ƥ�/CMIP5����NC��/tasmax_BCSD_historical/"
# #����3
#cite_path="I:/1RSLABdata/�Q���ƥ�/CMIP5����NC��/tasmin_BCSD_historical/"
#����4
#cite_path="I:/1RSLABdata/�Q���ƥ�/CMIP5����NC��/pr_BCSD_historical/"

#����5 �j��p�󵹩w�� ��B�q���B�j��80mm
#cite_path="I:/1RSLABdata/�Q���ƥ�/CMIP5����NC��/pr_BCSD_historical/"
#����6 �j��p�󵹩w�� �C�Ťp��10��
#cite_path="I:/1RSLABdata/�Q���ƥ�/CMIP5����NC��/tasmin_BCSD_historical/"
#����7 �j��p�󵹩w�� ���Ťj��35��
#cite_path="I:/1RSLABdata/�Q���ƥ�/CMIP5����NC��/tasmax_BCSD_historical/"

#����8 �j��p�󵹩w�� �C�Ťp��10�� 10�~ 2050:2041~2055
#cite_path="I:/1RSLABdata/�Q���ƥ�/CMIP5����NC��/tasmin_BCSD_rcp85/"
#����9 �j��p�󵹩w�� �C�Ťp��10�� 10�~ 2050:2041~2055
#cite_path="I:/1RSLABdata/�Q���ƥ�/CMIP5����NC��/tasmax_BCSD_rcp85/"
#����10 �j��p�󵹩w�� ��B�q���B�j��80mm 10�~ 2030:2036~2045
cite_path="I:/1RSLABdata/�Q���ƥ�/CMIP5����NC��/pr_BCSD_rcp45/"
#����11 �j��p�󵹩w�� ��B�q���B�j��80mm 10�~ 2050:2046~2055
#cite_path="I:/1RSLABdata/�Q���ƥ�/CMIP5����NC��/pr_BCSD_rcp85/"


#�U�Ҧ� �W�r ��m��V�q��
modelz_vector=list.files(cite_path)

#��X�s���m
setwd("I:/1RSLABdata/�Q���ƥ�/NCfile_OC")

# #�ݤ��R��Ƭ��eY�~�����
# yrz_num_to_process=30
# #�ݤ��R��Ʀ~���d��
# yrz_range_to_process=1:30

#�̾ڻݨD�D����

#��Ԯɬq���(1976-2005) ��45�~���� 1-30
#��Ԯɬq2030(2026-2035) ��95�~����51-60
#��Ԯɬq2050(2046-2055) ��95�~����71-80

#�ݨD��ư_�l�~ ; #�ݨD��ƥ��ݦ~
quest_head_yr = 51 ;quest_tail_yr = 60 

#20201130 ��ʬ����G�]�m�@�ӪŶ��s���Ҧ��p�⵲�G �j��B�z�U�Ҧ� �󵲧���p���Ҧ�����
oc_over_modelz=c()
ptm_over_model=proc.time()
for(i in 1:length(modelz_vector)){
  
  input_fn=list.files(paste(cite_path,modelz_vector[i],"/r1i1p1/",sep=""))
  #�ˬd�ɮ׬O�_�s�b---- 
  if(length(input_fn)==0){
    print(c(i,"no file inside")) 
    next
  }
  #�ˬd�ɮ׬O�_�s�b end----
  fn_condition=gsub(".nc","_",input_fn);fn_condition
  #�ɮ׸��|
  input_path=paste(cite_path,modelz_vector[i],"/r1i1p1/",input_fn,sep="")
  
  ncfile=nc_open(input_path)
  
  ###���p����ɶ� 
  total_ptm=proc.time()
  
  #Ū�����nc�ɮ�----
  #Ū�J���
  
  data=ncvar_get(ncfile)
  print(c("�j��l","��ƺ���"))
  print(c(i,dim(data)))
  
  
  #Ū�����nc�ɮ�end----
  
  #�̷ӲĤT���פ��P(�㰣��365 �� 360) �������P���z��x�} �θ�ƺI��
  if(dim(data)[3]%%365==0){
    #��Ʀ~�ƴ���
    print("�C�~365��")
    print("��Ʀ~��")
    print(dim(data)[3]/365)
    
    pick_up_mat_for_month_follow_dim3=pick_up_mat_dayz365
    #pick_up_mat_for_tenday_follow_dim3=pick_up_mat_for_tenday_for_360
    
    #365��
    dayz_of_month=c(31,28,31,30,31,30,31,31,30,31,30,31)
    dayz_in_yr=sum(dayz_of_month)
    
    #���e(yrz_num_to_process)�~
    #data=data[,,1:(dayz_in_yr*yrz_num_to_process)]
    
    #�̾ڻݨD�^����� #���N�W��
    data=data[,,(quest_head_yr*dayz_in_yr-dayz_in_yr+1):(quest_tail_yr*dayz_in_yr)]
                #�ݨD��ư_�l                           #�ݨD��ƥ���
  }else{
    
    #��Ʀ~�ƴ���
    print("----------�C�~360��")
    print("��Ʀ~��")
    print(dim(data)[3]/360)
    
    pick_up_mat_for_month_follow_dim3=pick_up_mat_dayz360
    #pick_up_mat_for_tenday_follow_dim3=pick_up_mat_for_tenday
    
    #�C�Ӥ몺�Ѽ�
    #360��
    dayz_of_month=rep(30,12)
    dayz_in_yr=sum(dayz_of_month)
    
    #���e30(yrz_num_to_process)�~
    #data=data[,,1:(dayz_in_yr*yrz_num_to_process)]
    
    #�̾ڻݨD�^����� #���N�W��
    data=data[,,(quest_head_yr*dayz_in_yr-dayz_in_yr+1):(quest_tail_yr*dayz_in_yr)]
                #�ݨD��ư_�l                           #�ݨD��ƥ���
  }
  #print(dim(pick_up_mat_for_month_follow_dim3))
  #dim(data)
  
  
  #�u��G���w��ԭ� ���w�s��Ѽ� �^�ǵo�ͦ���----
  #���w�G��ԭ�/�s��Ѽ� 
  #�D���G�ŦX����
  #�ѼƳ]�w----
  #���w�Y��
  given_value = 80
  #�]�w�j�󵹩w��
  #�ƭ�array���T/F array
  #�]�w�j�� �p��
  tel_array = data >= given_value 
  #�]�w�s��Ѽ�
  continu_number = 1
  #�ѼƳ]�wend----
  
  #20201118 �ǿz��x�}���θ�� �p�⦸��
  #�p�� �U��
  ptm=proc.time() #12�Ӥ�ɶ���90��
  count_month_list = lapply(seq(1,12), function(x) apply(tel_array[,,pick_up_mat_for_month_follow_dim3[,x]],c(1,2),mean))
  proc.time()-ptm 
  
  #�Ҧ����(�C����*�~��)���� * �C���� => ��30�~���륭��
  count_month_mean_list = lapply(seq(1,12),function(x) count_month_list[[x]]*dayz_of_month[x])
  
  #�N���Ƶ��G�Ʀ�����v���x�}���ˤl
  #mat_of_countz=matrix(countz_of_tel_mat,nrow=60)
  
  #�p�⵲�G�t�s��csv
  #write.csv(mat_of_countz,"mat_of_countz.csv")
  
  #�u��G���w��ԭ� ���w�s��Ѽ� �^�ǵo�ͦ���end----
  
  #PS: ������array�J��U�Ҧ����� ��X�U�Ҧ��p�⵲�G���\�������
  #�s�ɡG�HRData�榡 ��K�����Ҧ���Ū��----
  #save(count_month_list,file=paste(fn_condition,"count_month_list.RData",sep=""))
  #�s�ɡG�HRData�榡 ��K�����Ҧ���Ū�� end----
  
  # #�s�ɡG�H�j���Xcsv��----
  # da_for_csv=count_month_list
  # for(ci in 1:length(da_for_csv)){
  #   fn=paste(fn_condition,"count_month_",ci,".csv",sep="");fn
  #   write.csv(da_for_csv[[ci]],fn)
  # }
  #�s�ɡG�H�j���Xcsv�� end----
  
  #�p�⵲�G�Ʀ�array�榡
  count_month_mean_array = array(as.numeric(unlist(count_month_mean_list)), dim=c(60, 81, 12))
  
  #�X�֦U�Ҧ��p�⵲�G �Harray�榡�ñ�
  if(length(oc_over_modelz)==0) dim_4th=1 else {dim_4th=dim(oc_over_modelz)[4]+1}
  oc_over_modelz = array(c(oc_over_modelz, count_month_mean_array), dim=c(60, 81, 12, dim_4th))
  
  print(proc.time()-total_ptm)
}
proc.time() - ptm_over_model
#�U�Ҧ��p�⵲�G��X��RData
#�ɮשR�W�ΦW��
file_name=tail(strsplit(cite_path,"/")[[1]],1);file_name
save(oc_over_modelz,file=paste(file_name,"_oc_over_modelz.RData",sep=""))

#�]�m�Ŷ��s���Ҧ��p�⵲�G �åB�p���Ҧ�����
#�T�{��Ҧ����G����ƺ���[�|�� �x�} �ɴ�(��/��) �Ҧ��ƥ�]
dim(oc_over_modelz)
#�p���Ҧ�����
oc_modelz_mean = apply(oc_over_modelz,c(1,2,3),mean)
#�T�{��Ҧ���������ƺ���[�T�� �x�} �ɴ�(��/��)]
dim(oc_modelz_mean)
#�˵� 12��� ���ƽd��
apply(oc_over_modelz,3,function(x) range(x,na.rm = T))


# ��X�զX�Ϥ� ------------------------------------------------------------------


#ø�s�ÿ�X�Ϥ�[12�Ӥ� �m��4*3���զX��]----

#�Ϥ��ɦW�G�M���|��ƧX�ۦP
fig_name=tail(strsplit(cite_path,"/")[[1]],1);fig_name

#color bar�ƭȽd��]�w----
#bks=seq(50,1050,50)
#bks=seq(-2,90,2)
bks=seq(0,10,.1) #���B80+ mm
#bks=seq(0,31,.1) #�C��10- C #����35+ C
#bks=seq(-1,4)
#bks=seq(-2,140,2)

#color bar�C��վ�
#colz=tim.colors(length(bks)-1)
colz=rev(colorRampPalette(c(rev(tim.colors(55)),rev(brewer.pal(9,"Blues"))))(length(bks)-1))

#�e�{color bar�C�����
col_show=matrix(rep(bks,each=10),nrow=10)
image.plot(col_show,col=colz,legend.shrink = 1)

#����Y�g
month_name=c("Jan","Feb","Mar","Apr","May","Jun","Jul","Aug","Sep","Oct","Nov","Dec")

jpeg(paste(fig_name,".jpg",sep=""),width = 480*3*1.5,height = 480*4*1.5)

#�Ϥ��̾�12�Ӥ����
layout(t(matrix(1:12, 3, 4)))
layout.show(12)

#�ݿ�X����
dada = oc_modelz_mean 
for(mi in 1:12){
  sda=dada[,,mi]
  image.plot(sda,legend.shrink = 1,legend.width=1.2*6,
             breaks = bks,
             col=colz,xaxt= "n", yaxt= "n",
             axis.args=list(cex.axis=1.5*2)
  )
  #�K�[�����Ϥ����W��
  legend("topleft",legend = month_name[mi],cex=1.5*2)
}
dev.off()

#ø�s�Ϥ�end----


# ��XCSV -------------------------------------------------------------------



#��X��CSV�ɮ�----
#�N��Ҧ� �륭�� ����array �ର�x�} ��Kcsv��X
array_to_da_for_csv = matrix(unlist(oc_modelz_mean),ncol = 12)
#�˵�for csv�ɮת�����
dim(array_to_da_for_csv)

#�NNA�H-99.9���N
array_to_da_for_csv[is.na(array_to_da_for_csv)]=-99.9

#Ū�J�@�ӽd��csv�ɮ�
csv_format=read.csv("C:/Users/rslab/BU/2018NJ/sideProject/AR5_pr_daily/pr-rcp26/BNU-ESM/BNU-ESM_pr_2006.csv",header = T)
#�������m����
for_csv_format=cbind(csv_format[,3:4],array_to_da_for_csv)
write.table(for_csv_format,paste(fig_name,".csv",sep=""),row.names=F,sep=",",  col.names=FALSE)
#��X��CSV�ɮ� end----



# �ᬰ�ƥΰ� -------------------------------------------------------------------

#�Ϥ���X���� ����[�J�x�W�a��----
#��ơG���w��Ƥ��ɮצW��
jpeg_export=function(fig_da,fn="test"){
  jpeg(paste(fn,".jpg",sep=""))
  image.plot(x=seq(119.2,122.15,0.05),y=seq(21.5,25.5,0.05),fig_da,legend.shrink = 1,
             xlab="�g��",ylab="�n��")
  legend("bottomright",legend = c("",""))
  
  dev.off()
}




#Ū�J�ݭn�]��NA�������I
load("C:/Users/rslab/BU/2018NJ/sideProject/sideProjectWorkPlace/grid_coord_of_na.RData")
head(grid_coord_of_na)
dim(grid_coord_of_na)

#�N�v���x�}���ӪťճB�]��NA
dada=mat_of_countz
for(i in 1:nrow(grid_coord_of_na)){
  dada[grid_coord_of_na[i,1],grid_coord_of_na[i,2]]=NA
}

#�]�w�Ϥ��W�� �åH��ƥX��
fn="fig_test"
jpeg_export(dada,fn)

#�Ϥ���X���� ����[�J�x�W�a�� end----

#�и�Ƨ��θ��| ��Ƨ��W�٬����ͮɶ�
#�����@���p���G �n��X�ɶi�榹�\��G���͸�Ƨ��ë��w�����| 
#����n�A�ܧ���| ���w���B�z��Ƹ��|

#�ҡG
#setwd("���R�θ��|")
# ���R���G����

#�и�Ƨ��ë��w���|

#��X���G[��/csv]

#setwd("�U�Ӥ��R���|")

cr_path=paste("I:/1RSLABdata/�Q���ƥ�/NCfile_OC/",substr(Sys.time(),1,16),"_Cal_OC/",sep="")
#�˵����|
cr_path
#���͸��|
dir.create(cr_path)
#�]�����| ��m���X���
setwd(cr_path)

#20210111 ���ե[�W�a��
library(rgdal) #read shp file
library(lattice) #�ե�levelplot

#Ū�J�a�ϥ�shapefile�A�è��X��/�x�_��/�s�_��
tw_map=readOGR("C:/Users/rslab/BU/2018NJ/visualize/���i�ιϤ�/190328/TWmap/COUNTY_MOI_1070516.shp")

#�n޳�c�Ӫ��y���ഫ�{���X
wgs84=CRS("+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs")
twd97=CRS("+proj=tmerc +lat_0=0 +lon_0=121 +k=0.9999 +x_0=250000 +y_0=0 +ellps=GRS80 +units=m +no_defs")
a=SpatialPoints(cbind(station298[,4],station298[,5]),wgs84)
b=spTransform(a,twd97)

#tw_map_tm2=spTransform(tw_map,twd97)

plot(tw_map)


tt=t(matrix(csv_format[,6],ncol=60))
image.plot(x=seq(119.2,122.15,0.05),y=seq(21.5,25.5,0.05),tt,legend.shrink = 1,
           col=colz,breaks = bks,
           xlab="�g��",ylab="�n��")
plot(tw_map,add=T)