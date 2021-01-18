library(ncdf4) #Ū��nc��
library(fields) #ø�simage��
#library(arm) #array append

#20201102 #��ڨϥ�

# #Ū�J�몺�z��x�}
# load("C:/Users/rslab/BU/2018NJ/sideProject/sideProjectWorkPlace/pick_up_mat_for_month.RData")
# #Ū�J�����z��x�}
# load("C:/Users/rslab/BU/2018NJ/sideProject/sideProjectWorkPlace/pick_up_mat_for_tenday.RData")

# #Ū�J360�M�� �몺�z��x�}
# load("C:/Users/rslab/BU/2018NJ/sideProject/sideProjectWorkPlace/pick_up_mat_for_month_for_360.RData")
# #Ū�J360�M�� �����z��x�}
# load("C:/Users/rslab/BU/2018NJ/sideProject/sideProjectWorkPlace/pick_up_mat_for_tenday_for_360.RData")

#Ū�J365 �� �z��x�} 10�~
load("C:/Users/rslab/BU/2018NJ/sideProject/sideProjectWorkPlace/pick_up_mat_for_month_dayz365_yrz10.RData")
#Ū�J360 �� �z��x�} 10�~
load("C:/Users/rslab/BU/2018NJ/sideProject/sideProjectWorkPlace/pick_up_mat_for_month_dayz360_yrz10.RData")

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
#cite_path="I:/1RSLABdata/�Q���ƥ�/CMIP5����NC��/pr_BCSD_historical/"
#����7 �j��p�󵹩w�� ���Ťj��35��
#cite_path="I:/1RSLABdata/�Q���ƥ�/CMIP5����NC��/tasmax_BCSD_historical/"

#����8 �j��p�󵹩w�� �C�Ťp��10�� 10�~ 2050:2041~2055
cite_path="I:/1RSLABdata/�Q���ƥ�/CMIP5����NC��/tasmin_BCSD_rcp85/"



#�U�Ҧ� �W�r ��m��V�q��
modelz_vector=list.files(cite_path)

#��X�s���m
setwd("I:/1RSLABdata/�Q���ƥ�/NCfile_OC")



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
  ptm=proc.time() #��10��
  data=ncvar_get(ncfile)
  dim(data)
  proc.time()-ptm
  print(dim(data))
  #��Ʀ~��
  print(dim(data)[3]/365)
  
  #Ū�����nc�ɮ�end----
  
  #�̷ӲĤT���פ��P(�㰣��365 �� 360) �������P���z��x�} �θ�ƺI��
  if(dim(data)[3]%%365==0){
    pick_up_mat_for_month_follow_dim3=pick_up_mat_for_month_dayz365_yrz30
    #pick_up_mat_for_tenday_follow_dim3=pick_up_mat_for_tenday_for_360
    #���e30�~
    data=data[,,1:(365*30)]
  }else{
    pick_up_mat_for_month_follow_dim3=pick_up_mat_for_month_dayz360_yrz30
    #pick_up_mat_for_tenday_follow_dim3=pick_up_mat_for_tenday
    #���e30�~
    data=data[,,1:(360*30)]
  }
  print(dim(pick_up_mat_for_month_follow_dim3))
  dim(data)
  
  
  
  #�u��G���w��ԭ� ���w�s��Ѽ� �^�ǵo�ͦ���----
  #���w�G��ԭ�/�s��Ѽ� 
  #�D���G�ŦX����
  #�ѼƳ]�w----
  #���w�Y��
  given_value=10
  #�]�w�j�󵹩w��
  #�ƭ�array���T/F array
  #�]�w�j�� �p��
  tel_array=data <= given_value 
  #�]�w�s��Ѽ�
  continu_number = 1
  #�ѼƳ]�wend----
  
  #20201118 �ǿz��x�}���θ�� �p�⦸��[sum �`��]
  #20201130 �ǿz��x�}���θ�� �p�⥭������[mean �A���W�~��]
  #�p�� �U��
  ptm=proc.time() #12�Ӥ�ɶ���90��
  #�[�`
  #count_month_mean_list=lapply(seq(1,12), function(x) apply(tel_array[,,pick_up_mat_for_month_follow_dim3[,x]],c(1,2),sum))
  #����
  count_mean_list=lapply(seq(1,12), function(x) apply(tel_array[,,pick_up_mat_for_month_follow_dim3[,x]],c(1,2),mean))
  proc.time()-ptm 
  
  #�N���Ƶ��G�Ʀ�����v���x�}���ˤl
  #mat_of_countz=matrix(countz_of_tel_mat,nrow=60)
  
  #�p�⵲�G�t�s��csv
  #write.csv(mat_of_countz,"mat_of_countz.csv")
  
  #�u��G���w��ԭ� ���w�s��Ѽ� �^�ǵo�ͦ���end----
  
  #�s�ɡG�HRData�榡 ��K�����Ҧ���Ū��----
  save(count_mean_month_list,file=paste(fn_condition,"count_mean_month_list.RData",sep=""))
  #�s�ɡG�HRData�榡 ��K�����Ҧ���Ū�� end----
  
  #�s�ɡG�H�j���Xcsv��----
  da_for_csv=count_month_list
  for(ci in 1:length(da_for_csv)){
    fn=paste(fn_condition,"count_mean_month_",ci,".csv",sep="");fn
    write.csv(da_for_csv[[ci]],fn)
  }
  
  #�s�ɡG�H�j���Xcsv�� end----
  print(proc.time()-total_ptm)
}




#20201103 ��Ҧ��p���ԭ�----

#Ū����ƧX�����ɮצW�� �ÿ���ҭn������
form_RData=list.files("I:/1RSLABdata/�Q���ƥ�/NCfile_OC",pattern=".RData")
form_RData
#RData�ƥ� mean/median* month/tenday ���|�زզX
length(form_RData) 
#�몺����
mean_pt=form_RData
mean_pt
length(mean_pt)
# #�몺����
# mean_month_pt=form_RData[sort(c(seq(1,length(form_RData)/4)*4-3,seq(1,length(form_RData)/4)*4-3+1))][seq(1,44,2)]
# mean_month_pt

#***���B�� tenday mean (��������)����

#�]�w��ԭ�mean/median �Ҧb���|
setwd("I:/1RSLABdata/�Q���ƥ�/NCfile_OC")

#�qRData Ū�imean/median �øm��list��
over_modelz=list()
#���ɦW
#dada=mean_tenday_pt
#���ɦW
dada=mean_pt
for(i in 1:length(dada)){
  load(dada[i])
  #������
  #over_modelz[[i]]=mean_tenday_list_v1
  #�륭��
  over_modelz[[i]]=count_month_list
}

#�N��Ҧ���ԭȪ���ƨ̷Ӧ�/�� �Ϥ����
dada=over_modelz
#�̤p��쬰60 81�x�}
form_by_month=list()
for(i in 1:length(dada[[1]])){  #loop for 36��
  s_arrary=array(NA,c(dim(dada[[1]][[1]]),length(dada)))
  for(i2 in 1:length(over_modelz)){ #loop for 13�Ҧ�
    s_arrary[,,i2] = over_modelz[[i2]][[i]]
  }
  form_by_month[[i]]=s_arrary
}

#�p��count ���Ҧ���mean
input_da=form_by_month
ptm=proc.time() #12�Ӥ�ɶ���90��
list_of_mean_over_countz=lapply(input_da, function(x) apply(x,c(1,2),mean))
proc.time()-ptm 

save(list_of_mean_over_countz,file="list_of_mean_over_countz.RData")

# #��Bø�s36�����v�� 
 lapply(list_of_mean_over_countz,image.plot)

# #��X�t�s��csv��
# da_for_csv=list_of_mean_over_meanz
# for(ci in 1:length(da_for_csv)){
#   fn=paste("mean_over_meanz_month_",ci,".csv",sep="");fn
#   write.csv(da_for_csv[[ci]],fn)
# }


#20201103 ��Ҧ��p���ԭ� end----





#�Ϥ���X----
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

#�N�v���x�}���ӪťճB�]��NA
dada=mat_of_countz
for(i in 1:nrow(grid_coord_of_na)){
  dada[grid_coord_of_na[i,1],grid_coord_of_na[i,2]]=NA
}

#�]�w�Ϥ��W�� �åH��ƥX��
fn="fig_test"
jpeg_export(dada,fn)

#�Ϥ���Xend----

#20201103 ��Ҧ��p���ԭ�----

#Ū����ƧX�����ɮצW�� �ÿ���ҭn������
form_RData=list.files("I:/1RSLABdata/�Q���ƥ�/NCfile_OC",pattern=".RData")
form_RData
#RData�ƥ� mean/median* month/tenday ���|�زզX
length(form_RData) 
#�몺����
mean_pt=form_RData[seq(1,length(form_RData),2)]
mean_pt
length(mean_pt)
# #�몺����
# mean_month_pt=form_RData[sort(c(seq(1,length(form_RData)/4)*4-3,seq(1,length(form_RData)/4)*4-3+1))][seq(1,44,2)]
# mean_month_pt

#***���B�� tenday mean (��������)����

#�]�w��ԭ�mean/median �Ҧb���|
setwd("I:/1RSLABdata/�Q���ƥ�/NCfile_OC")

#�qRData Ū�imean/median �øm��list��
over_modelz=list()
#���ɦW
#dada=mean_tenday_pt
#���ɦW
dada=mean_pt
for(i in 1:length(dada)){
  load(dada[i])
  #������
  #over_modelz[[i]]=mean_tenday_list_v1
  #�륭��
  over_modelz[[i]]=mean_month_list_v1
}

#�N��Ҧ���ԭȪ���ƨ̷Ӧ�/�� �Ϥ����
dada=over_modelz
#�̤p��쬰60 81�x�}
form_by_month=list()
for(i in 1:length(dada[[1]])){  #loop for 36��
  s_arrary=array(NA,c(dim(dada[[1]][[1]]),length(dada)))
  for(i2 in 1:length(over_modelz)){ #loop for 13�Ҧ�
    s_arrary[,,i2] = over_modelz[[i2]][[i]]
  }
  form_by_month[[i]]=s_arrary
}

#�p��mean ���Ҧ���mean
input_da=form_by_month
ptm=proc.time() #12�Ӥ�ɶ���90��
list_of_mean_over_meanz=lapply(input_da, function(x) apply(x,c(1,2),mean))
proc.time()-ptm 

save(list_of_mean_over_meanz,file="list_of_mean_over_meanz.RData")

#��Bø�s36�����v��
lapply(list_of_mean_over_meanz,image.plot)

# #��X�t�s��csv��
# da_for_csv=list_of_mean_over_meanz
# for(ci in 1:length(da_for_csv)){
#   fn=paste("mean_over_meanz_month_",ci,".csv",sep="");fn
#   write.csv(da_for_csv[[ci]],fn)
# }


#20201103 ��Ҧ��p���ԭ� end----



modelz_vector=list.files(cite_path)

for(i in 1:length(modelz_vector)){
  input_fn=list.files(paste(cite_path,modelz_vector[i],"/r1i1p1/",sep=""))
  if(length(input_fn)==0){
    print(c(i,"no file inside")) 
    next
    }
    fn_condition=gsub(".nc","_",input_fn);fn_condition
    #�ɮ׸��|
    input_path=paste(cite_path,modelz_vector[i],"/r1i1p1/",input_fn,sep="")
    ncfile=nc_open(input_path)

    #Ū�����nc�ɮ�----
    #Ū�J���
    ptm=proc.time() #��10��
    data=ncvar_get(ncfile)
    dim(data)
    proc.time()-ptm
    print(dim(data))
    print(c(i,dim(data)[3]/365))
    #Ū�����nc�ɮ�end----
}


# #20201118 �U�Z�ɶ��᪺�ˬd �t�@�ذ��k
# #�˵���Ƚw
# #���J�륭���C��
# load("I:/1RSLABdata/�Q���ƥ�/my_validation_box/tasmin_hist_base_mean/list_of_mean_over_meanz.RData")
# length(list_of_mean_over_meanz)
# dim(list_of_mean_over_meanz[[1]])

#20201120 ��z���X���
#�]�w��ƧX���|
#��s1
#oc_path="I:/1RSLABdata/�Q���ƥ�/my_validation_box/tasmin_hist_base_mean"
#��s2
#oc_path="I:/1RSLABdata/�Q���ƥ�/my_validation_box/pr_hist_base_80_count"
#��s3
#oc_path="I:/1RSLABdata/�Q���ƥ�/my_validation_box/tas_hist_base_mean"
#��s4
#oc_path="I:/1RSLABdata/�Q���ƥ�/my_validation_box/tasmax_hist_base_mean"
#��s5
oc_path="I:/1RSLABdata/�Q���ƥ�/my_validation_box/pr_hist_base_mean"



#���Jlist �s��12�Ӥ�p�⵲�G
load(paste(oc_path,"/",list.files(oc_path,pattern="list_of"),sep=""))
length(list_of_mean_over_meanz)
sapply(list_of_mean_over_meanz,dim)

sapply(list_of_mean_over_meanz,function(x) range(x,na.rm = T))


#�Ϥ��ɮצW�٩M���|��ƧX�ۦP
fig_name=tail(strsplit(oc_path,"/")[[1]],1)

#�]color bar�ƭȽd��
#bks=seq(50,1050,50)
bks=seq(-2,40,2)
#bks=seq(-1,10)
colz=tim.colors(length(bks)-1)

#����Y�g
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

#��X��CSV�ɮ�----
dada=list_of_mean_over_meanz
da_for_csv=c()
for(mi in 1:12){
  sda=dada[[mi]]
  da_for_csv=cbind(da_for_csv,as.vector(sda))

}
#�NNA�H-99.9���N
da_for_csv[is.na(da_for_csv)]=-99.9


csv_format=read.csv("C:/Users/rslab/BU/2018NJ/sideProject/AR5_pr_daily/pr-rcp26/BNU-ESM/BNU-ESM_pr_2006.csv",header = T)
for_csv_format=cbind(csv_format[,3:4],da_for_csv)
write.table(for_csv_format,paste(fig_name,".csv",sep=""),row.names=F,sep=",",  col.names=FALSE)
#��X��CSV�ɮ� end----
