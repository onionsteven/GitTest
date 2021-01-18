library(ncdf4) #Ū��nc��
library(fields) #ø�simage��

#���ԭz�G���ɮ׬�sideProject�u�㶰�X�����H�U�u��G
#1.�ѬD��x�}�p�� ��ԭ� �Y�خɴ� �� ����
#2.�ѬD��x�}�p�� ��ԭ� �j��Τp�󵹩w�� �� ����
#3.�D��x�}���;�

#��X���|
setwd("C:/Users/rslab/Documents/sideProExport/")

###���p����ɶ� 
total_ptm=proc.time()

#Ū�����nc�ɮ�----

#�ɮ׸��|
#input_path="I:/1RSLABdata/�Q���ƥ�/CMIP5����NC��/pr_BCSD_historical/ACCESS1-0/r1i1p1/pr_BCSD_ACCESS1-0.nc"
input_path="I:/1RSLABdata/�Q���ƥ�/CMIP5����NC��/pr_BCSD_rcp26/bcc-csm1-1/r1i1p1/pr_BCSD_bcc-csm1-1.nc"
ncfile=nc_open(input_path)
#ncfile=nc_open("C:/Users/rslab/Downloads/AR5_�έp_��_V2 -rcp85/AR5_�έp_��_V2/pr_BCSD_rcp85/ACCESS1-3/r1i1p1/pr_BCSD_ACCESS1-3.nc")

#Ū�J���
ptm=proc.time() #��10��
data=ncvar_get(ncfile)
dim(data)
proc.time()-ptm

#Ū�����nc�ɮ�end----


#20201029 �ǥѿz��x�} �N95�~���P�@�Ӧ�/�� �i��p��

#�ھڸ�Ʀ~�Ƥ��P �������P���z��x�}----
#Ū�J�몺�z��x�}
load("C:/Users/rslab/BU/2018NJ/sideProject/sideProjectWorkPlace/pick_up_mat_for_month.RData")
#Ū�J�����z��x�}
load("C:/Users/rslab/BU/2018NJ/sideProject/sideProjectWorkPlace/pick_up_mat_for_tenday.RData")

#Ū�J360�M�� �몺�z��x�}
load("C:/Users/rslab/BU/2018NJ/sideProject/sideProjectWorkPlace/pick_up_mat_for_month_for_360.RData")
#Ū�J360�M�� �����z��x�}
load("C:/Users/rslab/BU/2018NJ/sideProject/sideProjectWorkPlace/pick_up_mat_for_tenday_for_360.RData")



#�ھڸ�Ʀ~�Ƥ��P �������P���z��x�} end----

#Ū�J�몺�z��x�}
load("C:/Users/rslab/BU/2018NJ/sideProject/sideProjectWorkPlace/pick_up_mat_for_month.RData")
#�ǥѿz��x�}���θ�ƥX�Ӱ��p��
#�p�� �U�� �����
ptm=proc.time() #12�Ӥ�ɶ���90��
median_month_list_v1=lapply(seq(1,12), function(x) apply(data[,,pick_up_mat_for_month[,x]],c(1,2),median))
proc.time()-ptm 

#�p�� �U�� ����
ptm=proc.time() #12�Ӥ�ɶ���90��
mean_month_list_v1=lapply(seq(1,12), function(x) apply(data[,,pick_up_mat_for_month[,x]],c(1,2),mean))
proc.time()-ptm 

#Ū�J�����z��x�}
load("C:/Users/rslab/BU/2018NJ/sideProject/sideProjectWorkPlace/pick_up_mat_for_tenday.RData")
#�ǥѿz��x�}���θ�ƥX�Ӱ��p��
#�p�� �U�� �����
ptm=proc.time() #36�Ӧ��ɶ���168��
median_tenday_list_v1=lapply(seq(1,36), function(x) apply(data[,,pick_up_mat_for_tenday[,x]],c(1,2),median))
proc.time()-ptm 

#�p�� �U�� ����
ptm=proc.time() #36�Ӧ��ɶ���168��
mean_tenday_list_v1=lapply(seq(1,36), function(x) apply(data[,,pick_up_mat_for_tenday[,x]],c(1,2),mean))
proc.time()-ptm

#�s�ɡG�HRData�榡 ��K�����Ҧ���Ū��----
save(median_month_list_v1,file="median_month_list_v1.RData")
save(mean_month_list_v1,file="mean_month_list_v1.RData")

save(median_tenday_list_v1,file="median_tenday_list_v1.RData")
save(mean_tenday_list_v1,file="mean_tenday_list_v1.RData")

#�s�ɡG�HRData�榡 ��K�����Ҧ���Ū�� end----

#�s�ɡG�H�j���Xcsv��----
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
#�s�ɡG�H�j���Xcsv�� end----

#�α��G�Nlist�����\��array���榡 �H�K���򰵮�ԭ�/�s��Ѽƿz��
dada=median_tenday_list_v1
sda=dada[[1]]
array_form=array(numeric(),c(dim(sda),length(dada)))
for(pi in 1:length(dada)){
  array_form[,,pi]=dada[[pi]]
}

#20201030 list to array
da_to_array=median_tenday_list_v1
array_form_v1=array(as.numeric(unlist(da_to_array)), dim=c(dim(da_to_array[[1]]),length(da_to_array)))

#�u��G���w��ԭ� ���w�s��Ѽ� �^�ǵo�ͦ���----
#���w�G��ԭ�/�s��Ѽ� 
#�D���G�ŦX����
#�ѼƳ]�w----
#���w�Y��
given_value=3
#�]�w�j�󵹩w��
#�ƭ�array���T/F array
#�]�w�j�� �p��
tel_array=array_form >= given_value 
#�]�w�s��Ѽ�
continu_number = 2
#�ѼƳ]�wend----

#�̷ӲĤT���� �C�X�� 
tel_by_dim_3rd=apply(tel_array,c(3),function(x) x) 
#�p��T/F ���t�� �è̦���ŦX��ԭȪ��_����m
diff_of_dim_3rd = tel_by_dim_3rd[,-1] - tel_by_dim_3rd[,-ncol(tel_by_dim_3rd)]

#��ơG�ǥѲĤT���ת�diff�x�} ��X"�s��Ѽ�" �� "����"
happen_countz_function=function(x,continu_number){
  
  quali_value_show_up=which(x==1) #�ŦX�ȥX�{
  quali_value_ended=which(x==-1) #�ŦX�Ȧb�e�@�Ӧ�m�X�{
  
  #�X�{�P�����O�_�����X�{ �Y�O�h���פ@��  [�Y���פ��@�˫h �Y���i��X���D]
  if(length(quali_value_show_up)!=length(quali_value_ended)){
    continuous_dayz=abs(quali_value_show_up[-length(quali_value_show_up)] - quali_value_ended) 
  } else {
    #�s��Ѽ�
    continuous_dayz=abs(quali_value_show_up - quali_value_ended )
  }

  #�o�ͦ���
  oc=sum(continuous_dayz==continu_number)
  
  return(oc)
} 

ptm=proc.time() #��156��
countz_of_tel_mat=apply(diff_of_dim_3rd,1,function(x) happen_countz_function(x,continu_number = continu_number))
proc.time()-ptm

#�N���Ƶ��G�Ʀ�����v���x�}���ˤl
mat_of_countz=matrix(countz_of_tel_mat,nrow=60)

#�p�⵲�G�t�s��csv
write.csv(mat_of_countz,"mat_of_countz.csv")

#�u��G���w��ԭ� ���w�s��Ѽ� �^�ǵo�ͦ���end----


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

proc.time()-total_ptm


#20201103 ��Ҧ��p���ԭ�----

#Ū����ƧX�����ɮצW�� �ÿ���ҭn������
form_RData=list.files("I:/1RSLABdata/�Q���ƥ�/NCfile_OC",pattern=".RData")
form_RData
#RData�ƥ� mean/median* month/tenday ���|�زզX
length(form_RData) 
#����
mean_pt=form_RData[sort(c(seq(1,length(form_RData)/4)*4-3,seq(1,length(form_RData)/4)*4-3+1))]
mean_pt
#�몺����
mean_month_pt=form_RData[sort(c(seq(1,length(form_RData)/4)*4-3,seq(1,length(form_RData)/4)*4-3+1))][seq(1,44,2)]
mean_month_pt
#��������
mean_tenday_pt=form_RData[sort(c(seq(1,length(form_RData)/4)*4-3,seq(1,length(form_RData)/4)*4-3+1))][seq(2,44,2)]
mean_tenday_pt
#***���B�� tenday mean (��������)����

#�]�w��ԭ�mean/median �Ҧb���|
setwd("I:/1RSLABdata/�Q���ƥ�/NCfile_OC")

#�qRData Ū�imean/median �øm��list��
over_modelz=list()
#���ɦW
#dada=mean_tenday_pt
#���ɦW
dada=mean_month_pt
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
form_by_tenday=list()
for(i in 1:length(dada[[1]])){  #loop for 36��
  s_arrary=array(NA,c(dim(dada[[1]][[1]]),length(dada)))
  for(i2 in 1:length(over_modelz)){ #loop for 13�Ҧ�
    s_arrary[,,i2] = over_modelz[[i2]][[i]]
  }
  form_by_tenday[[i]]=s_arrary
}

#�p��mean ���Ҧ���mean
input_da=form_by_tenday
ptm=proc.time() #12�Ӥ�ɶ���90��
list_of_mean_over_meanz=lapply(input_da, function(x) apply(x,c(1,2),mean))
proc.time()-ptm 

#��Bø�s36�����v�� 
lapply(list_of_mean_over_meanz,image.plot)

#��X�t�s��csv��
da_for_csv=list_of_mean_over_meanz
for(ci in 1:length(da_for_csv)){
  fn=paste("mean_over_meanz_month_",ci,".csv",sep="");fn
  write.csv(da_for_csv[[ci]],fn)
}


#20201103 ��Ҧ��p���ԭ� end----


# 20201203 �D��x�}���;����˵��u�� ---------------------------------------------------

#��ơG�D��x�}���;�
#regular_dayz_in_yr= T �� F
#period_to_cal�� "mon" / "tenday" ���

pick_up_mat_generator=function(regular_dayz_in_yr=T,period_to_cal="mon",data_over_yrz=30){
  
  #regular_dayz_in_yr == T �h�C�~��Ƭ�365 ��L�h�O360
  if(regular_dayz_in_yr==T){
    #�C�Ӥ몺�Ѽ�
    dayz_in_yr=c(31,28,31,30,31,30,31,31,30,31,30,31)
    #�@�~��ƴ���
    print(c("�@�~���",sum(dayz_in_yr)))
  }else{
    #�C�Ӥ몺�Ѽ� ***** �ĤG�جO�C�Ӥ��30��
    dayz_in_yr=rep(30,12)
    #�@�~��ƴ���
    print(c("�@�~���",sum(dayz_in_yr)))
  }
  
  #���R���G��----
  #����Ʈɪ���
  month_tail_in_yr=cumsum(dayz_in_yr)
  month_tail_in_yr
  #����Ʈɪ��Y
  month_head_in_yr=month_tail_in_yr-dayz_in_yr+1
  month_head_in_yr
  
  #���R���G��----
  #�����_�l���z----
  #�W���_�l��
  month_head_in_yr
  #�����_�l��
  month_head_in_yr+10
  #�U���_�l��
  month_head_in_yr+20
  
  #�@�~�������_�l��
  tenday_head_in_yr=as.vector(rbind(month_head_in_yr,month_head_in_yr+10,month_head_in_yr+20))
  #�̷Ӧ����� �@�~���h�֦�
  length(tenday_head_in_yr)
  
  #�����������z----
  #�W��������
  month_head_in_yr+9
  #����������
  month_head_in_yr+19
  #�U��������
  month_tail_in_yr=cumsum(dayz_in_yr)
  month_tail_in_yr
  
  #�@�~������������
  tenday_tail_in_yr=as.vector(rbind(month_head_in_yr+9,month_head_in_yr+19,month_tail_in_yr))
  
  #�̷Ӥ릯 ���ͤ��P�������Y��
  if(period_to_cal=="mon"){
    
    cutting_head=month_head_in_yr
    cutting_tail=month_tail_in_yr
    
    #�ɴ��ƴ���
    print(c("�ɴ���",length(cutting_head)))
  }else if(period_to_cal=="tenday"){
    
    cutting_head=tenday_head_in_yr
    cutting_tail=tenday_tail_in_yr
    
    #�ɴ��ƴ���
    print(c("�ɴ���",length(cutting_head)))
  }else{return("out of setting")}
  
  #��ƪ��~�� ***** �ĤG�جO�̷ӭn���R����Ʀ~�Ƥ��P����
  #data_over_yrz=30
  yrz=seq(1,data_over_yrz)
  
  #���R�~�ƴ���
  print(c("���R�~��",length(yrz)))
  
  #�]�m�@�ӿz��x�}(T/F) �C��column�O�ĤT���׭n��X�Ӫ����� row�O����/��� (36/12)
  pick_up_mat=c()
  for(pi in 1:length(cutting_head)){
    
    head_num = sum(dayz_in_yr) * yrz - sum(dayz_in_yr) + cutting_head[pi]
    tail_num = sum(dayz_in_yr) * yrz - sum(dayz_in_yr) + cutting_tail[pi]
    #F�V�q���שM�ĤT���פ@�P
    #length_of_dim_3=dim(data)[3] ###<-------------------�V�q���׳]�w
    length_of_dim_3= sum(dayz_in_yr) * length(yrz)  ###<-------------------�V�q���׳]�w #�@�~�Ѽ�*��Ʀ~��
    #�]�m�@F�V�q �P�ĤT���׵��� �N�Q�^����m�ФWT
    long_F = rep(F,length_of_dim_3)
    for(ri in 1:length(head_num)){
      long_F[head_num[ri]:tail_num[ri]] = T
    }
    pick_up_mat = cbind(pick_up_mat,long_F)
  }
  return(pick_up_mat)
}

#�ϥ�
wtd_pick_up_mat=pick_up_mat_generator(regular_dayz_in_yr=T,period_to_cal="tenday",data_over_yrz=10)

pick_up_mat_for_month_dayz365_yrz10=pick_up_mat_generator(regular_dayz_in_yr=T,period_to_cal="mon",data_over_yrz=10)
pick_up_mat_for_month_dayz360_yrz10=pick_up_mat_generator(regular_dayz_in_yr=F,period_to_cal="mon",data_over_yrz=10)

#�D��x�}�R�W �åB�t�s

save(pick_up_mat_for_month_dayz365_yrz10,file="pick_up_mat_for_month_dayz365_yrz10.RData")

save(pick_up_mat_for_month_dayz360_yrz10,file="pick_up_mat_for_month_dayz360_yrz10.RData")

# �ˬd�D��x�}(��) ---------------------------------------------------------------

#�T�{�z��x�}������
dim(wtd_pick_up_mat)
365*30
360*30

#�v���T�{
bmp("pick_up_mat_image_check.bmp",width = 480*5,height = 480*5)
image.plot(wtd_pick_up_mat)
dev.off()

#�˵��U�ɴ����Ѽ�
apply(wtd_pick_up_mat,2,sum)

#�˵��D��x�}�g�����p
plot(wtd_pick_up_mat[1:(365*3),12],ty="l",xlab="�ɶ��b",ylab="0/1���O�_���")
#�˵������D��x�}
wtd_pick_up_mat[1:70,1:5]

