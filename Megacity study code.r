library(dplyr)
library(foreign)
library(coin)
library(survival)
library(survminer)
library(eoffice) 
library(ggplot2)
library(polycor)
library(table1)

#import data----
longit00<-read.csv("Z:/megacity project/CLHLS data/cov_longit00_1st.csv",header=T)
longit02<-read.csv("Z:/megacity project/CLHLS data/cov_longit02_1st.csv",header=T)
longit05<-read.csv("Z:/megacity project/CLHLS data/cov_longit05_1st.csv",header=T)
longit08<-read.csv("Z:/megacity project/CLHLS data/cov_longit08_1st.csv",header=T)
longit11<-read.csv("Z:/megacity project/CLHLS data/cov_longit11_1st.csv",header=T)

ndvi1250no<-read.csv("Z:/megacity project/individual NDVI/mega_ndvi_1250_nocloud.csv",header=T,encoding="UTF-8")
o32<-read.csv("Z:/megacity project/mega_o3_id_source2_khd.csv",header=T)
no2<-read.csv("Z:/megacity project/mega_no2_id.csv",header=T)
no2<-as.data.frame(lapply(no2,as.numeric))
pm25<-read.csv("Z:/megacity project/mega_pm25_id.csv",header=T)

survival<-read.csv("Z:/megacity project/CLHLS data/surv_longit9814_18_1st.csv",header=T)
survival$survTime_month<-as.numeric(as.character(survival$survTime_month))
survival$event<-as.numeric(as.character(survival$event))

income00<-read.csv("Z:/megacity project/CLHLS data/income全/clhls2000_income.csv",header=T)
income02<-read.csv("Z:/megacity project/CLHLS data/income全/clhls2002_income.csv",header=T)
income05<-read.csv("Z:/megacity project/CLHLS data/income全/clhls2005_income.csv",header=T)
income08<-read.csv("Z:/megacity project/CLHLS data/income全/clhls2008_income.csv",header=T)
income11<-read.csv("Z:/megacity project/CLHLS data/income全/clhls2011_income.csv",header=T)
income<-bind_rows(income00,income02,income05,income08,income11)
income<-income%>%distinct(id,.keep_all=T)

canyin<-read.csv("C:/Users/65700/Box/Megacity study/POI data-individual/餐饮服务.csv",header=T,encoding="UTF-8")
daolu<-read.csv("C:/Users/65700/Box/Megacity study/POI data-individual/道路附属设施.csv",header=T,encoding="UTF-8")
fengjing<-read.csv("C:/Users/65700/Box/Megacity study/POI data-individual/风景名胜.csv",header=T,encoding="UTF-8")
gongshe<-read.csv("C:/Users/65700/Box/Megacity study/POI data-individual/公共设施.csv",header=T,encoding="UTF-8")
gongsi<-read.csv("C:/Users/65700/Box/Megacity study/POI data-individual/公司企业.csv",header=T,encoding="UTF-8")
gouwu<-read.csv("C:/Users/65700/Box/Megacity study/POI data-individual/购物服务.csv",header=T,encoding="UTF-8")
jiaotong<-read.csv("C:/Users/65700/Box/Megacity study/POI data-individual/交通设施服务.csv",header=T,encoding="UTF-8")
kejiao<-read.csv("C:/Users/65700/Box/Megacity study/POI data-individual/科教文化服务.csv",header=T,encoding="UTF-8")
zhuzhai<-read.csv("C:/Users/65700/Box/Megacity study/POI data-individual/商务住宅.csv",header=T,encoding="UTF-8")
fuwu<-read.csv("C:/Users/65700/Box/Megacity study/POI data-individual/生活服务.csv",header=T,encoding="UTF-8")
shinei<-read.csv("C:/Users/65700/Box/Megacity study/POI data-individual/室内设施.csv",header=T,encoding="UTF-8")
tiyu<-read.csv("C:/Users/65700/Box/Megacity study/POI data-individual/体育休闲服务.csv",header=T,encoding="UTF-8")
yiliao<-read.csv("C:/Users/65700/Box/Megacity study/POI data-individual/医疗保健服务.csv",header=T,encoding="UTF-8")
sanjia<-read.csv("C:/Users/65700/Box/Megacity study/POI data-individual/三级甲等医院.csv",header=T,encoding="UTF-8")

ring<-read.csv("C:/Users/65700/Box/Megacity study/mega_id_ring.csv",header=T,encoding="UTF-8")%>%select(id,ring)

#merge data----
data<-bind_rows(longit00,longit02,longit05,longit08,longit11)
data<-data%>%full_join(ndvi1250no,by="id")
data<-data%>%filter(city=="北京市"|city=="上海市"|city=="天津市"|city=="重庆市"|city=="成都市"|city=="广州市")
data<-data%>%full_join(o32,by="id")%>%full_join(no2,by="id")%>%full_join(pm25,by="id")
data<-data%>%full_join(survival,by="id")    %>%filter(city=="北京市"|city=="上海市"|city=="天津市"|city=="重庆市"|city=="成都市"|city=="广州市")
data<-data%>%full_join(income,by="id")%>%filter(city=="北京市"|city=="上海市"|city=="天津市"|city=="重庆市"|city=="成都市"|city=="广州市")
data<-data%>%full_join(ring,by="id")   
data$endYear_2<-as.numeric(as.character(data$endYear_2))
data$span<-data$endYear_2-data$baselineYear
data <- data%>%mutate(innercity=ifelse(district=="东城区"|district=="西城区"|district=="朝阳区"|district=="海淀区"|district=="丰台区"|district=="石景山区"|district=="和平区"|district=="河西区"|district=="南开区"|district=="河东区"|district=="红桥区"|district=="河北区"|district=="黄浦区"|district=="徐汇区"|district=="长宁区"|district=="静安区"|district=="普陀区"|district=="虹口区"|district=="杨浦区"|district=="锦江区"|district=="青羊区"|district=="金牛区"|district=="武侯区"|district=="成华区"|district=="新都区"|district=="郫都区"|district=="温江区"|district=="双流区"|district=="龙泉驿区"|district=="青白江区"|district=="新津区"|district=="都江堰市"|district=="渝中区"|district=="大渡口区"|district=="江北区"|district=="沙坪坝区"|district=="九龙坡区"|district=="南岸区"|district=="北碚区"|district=="渝北区"|district=="巴南区"|district=="越秀区"|district=="荔湾区"|district=="天河区"|district=="海珠区"|district=="黄埔区"|district=="白云区", "inner", "suburb"))
data$innercity<-as.factor(as.character(data$innercity))
data$city<-as.factor(as.character(data$city))

data<-data%>%full_join(canyin,by="id")%>%full_join(daolu,by="id")%>%full_join(fengjing,by="id")%>%full_join(gongshe,by="id")%>%full_join(gongsi,by="id")%>%full_join(gouwu,by="id")%>%full_join(jiaotong,by="id")%>%full_join(kejiao,by="id")%>%full_join(zhuzhai,by="id")%>%full_join(fuwu,by="id")%>%full_join(shinei,by="id")%>%full_join(tiyu,by="id")%>%full_join(yiliao,by="id")
data<-data%>%full_join(sanjia,by="id")
data$POI1km<-(data$count_1km_交通设施服务+data$count_1km_体育休闲服务+data$count_1km_公共设施+data$count_1km_公司企业+data$count_1km_医疗保健服务+data$count_1km_商务住宅+data$count_1km_室内设施+data$count_1km_生活服务+data$count_1km_科教文化服务+data$count_1km_购物服务+data$count_1km_道路附属设施+data$count_1km_风景名胜+data$count_1km_餐饮服务)/5
data$POI5km<-data$count_5km_交通设施服务+data$count_5km_体育休闲服务+data$count_5km_公共设施+data$count_5km_公司企业+data$count_5km_医疗保健服务+data$count_5km_商务住宅+data$count_5km_室内设施+data$count_5km_生活服务+data$count_5km_科教文化服务+data$count_5km_购物服务+data$count_5km_道路附属设施+data$count_5km_风景名胜+data$count_5km_餐饮服务
POImin<-vector()
for(i in 1:4992){
  POImin=c(POImin,min(data[i,c(231,234,237,240,243,246,249,252,255,258,261,264,267)]))
}
data<-cbind(data,POImin)

#NDVI and air pollutants----
##Cumulative NDVI
ndviyearly <- c("NDVI_1250_yearly_2000","NDVI_1250_yearly_2001","NDVI_1250_yearly_2002","NDVI_1250_yearly_2003","NDVI_1250_yearly_2004","NDVI_1250_yearly_2005","NDVI_1250_yearly_2006","NDVI_1250_yearly_2007", "NDVI_1250_yearly_2008", "NDVI_1250_yearly_2009","NDVI_1250_yearly_2010","NDVI_1250_yearly_2011","NDVI_1250_yearly_2012","NDVI_1250_yearly_2013","NDVI_1250_yearly_2014","NDVI_1250__yearly_2015","NDVI_1250_yearly_2016","NDVI_1250_yearly_2017","NDVI_1250_yearly_2018","NDVI_1250_yearly_2019")
for (ndvitime in ndviyearly)
{
  data[,ndvitime]<-ifelse(substr(ndvitime,18,21)>= data$baselineYear & substr(ndvitime,18,21)<=data$endYear_2,
                                data[,ndvitime],NA)
} 
data$NDVI_cum_yearly <- rowMeans(data[,ndviyearly],na.rm = T)*10

##last-year no2
no2yearly<-c("X1990_final_1km","X1995_final_1km","X2000_final_1km","X2005_final_1km","X2006_final_1km","X2007_final_1km","X2008_final_1km","X2009_final_1km","X2010_final_1km","X2011_final_1km","X2012_final_1km","X2013_final_1km","X2014_final_1km","X2015_final_1km","X2016_final_1km","X2017_final_1km","X2018_final_1km","X2019_final_1km")
for (no2time in no2yearly)
{
  data[,no2time]<-ifelse(substr(no2time,2,5)==data$endYear_2,data[,no2time],ifelse(substr(data$endYear_2,2,5)==2001|2002,data[,"X2000_final_1km"],ifelse(substr(data$endYear_2,2,5)==2003|2004,data[,"X2005_final_1km"],NA)))
} 
data$lastyrno2 <- rowMeans(data[,no2yearly],na.rm = T)/10

##last-year pm25
pm25yearly<-c("PM_1998","PM_1999","PM_2000","PM_2001","PM_2002","PM_2003","PM_2004","PM_2005","PM_2006","PM_2007","PM_2008","PM_2009","PM_2010","PM_2011","PM_2012","PM_2013","PM_2014","PM_2015","PM_2016","PM_2017","PM_2018","PM_2019","PM_2020")
for (pm25time in pm25yearly)
{
  data[,pm25time]<-ifelse(substr(pm25time,4,7)==data$endYear_2,
                                data[,pm25time],NA)
} 
data$lastyrpm25 <- rowMeans(data[,pm25yearly],na.rm = T)/10

##last-year o3
o3yearly<-c("O3_2005","O3_2006","O3_2007","O3_2008","O3_2009","O3_2010","O3_2011","O3_2012","O3_2013","O3_2014","O3_2015","O3_2016","O3_2017","O3_2018","O3_2019","O3_2020")
for (o3time in o3yearly)
{
  data[,o3time]<-ifelse(substr(o3time,4,7)==data$endYear_2,data[,o3time],ifelse(substr(o3time,4,7)==2000|2001|2002|2003|2004,data[,"O3_2005"],NA))
} 
data$lastyro3 <- rowMeans(data[,o3yearly],na.rm = T)/10

##NDVI change
ndvi1250no<-ndvi1250no%>%filter(data$span>1)
ndvi_yearly <- ndvi1250no[,c("id","NDVI_1250_yearly_2000","NDVI_1250_yearly_2001","NDVI_1250_yearly_2002","NDVI_1250_yearly_2003","NDVI_1250_yearly_2004","NDVI_1250_yearly_2005","NDVI_1250_yearly_2006","NDVI_1250_yearly_2007", "NDVI_1250_yearly_2008", "NDVI_1250_yearly_2009", "NDVI_1250_yearly_2010","NDVI_1250_yearly_2011","NDVI_1250_yearly_2012","NDVI_1250_yearly_2013", "NDVI_1250_yearly_2014", "NDVI_1250_yearly_2015","NDVI_1250_yearly_2016","NDVI_1250_yearly_2017","NDVI_1250_yearly_2018","NDVI_1250_yearly_2019")]
response.mat <- data.frame(t(ndvi_yearly[,2:21]))
response.mat$time <- c(1:20)
reg <- lapply(response.mat[,c(1:2976)], function(y) {
  coef <- data.frame(matrix(nrow=1,ncol=2))
  coef$X1 <- summary(lm(y~time,data=response.mat))$coefficients[2,"Estimate"]
  coef$X2 <- summary(lm(y~time,data=response.mat))$coefficients[2,"Pr(>|t|)"]
  return(coef)
})
reg_2 <- bind_rows(reg)

ndvi_yearly <- cbind(ndvi_yearly, reg_2)
ndvi_yearly$NDVI_yearly_change <- ifelse(ndvi_yearly$X2>=0.05|ndvi_yearly$X1==0,1,
                                    ifelse(ndvi_yearly$X2<0.05 & ndvi_yearly$X1>0,2,
                                          ifelse(ndvi_yearly$X2<0.05 &ndvi_yearly$X1<0,3,NA)))
ndvi_yearly<-ndvi_yearly%>%select(id,NDVI_yearly_change)
data<-data%>%full_join(ndvi_yearly,by="id")
data$NDVI_yearly_change<-as.factor(as.character(data$NDVI_yearly_change))
summary(data$NDVI_yearly_change)

##pm25 change
pm25<-pm25%>%filter(data$span>1)
pm25<-pm25[,c("id","PM_1998","PM_1999","PM_2000","PM_2001","PM_2002","PM_2003","PM_2004","PM_2005","PM_2006","PM_2007","PM_2008","PM_2009","PM_2010","PM_2011","PM_2012","PM_2013","PM_2014","PM_2015","PM_2016","PM_2017","PM_2018","PM_2019","PM_2020")]
response.mat <- data.frame(t(pm25[,2:24]))
response.mat$time <- c(1:23)
reg <- lapply(response.mat[,c(1:2976)], function(y) {
  coef <- data.frame(matrix(nrow=1,ncol=2))
  coef$X1 <- summary(lm(y~time,data=response.mat))$coefficients[2,"Estimate"]
  coef$X2 <- summary(lm(y~time,data=response.mat))$coefficients[2,"Pr(>|t|)"]
  return(coef)
})
reg_2 <- bind_rows(reg)

pm25 <- cbind(pm25, reg_2)
pm25$pm25_change <- ifelse(pm25$X2>=0.05|pm25$X1==0,1,
                                    ifelse(pm25$X2<0.05 & pm25$X1>0,2,
                                           ifelse(pm25$X2<0.05 &pm25$X1<0,3,NA))) 
pm25c<-pm25%>%select(id,pm25_change)
data<-data%>%full_join(pm25c,by="id")
data$pm25_change<-as.factor(as.character(data$pm25_change))
summary(data$pm25_change)

###pm25 inverse U
pm25<-pm25[,c("id","PM_1998","PM_1999","PM_2000","PM_2001","PM_2002","PM_2003","PM_2004","PM_2005","PM_2006","PM_2007","PM_2008","PM_2009","PM_2010","PM_2011","PM_2012","PM_2013","PM_2014","PM_2015","PM_2016","PM_2017","PM_2018","PM_2019","PM_2020")]
response.mat <- data.frame(t(pm25[,2:24]))
response.mat$time <- c(1:23)
response.mat$time2 <- (response.mat$time)^2
reg <- lapply(response.mat[,c(1:2976)], function(y) {
  coef <- data.frame(matrix(nrow=1,ncol=4))
  coef$X1 <- summary(lm(y~time+time2,data=response.mat))$coefficients[2,"Estimate"]
  coef$X2 <- summary(lm(y~time+time2,data=response.mat))$coefficients[2,"Pr(>|t|)"]
  coef$X3 <- summary(lm(y~time+time2,data=response.mat))$coefficients[3,"Estimate"]
  coef$X4 <- summary(lm(y~time+time2,data=response.mat))$coefficients[3,"Pr(>|t|)"]
  return(coef)
})
reg_2 <- bind_rows(reg)
summary(reg_2)
pm25 <- cbind(pm25, reg_2)
pm25$pm25_change <- ifelse(pm25$X2<0.05&pm25$X1>0&pm25$X4<0.05&pm25$X3<0,1,2)
summary(pm25$pm25_change)

##o3 change
o3<-o32%>%filter(data$endYear_2>=2006&data$span>1)
response.mat <- data.frame(t(o3[,2:17]))
response.mat$time <- c(1:16)
reg <- lapply(response.mat[,c(1:2234)], function(y) {
  coef <- data.frame(matrix(nrow=1,ncol=2))
  coef$X1 <- summary(lm(y~time,data=response.mat))$coefficients[2,"Estimate"]
  coef$X2 <- summary(lm(y~time,data=response.mat))$coefficients[2,"Pr(>|t|)"]
  return(coef)
})
reg_2 <- bind_rows(reg)

o3<- cbind(o3, reg_2)
o3$o3_change <- ifelse(o3$X2>=0.05|o3$X1==0,1,
                                    ifelse(o3$X2<0.05 & o3$X1>0,2,
                                           ifelse(o3$X2<0.05 &o3$X1<0,3,NA)))
o3<-o3%>%select(id,o3_change)
data<-data%>%full_join(o3,by="id")
data$o3_change<-as.factor(as.character(data$o3_change))
summary(data$o3_change)

##no2 change
no2<-no2%>%filter(data$span>1&(data$endYear_2>=2006|(data$baselineYear==2000&data$endYear_2==2005)))
no2<-no2%>%filter(no2$X1990_final_1km!="."&no2$X1995_final_1km!="."&no2$X2000_final_1km!="."&no2$X2005_final_1km!="."&no2$X2006_final_1km!="."&no2$X2007_final_1km!="."&no2$X2008_final_1km!="."&no2$X2009_final_1km!="."&no2$X2010_final_1km!="."&no2$X2011_final_1km!="."&no2$X2012_final_1km!="."&no2$X2013_final_1km!="."&no2$X2014_final_1km!="."&no2$X2015_final_1km!="."&no2$X2016_final_1km!="."&no2$X2017_final_1km!="."&no2$X2018_final_1km!="."&no2$X2019_final_1km!=".")
no2<-no2[,c("id","X1990_final_1km","X1995_final_1km","X2000_final_1km","X2005_final_1km","X2006_final_1km","X2007_final_1km","X2008_final_1km","X2009_final_1km","X2010_final_1km","X2011_final_1km","X2012_final_1km","X2013_final_1km","X2014_final_1km","X2015_final_1km","X2016_final_1km","X2017_final_1km","X2018_final_1km","X2019_final_1km")]
response.mat <- data.frame(t(no2[,2:19]))
response.mat$time <- c(1:18)
reg <- lapply(response.mat[,c(1:2252)], function(y) {
  coef <- data.frame(matrix(nrow=1,ncol=2))
  coef$X1 <- summary(lm(y~time,data=response.mat))$coefficients[2,"Estimate"]
  coef$X2 <- summary(lm(y~time,data=response.mat))$coefficients[2,"Pr(>|t|)"]
  return(coef)
})
reg_2 <- bind_rows(reg)
no2 <- cbind(no2, reg_2)
no2$no2_change <- ifelse(no2$X2>=0.05|no2$X1==0,1,
                                    ifelse(no2$X2<0.05 & no2$X1>0,2,
                                           ifelse(no2$X2<0.05 &no2$X1<0,3,NA))) 
no2c<-no2%>%select(id,no2_change)
data<-data%>%full_join(no2c,by="id")
data$no2_change<-as.factor(as.character(data$no2_change))
summary(data$no2_change)


#baseline characteristics----
data$agegroup<-ifelse(data$age<=79&data$age>=65,1,ifelse(data$age>=80&data$age<=89,2,ifelse(data$age>=90&data$age<=99,3,4)))
data$agegroup<-as.factor(as.character(data$agegroup))

data$f35<-as.numeric(as.character(data$f35))
data$f35_2<-as.numeric(as.character(data$f35_2))
data<-data%>%mutate(hincome=ifelse(data$baselineYear==2000,data$f35_2,data$f35))
data<-data%>%mutate(hhincome=ifelse(hincome<5000,1,ifelse(hincome>=5000&hincome<=15000,2,3)))
data$hhincome<-as.factor(as.character(data$hhincome))
data$hhincome<-addNA(data$hhincome)

table1(~agegroup+sex+ethnic++education+occupation+marriage+hhincome+innercity+smoking+alcohol+exercise|city,data=data)


#cox regression models----

##age and sex-adjusted models
data2<-data
data[,c("ethnic","marriage","exercise","alcohol","smoking","education_cat","occupation")]<-lapply(data[,c("ethnic","marriage","exercise","alcohol","smoking","education_cat","occupation")],as.numeric)
data<-data%>%filter(ethnic==3|ethnic==2)%>%filter(marriage==3|marriage==2)%>%filter(exercise==4|exercise==2|exercise==3)%>%filter(smoking==4|smoking==2|smoking==3)%>%filter(alcohol==4|alcohol==2|alcohol==3)%>%filter(education_cat==4|education_cat==2|education_cat==3)%>%filter(occupation==3|occupation==2)
data[,c("ethnic","marriage","exercise","alcohol","smoking","education_cat","occupation")]<-lapply(data[,c("ethnic","marriage","exercise","alcohol","smoking","education_cat","occupation")],as.factor)

###covariates
summary(coxph(Surv(survTime_month,event)~age+sex,data))
summary(coxph(Surv(survTime_month,event)~agegroup+sex,data))
summary(coxph(Surv(survTime_month,event)~age+sex+ethnic,data))
summary(coxph(Surv(survTime_month,event)~age+sex+education_cat,data))
summary(coxph(Surv(survTime_month,event)~age+sex+occupation,data))
summary(coxph(Surv(survTime_month,event)~age+sex+marriage,data))
summary(coxph(Surv(survTime_month,event)~age+sex+hincome,data))
summary(coxph(Surv(survTime_month,event)~age+sex+hhincome,data))
summary(coxph(Surv(survTime_month,event)~age+sex+innercity,data))
summary(coxph(Surv(survTime_month,event)~age+sex+city,data))
summary(coxph(Surv(survTime_month,event)~age+sex+smoking,data))
summary(coxph(Surv(survTime_month,event)~age+sex+alcohol,data))
summary(coxph(Surv(survTime_month,event)~age+sex+exercise,data))


###POI(continuous)
summary(coxph(Surv(survTime_month,event)~age+sex+POI1km,data))
summary(coxph(Surv(survTime_month,event)~age+sex+POI5km,data))
summary(coxph(Surv(survTime_month,event)~age+sex+POImin,data))
summary(coxph(Surv(survTime_month,event)~age+sex+count_1km_医疗保健服务,data))
summary(coxph(Surv(survTime_month,event)~age+sex+count_5km_医疗保健服务,data))
summary(coxph(Surv(survTime_month,event)~age+sex+distance_医疗保健服务,data))
summary(coxph(Surv(survTime_month,event)~age+sex+count_1km_体育休闲服务,data))
summary(coxph(Surv(survTime_month,event)~age+sex+count_5km_体育休闲服务,data))
summary(coxph(Surv(survTime_month,event)~age+sex+distance_体育休闲服务,data))
summary(coxph(Surv(survTime_month,event)~age+sex+count_1km_风景名胜,data))
summary(coxph(Surv(survTime_month,event)~age+sex+count_5km_风景名胜,data))
summary(coxph(Surv(survTime_month,event)~age+sex+distance_风景名胜,data))
summary(coxph(Surv(survTime_month,event)~age+sex+count_1km_三级甲等医院,data))
summary(coxph(Surv(survTime_month,event)~age+sex+count_5km_三级甲等医院,data))
summary(coxph(Surv(survTime_month,event)~age+sex+distance_三级甲等医院,data))

###POI by quartile
dat<-data%>%filter(data$survTime_month!="NA")
summary(dat$count_5km_医疗保健服务)
summary(dat$count_5km_体育休闲服务)
summary(dat$count_5km_风景名胜)
dat$医疗<-ifelse(dat$count_5km_医疗保健服务<=49&dat$count_5km_医疗保健服务>=0,1,ifelse(dat$count_5km_医疗保健服务>49&dat$count_5km_医疗保健服务<=588,2,ifelse(dat$count_5km_医疗保健服务>588&dat$count_5km_医疗保健服务<=2312,3,4)))
dat$医疗<-as.factor(as.character(dat$医疗))
table(dat$医疗)
dat$体育<-ifelse(dat$count_5km_体育休闲服务<=49&dat$count_5km_体育休闲服务>=0,1,ifelse(dat$count_5km_体育休闲服务>49&dat$count_5km_体育休闲服务<=422,2,ifelse(dat$count_5km_体育休闲服务>422&dat$count_5km_体育休闲服务<=2778,3,4)))
dat$体育<-as.factor(as.character(dat$体育))
table(dat$体育)
dat$风景<-ifelse(dat$count_5km_风景名胜<=18&dat$count_5km_风景名胜>=0,1,ifelse(dat$count_5km_风景名胜>18&dat$count_5km_风景名胜<=124,2,ifelse(dat$count_5km_风景名胜>124&dat$count_5km_风景名胜<=729,3,4)))
dat$风景<-as.factor(as.character(dat$风景))
table(dat$风景)
summary(coxph(Surv(survTime_month,event)~age+sex+医疗,dat))
summary(coxph(Surv(survTime_month,event)~age+sex+体育,dat))
summary(coxph(Surv(survTime_month,event)~age+sex+风景,dat))


###NDVI and air pollutants
summary(coxph(Surv(survTime_month,event)~age+sex+NDVI_cum_yearly,data))
summary(coxph(Surv(survTime_month,event)~age+sex+lastyrno2,data))
summary(coxph(Surv(survTime_month,event)~age+sex+lastyro3,data))
summary(coxph(Surv(survTime_month,event)~age+sex+lastyrpm25,data))
summary(coxph(Surv(survTime_month,event)~age+sex+NDVI_yearly_change,data))
summary(coxph(Surv(survTime_month,event)~age+sex+o3_change,data))
summary(coxph(Surv(survTime_month,event)~age+sex+no2_change,data))
summary(coxph(Surv(survTime_month,event)~age+sex+pm25_change,data))


###Ring roads
bj<-data2%>%filter(city=="北京市")
tj<-data2%>%filter(city=="天津市")
sh<-data2%>%filter(city=="上海市")
cq<-data2%>%filter(city=="重庆市")
cd<-data2%>%filter(city=="成都市")
gz<-data2%>%filter(city=="广州市")
####Beijing
bj$ring<-as.factor(as.character(bj$ring))
bj$ring<-factor(bj$ring,levels=c("Out of Ring 6","In Ring 2","Between Ring 2 and Ring 3","Between Ring 3 and Ring 4","Between Ring 4 and Ring 5","Between Ring 5 and Ring 6"))
summary(coxph(Surv(survTime_month,event)~age+sex+ring,bj))
anova(coxph(Surv(survTime_month,event)~age+sex+ring,bj))

####shanghai
sh$ring<-as.factor(as.character(sh$ring))
sh$ring<-factor(sh$ring,levels=c("Out of Ring 4","In Ring 1 (内环)","Between Ring 1 and Ring 2 (中环)","Between Ring 2 and Ring 3 (外环)","Between Ring 3 and Ring 4 (郊环)"))
summary(coxph(Surv(survTime_month,event)~age+sex+ring,sh))
anova(coxph(Surv(survTime_month,event)~age+sex+ring,sh))

####tianjin
tj$ring<-as.factor(as.character(tj$ring))
tj$ring<-factor(tj$ring,levels=c("Out of Ring 5","In Ring 1 (内环线)","Between Ring 1 and Ring 2 (中环线)","Between Ring 2 and Ring 3 (快速路)","Between Ring 3 and Ring 4 (外环线)","Between Ring 4 and Ring 5 (环城高速圈)"))
summary(coxph(Surv(survTime_month,event)~age+sex+ring,tj))
anova(coxph(Surv(survTime_month,event)~age+sex+ring,tj))

####Chengdu
cd$ring<-as.factor(as.character(cd$ring))
cd$ring<-factor(cd$ring=="In Ring 1 (一环路)"|cd$ring=="Between Ring 1 and Ring 2 (二环路)",1,ifelse(cd$ring=="Between Ring 2 and Ring 2.5 (中环路)"|cd$ring=="Between Ring 2.5 and Ring 3 (三环路)",2,ifelse(cd$ring=="Between Ring 3 and Ring 4 (四环路-绕城高速)"|cd$ring=="Between Ring 4 and Ring 5 (五环路)",3,4)))
summary(coxph(Surv(survTime_month,event)~age+sex+ring,cd))
anova(coxph(Surv(survTime_month,event)~age+sex+ring,cd))

####Guangzhou
gz$ring<-as.factor(as.character(gz$ring))
gz$ring<-factor(gz$ring,levels=c("Out of Ring 2 (第三圈层)","In Ring 1 (第一圈层)","Between Ring 1 and Ring 2 (第二圈层)"))
summary(coxph(Surv(survTime_month,event)~age+sex+ring,gz))
anova(coxph(Surv(survTime_month,event)~age+sex+ring,gz))

####Chongqing
cq$ring<-as.factor(as.character(cq$ring))
cq$ring<-factor(cq$ring,levels=c("Out of Ring 3","In Ring 1","Between Ring 1 and Ring 2","Between Ring 2 and Ring 3"))
summary(coxph(Surv(survTime_month,event)~age+sex+ring,cq))
anova(coxph(Surv(survTime_month,event)~age+sex+ring,cq))



#Table 5. HRs and 95% CIs for association between all-cause mortality and health risk factors in adjusted Cox models----

##Model 1 Age+Sex
summary(coxph(Surv(survTime_month,event)~age+sex,data))


##Model 2 +covariates
summary(coxph(Surv(survTime_month,event)~age+sex+ethnic+marriage+exercise+alcohol+smoking+education_cat+occupation+hhincome+innercity+city,data))


##Model 3 +POI
summary(coxph(Surv(survTime_month,event)~age+sex+ethnic+marriage+exercise+alcohol+smoking+education_cat+occupation+hhincome+innercity+city+count_5km_体育休闲服务+count_5km_医疗保健服务+count_5km_风景名胜,data))


##Model 4 +NDVI
summary(coxph(Surv(survTime_month,event)~age+sex+ethnic+marriage+exercise+alcohol+smoking+education_cat+occupation+hhincome+innercity+city+count_5km_体育休闲服务+count_5km_医疗保健服务+count_5km_风景名胜+NDVI_cum_yearly,data))


##model 5 +air
dat<-data%>%filter(data$NDVI_yearly_change!="NA")
dat<-dat%>%filter(dat$NDVI_cum_yearly!="NA")
dat<-dat%>%filter(dat$NDVI_cum_yearly<=9)
dat[,c("ethnic","marriage","exercise","alcohol","smoking","education_cat","occupation")]<-lapply(dat[,c("ethnic","marriage","exercise","alcohol","smoking","education_cat","occupation")],as.numeric)
dat<-dat%>%filter(ethnic==3|ethnic==2)%>%filter(marriage==3|marriage==2)%>%filter(exercise==4|exercise==2|exercise==3)%>%filter(smoking==4|smoking==2|smoking==3)%>%filter(alcohol==4|alcohol==2|alcohol==3)%>%filter(education_cat==4|education_cat==2|education_cat==3)%>%filter(occupation==3|occupation==2)
dat[,c("ethnic","marriage","exercise","alcohol","smoking","education_cat","occupation")]<-lapply(dat[,c("ethnic","marriage","exercise","alcohol","smoking","education_cat","occupation")],as.factor)
dat<-dat%>%filter(dat$lastyrno2<5)
summary(dat$count_5km_体育休闲服务)
summary(dat$count_5km_医疗保健服务)
summary(dat$count_5km_风景名胜)
dat$医疗<-ifelse(dat$count_5km_医疗保健服务<=47&dat$count_5km_医疗保健服务>=0,1,ifelse(dat$count_5km_医疗保健服务>47&dat$count_5km_医疗保健服务<=520,2,ifelse(dat$count_5km_医疗保健服务>520&dat$count_5km_医疗保健服务<=2261,3,4)))
dat$医疗<-as.factor(as.character(dat$医疗))
dat$体育<-ifelse(dat$count_5km_体育休闲服务<=46&dat$count_5km_体育休闲服务>=0,1,ifelse(dat$count_5km_体育休闲服务>46&dat$count_5km_体育休闲服务<=378,2,ifelse(dat$count_5km_体育休闲服务>378&dat$count_5km_体育休闲服务<=2641,3,4)))
dat$体育<-as.factor(as.character(dat$体育))
dat$风景<-ifelse(dat$count_5km_风景名胜<=18&dat$count_5km_风景名胜>=0,1,ifelse(dat$count_5km_风景名胜>18&dat$count_5km_风景名胜<=112,2,ifelse(dat$count_5km_风景名胜>112&dat$count_5km_风景名胜<=680,3,4)))
dat$风景<-as.factor(as.character(dat$风景))
summary(coxph(Surv(survTime_month,event)~age+sex+ethnic+marriage+exercise+alcohol+smoking+education_cat+occupation+hhincome+innercity+city+count_5km_医疗保健服务+count_5km_体育休闲服务+count_5km_风景名胜+NDVI_cum_yearly+lastyrno2+lastyro3+lastyrpm25,data))
summary(coxph(Surv(survTime_month,event)~age+sex+ethnic+marriage+exercise+alcohol+smoking+education_cat+occupation+hhincome+innercity+city+医疗+体育+风景+NDVI_cum_yearly+lastyrno2+lastyro3+lastyrpm25,dat))


##Model 6 NDVI trend
summary(coxph(Surv(survTime_month,event)~age+sex+ethnic+marriage+exercise+alcohol+smoking+education_cat+occupation+hhincome+innercity+city+count_5km_体育休闲服务+count_5km_医疗保健服务+count_5km_风景名胜+NDVI_yearly_change,data))


##Model 7 air trend
summary(coxph(Surv(survTime_month,event)~age+sex+ethnic+marriage+exercise+alcohol+smoking+education_cat+occupation+hhincome+innercity+city+count_5km_体育休闲服务+count_5km_医疗保健服务+count_5km_风景名胜+NDVI_yearly_change+no2_change+pm25_change+o3_change,data))




#Nighttime light----
ntl00<-read.csv("Z:/megacity project/CLHLS night light merged/c2000_b001_light2000_2.csv",header=T)
ntl02<-read.csv("Z:/megacity project/CLHLS night light merged/c2002_b001_light2002_2.csv",header=T)
ntl05<-read.csv("Z:/megacity project/CLHLS night light merged/c2005_b001_light2005_2.csv",header=T)
ntl08<-read.csv("Z:/megacity project/CLHLS night light merged/c2008_b001_light2008_2.csv",header=T)
ntl11<-read.csv("Z:/megacity project/CLHLS night light merged/c2011_b001_light2011_2.csv",header=T)
ntl<-bind_rows(ntl00,ntl02,ntl05,ntl08,ntl11)
ntl<-ntl%>%distinct(id,.keep_all=T)
data<-data%>%inner_join(ntl,by="id")

summary(coxph(Surv(survTime_month,event)~age+sex+X_mean,data))
summary(coxph(Surv(survTime_month,event)~age+sex+ethnic+marriage+exercise+alcohol+smoking+education_cat+occupation+hhincome+innercity+city+count_5km_医疗保健服务+count_5km_体育休闲服务+count_5km_风景名胜+NDVI_cum_yearly+lastyrno2+lastyro3+lastyrpm25+X_mean,data))

data$sex2<-as.numeric(as.factor(data$sex))
data$ethnic2<-as.numeric(as.factor(data$ethnic))
data$education_cat2<-as.numeric(as.factor(data$education_cat))
data$occupation2<-as.numeric(as.factor(data$occupation))
data$residence2<-as.numeric(as.factor(data$residence))
data$marriage2<-as.numeric(as.factor(data$marriage))
data$smoking2<-as.numeric(as.factor(data$smoking))
data$alcohol2<-as.numeric(as.factor(data$alcohol))
data$exercise2<-as.numeric(as.factor(data$alcohol))
data$education_year<-as.numeric(as.factor(data$education_year))

cor.test(data$X_mean,data$sex2,method="pearson")
cor.test(data$X_mean,data$age,method="pearson")
cor.test(data$X_mean,data$hincome,method="pearson")
cor.test(data$X_mean,data$ethnic2,method="pearson")
cor.test(data$X_mean,data$education_year,method="pearson")
cor.test(data$X_mean,data$occupation2,method="pearson")
cor.test(data$X_mean,data$residence2,method="pearson")
cor.test(data$X_mean,data$marriage2,method="pearson")
cor.test(data$X_mean,data$smoking2,method="pearson")
cor.test(data$X_mean,data$alcohol2,method="pearson")
cor.test(data$X_mean,data$exercise2,method="pearson")
cor.test(data$X_mean,data$POI1km,method="pearson")
cor.test(data$X_mean,data$POImin,method="pearson")
cor.test(data$X_mean,data$count_5km_三级甲等医院,method="pearson")
cor.test(data$X_mean,data$count_1km_三级甲等医院,method="pearson")
cor.test(data$X_mean,data$distance_三级甲等医院,method="pearson")
cor.test(data$X_mean,data$lastyrpm25,method="pearson")
cor.test(data$X_mean,data$lastyro3,method="pearson")
cor.test(data$X_mean,data$lastyrno2,method="pearson")
cor.test(data$X_mean,data$NDVI_cum_yearly,method="pearson")
cor.test(data$X_mean,data$count_5km_体育休闲服务,method="pearson")
cor.test(data$X_mean,data$count_5km_医疗保健服务,method="pearson")
cor.test(data$X_mean,data$count_5km_风景名胜,method="pearson")
cor.test(data$X_mean,data$POI5km,method="pearson")