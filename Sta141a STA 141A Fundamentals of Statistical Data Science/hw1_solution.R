## problem 1a
days=1:1826
nd=length(days)
city=sample(1:10,nd,replace=T)
temp=runif(nd,0,200)
precip=rbinom(nd,100,0.01)
pm10=runif(nd,0,100)
death=runif(nd,0,100)
pollution_df=data.frame(city,days,temp,precip,pm10,death)
attach(pollution_df)
summary(pollution_df)

##1

death_20<-pollution_df[pm10>20,"death"]
city_20<-pollution_df[pm10>20,"city"]
tapply(death_20,city_20,mean)

##2
days_2=intersect(which(precip==0),which(temp>80))
death_2<-pollution_df[days_2,"death"]
pm10_2<-pollution_df[days_20,"pm10"]
city_2<-pollution_df[days_20,"city"]
tapply(death_2,city_2,mean)

## problem 1b
N<-1000 # visits
n<-200 # patients
patients<-sample(1:n,N,replace=TRUE)
gender<-patients
age=round(patients/2)
for(i in 1:N){
ifelse(patients[i]<=100,gender[i]<-"M",gender[i]<-"F")
}
nd=50 #dates
date<-sort(sample(1:nd,N,replace=T))
table(date)
weight=runif(N,0,200)
BP.sys=runif(N,0,600)
BP.dia=runif(N,0,600)
glucose=runif(N,0,100)
patients_df=data.frame(patients,gender,age,weight,BP.sys,BP.dia,glucose)

##1
table(patients)
##2
weight_patients<-split(weight,patients)
weight_max<-sapply(weight_patients,max)
patients_180<-which(weight_max>180)
patients_180_df<-patients_df[which(patients%in%patients_180),]
dim(patients_180_df)
tapply(patients_180_df$BP.sys,patients_180_df$patients,mean)
##
age_patients<-split(patients_df$age,patients_df$patients)
age_patients_1<-lapply(age_patients,function(x){return(x[1])})
patients_40<-which(age_patients_1>40)
patients_40_df<-patients_df[which(patients%in%patients_40),]
dim(patients_40_df)
tapply(patients_40_df$glucose,patients_40_df$patients,mean)
##

##2
animals<-c('cat','dog','cow','squirrel')
colors<-c('white','black','brown','red')
attributes<-c('big','small','angry','cute','finicky')
#a
Animal<-sample(animals,100,T)
Color<-sample(colors,100,T)
Attribute<-sample(attributes,100,T)
#b
paste(Animal,Color,Attribute,sep=" ")
#c
t_an<-table(Animal,Color,Attribute)
#d
apply(t_an,c(1,2),sum)
apply(t_an,c(1,3),sum)
apply(t_an,1,sum)


##3
plot(AirPassengers)
#time series. Seasonal effect and upward trend.

plot(EuStockMarkets)
#four time series. similar behaviour. accelerated growth end-of-sample.

plot(trees)
#scatter plot matrices. Linear positive association, particularly between girth and volume.

