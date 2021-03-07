library(tidyverse)
library(ggplot2)

#select the datasets folder
setwd(choose.dir())

options(scipen = 999)

#######1.Approach: Age Groups in the US
data<-read.csv(file="sm_usage.csv",header=TRUE,sep=";")

data$date<-as.Date(data$date, "%m/%d/%Y")


data1<-read.csv(file="mental_illness_data_age.csv",header=TRUE,sep=";")

#create barplot for mental illnes per age groups in the US
bar<-ggplot(data=data1, aes(x=Age, y=Percent)) +
  xlab("Age")+
  geom_bar(stat="identity",fill="#dc4374")+
  theme_bw()+
  ggtitle("Mentall illnes per age group in the US")+
  scale_y_continuous(name="Mentall illness [in %]",expand = expansion(mult = c(0, .1)),limits=c(0,16))+
  theme(legend.position = "null" ,
        panel.background = element_blank(),
        panel.grid.major = element_line(colour = "grey80"),
        panel.grid.minor = element_line(colour = "grey80"),
        text = element_text(size = 15),
        axis.title=element_text(size=14),
        axis.line = element_line(size = 1, colour = "black"))
#show barplot
bar

#create data frame sData
selectedData<-subset(data,date=="2018-01-10")
h1<-selectedData$X18.29
h2<-selectedData$X30.49
h3<-selectedData$X50.64
h4<-selectedData$X65
h5<-c("18-29","30-49","50-64","65+")
h6<-c(h1,h2,h3,h4)
sData<-data.frame(Age=h5,Percent=h6)

#create barplot for Social media usage per age group in the US

bar1<-ggplot(data=sData, aes(x=Age, y=Percent)) +
  xlab("Age")+
  ylab("Social Media Usage [in %]")+
  geom_bar(stat="identity",fill="#dc4374")+
  theme_bw()+
  ggtitle("Social Media usage per age group in the US")+
  theme(legend.position = "null" ,
        panel.background = element_blank(),
        panel.grid.major = element_line(colour = "grey80"),
        panel.grid.minor = element_line(colour = "grey80"),
        text = element_text(size = 15),
        axis.title=element_text(size=14),
        axis.line = element_line(size = 1, colour = "black"))

#show barplot
bar1

