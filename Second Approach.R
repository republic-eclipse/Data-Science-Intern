library(tidyverse)
library(ggplot2)

#select the datasets folder
setwd(choose.dir())

options(scipen = 999)


#######2.Approach: Countries with World Happiness Indexes
df_whi<- read_csv("WHI in 2017.csv")
df_socialmedia<-read_csv("Social_Media_Usage_by_Country.csv")%>%
  rename(Social_Media_Usage=Percentage)

df_population<-read_csv("Population.csv")





###Getting the mental tables for...
#..Adhd
df_adhd<-read_csv("adhd (year and sex).csv")%>%
 filter(Year == 2017)%>%
 rename(Country = Entity,
        ADHD_Prevalence_men = "Prevalence - Attention-deficit/hyperactivity disorder - Sex: Male - Age: All Ages (Number)",
        ADHD_Prevalence_woman="Prevalence - Attention-deficit/hyperactivity disorder - Sex: Female - Age: All Ages (Number)")

#..Depression
df_depression<-read_csv("depression (year and country).csv")%>%
  filter(Year == 2017)%>%
  rename(Country = Entity,
         DEPRESSION_Rate="Prevalence - Depressive disorders - Sex: Both - Age: Age-standardized (Percent)") 

#..and Anxiety
df_anxiety<-read_csv("anxiety-disorders(year and country).csv")%>%
  filter(Year == 2017)%>%
  rename(Country = Entity,
         ANXIETY_Prevalence_Both="Prevalence - Anxiety disorders - Sex: Both - Age: All Ages (Number)") 


####Merge all tables(mental disorders,social media usage, happines scores)
df_all <- df_adhd %>%
  merge(df_depression, by="Country")%>%
  merge(df_anxiety,by="Country")%>%
  merge(df_whi,by="Country")%>%
  merge(df_socialmedia,by="Country")%>%
  merge(df_population,by="Country")


#Select most important factors
df_final<-df_all%>%
  select(Country,
         ADHD_Prevalence_woman,
         ADHD_Prevalence_men,
         DEPRESSION_Rate,
         ANXIETY_Prevalence_Both,
         Social_Media_Usage,
         Happiness.Score,
         population)


#Summarize Adhd Prevalence + getting Anxiety,Adhd rates
df_final<- df_final %>% 
  mutate( ADHD_Prevalance_Both = ADHD_Prevalence_men + ADHD_Prevalence_woman)%>%
  mutate( ADHD_Rate = (ADHD_Prevalance_Both / population)*100)%>%
  mutate( ANXIETY_Rate=(ANXIETY_Prevalence_Both / population)*100)


#######Country span method 
countries_span<-df_final%>%
  subset((Happiness.Score>6)& (Happiness.Score<7.1))%>%
  select(Country,Social_Media_Usage,DEPRESSION_Rate,ANXIETY_Rate,ADHD_Rate)


#Depression rate
ggplot(data = countries_span)+
  aes(x = Social_Media_Usage, y =DEPRESSION_Rate, label=Country)+
  ggtitle("Country Span: Depression Rate")+
  scale_x_continuous(name="Social Media Usage [in %]", limits=c(40, 110),)+
  scale_y_continuous(name="Depression Rate [in %]",expand = c(0, 0), limits=c(0, 6))+
  geom_point(color="#f14d59")+
  geom_text(aes(label=Country,color="#f14d59"),hjust=0, vjust=-0.2,size=5)+
  geom_smooth(method = "lm",color="#dc4374",size=1.4)+
  theme(legend.position = "null" ,
        panel.background = element_blank(),
        panel.grid.major = element_line(colour = "grey80"),
        panel.grid.minor = element_line(colour = "grey80"),
        text = element_text(size = 15),
        axis.title=element_text(size=14),
        axis.line = element_line(size = 1, colour = "black"))


#Adhd rate
ggplot(data = countries_span)+
  aes(x = Social_Media_Usage, y =ADHD_Rate, label=Country)+
  ggtitle("Country Span: ADHD Rate")+
  scale_x_continuous(name="Social Media Usage [in %]", limits=c(40, 110),)+
  scale_y_continuous(name="ADHD Rate [in %]",expand = c(0, 0), limits=c(0, 2))+
  geom_point(color="#f14d59")+
  geom_text(aes(label=Country,color="#f14d59"),hjust=0, vjust=-0.2,size=5)+
  geom_smooth(method = "lm",color="#dc4374",size=1.4)+
  theme(legend.position = "null" ,
        panel.background = element_blank(),
        panel.grid.major = element_line(colour = "grey80"),
        panel.grid.minor = element_line(colour = "grey80"),
        text = element_text(size = 15),
        axis.title=element_text(size=14),
        axis.line = element_line(size = 1, colour = "black"))

# Anxiety rate
ggplot(data = countries_span)+
  aes(x = Social_Media_Usage, y =ANXIETY_Rate, label=Country)+
  ggtitle("Country Span: Anxiety Rate",)+
  scale_x_continuous(name="Social Media Usage [in %]", limits=c(40, 110))+
  scale_y_continuous(name="Anxiety Rate [in %]",expand = c(0, 0),limits=c(0, 7))+
  geom_point(color="#f14d59")+
  geom_text(aes(label=Country,color="#f14d59"),hjust=0, vjust=-0.2,size=5)+
  geom_smooth(method = "lm",color="#dc4374",size=1.4)+
  theme(legend.position = "null" ,
        panel.background = element_blank(),
        panel.grid.major = element_line(colour = "grey80"),
        panel.grid.minor = element_line(colour = "grey80"),
        text = element_text(size = 15),
        axis.title=element_text(size=14),
        axis.line = element_line(size = 1, colour = "black"))

###Compute the correlation coefficients
depression_cor<-cor(countries_span$Social_Media_Usage, countries_span$DEPRESSION_Rate)
adhd_cor<-cor(countries_span$Social_Media_Usage, countries_span$ADHD_Rate)
anxiety_cor<-cor(countries_span$Social_Media_Usage, countries_span$ANXIETY_Rate)


#######Country pairs method
set_all <- df_final %>% subset(Country=="Italy" | Country=="Russia" | Country=="Nigeria" | Country=="Vietnam" | Country=="Brazil" | Country=="United Arab Emirates" | Country =="Austria" | Country == "United States" | Country == "Singapore" | Country == "Mexico")

set_all <- set_all %>% mutate(Countries= case_when(Country =="Italy" |Country =="Russia" ~ "Italy|Russia",
                                                   Country=="Brazil" | Country=="United Arab Emirates" ~ "Brazil|UAE",
                                                   Country=="Nigeria" | Country=="Vietnam"~ "Nigeria|Vietnam" ,
                                                   Country =="Austria" | Country == "United States" ~ "Austra|US",
                                                   Country == "Singapore" | Country == "Mexico" ~ "Singapore|Mexico"))




#Anxiety rate country pairs
ggplot(set_all, aes(Social_Media_Usage,ANXIETY_Rate, group=Countries))+
  geom_text(aes(label=Country, colour= factor(Countries)) ,size=5,hjust=-0.18, vjust=0)+
  geom_point(aes(colour= factor(Countries)),size=6 )+ geom_line()+geom_path(aes(colour= factor(Countries)), size = 1.5)+
  theme_bw()+theme(legend.position = "null",text = element_text(size=15))+
  scale_x_continuous(name="Social Media Usage [in %]", limits=c(0,130))+
  scale_y_continuous(name= "Anxiety Rate [in %]", limits=c(0,7))+
  ggtitle("Country pairs: Anxiety Rate")

#ADHD rate country pairs
ggplot(set_all, aes(Social_Media_Usage,ADHD_Rate, group=Countries))+
  geom_text(aes(label=Country, colour= factor(Countries)) ,size=5,hjust=-0.18, vjust=0)+
  geom_point(aes(colour= factor(Countries)),size=6 )+ geom_line()+geom_path(aes(colour= factor(Countries)), size = 1.5)+
  theme_bw()+theme(legend.position = "null",text = element_text(size=15))+
  scale_x_continuous(name="Social Media Usage [in %]", limits=c(0,130))+
  scale_y_continuous(name= "ADHD Rate [in %]", limits=c(0,7))+
  ggtitle("Country pairs: ADHD Rate")

#Depression rate country pairs
ggplot(set_all, aes(Social_Media_Usage,DEPRESSION_Rate, group=Countries))+
  geom_text(aes(label=Country, colour= factor(Countries)) ,size=5,hjust=-0.18, vjust=0)+
  geom_point(aes(colour= factor(Countries)),size=6 )+ geom_line()+geom_path(aes(colour= factor(Countries)), size = 1.5)+
  theme_bw()+theme(legend.position = "null",text = element_text(size=15))+
  scale_x_continuous(name="Social Media Usage [in %]", limits=c(0,130))+
  scale_y_continuous(name= "Depression Rate [in %]", limits=c(0,7))+
  ggtitle("Country pairs: Depression Rate")



######Table of WHI for report
p<-df_whi %>%
  select (-Whisker.high,-Whisker.low,-Happiness.Rank) %>%
  rename(GDP.per.Capita=Economy..GDP.per.Capita.,
         Social.support=Family,
         Life.expectancy=Health..Life.Expectancy.,
         Corruption=Trust..Government.Corruption.)
