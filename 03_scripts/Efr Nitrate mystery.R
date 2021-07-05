#This is a script to look at Nitrate levels and sponge species
#Kenzie Cooke 6/21/2021
#loading packages
if(!require(tidyverse))install.packages("tidyverse"); library(tidyverse)
#import data
sp1<-read.csv("01_odata/Miller_sponge data.csv")
glimpse(sp1)
#create histogram to look at distribution of Nitrate in dataset. 
p1<-ggplot(data=sp1)
p1+
  geom_histogram(aes(Nitrate)) 
min(sp1$Nitrate, na.rm = T)
max(sp1$Nitrate, na.rm = T)
summary(sp1$Nitrate)
mean(sp1$Nitrate, na.rm = T)
#attempting to make new categories
sp2<-sp1 %>%
  filter(!is.na(Nitrate))%>%
  mutate(Turb=as.numeric(Nitrate),
         Nitrate.cat = case_when(
           Nitrate > 8 ~"outlier",
           Nitrate < 3.5 & Nitrate > 2 ~ "high",
           Nitrate < 2 & Nitrate > 1 ~"medium",
           Nitrate < 1 & Nitrate > 0 ~"low",
           Nitrate == 0 ~"none"),
         efr2=ifelse(Efr==0,0,1),# first I'm creating a new variable that is 0 if the sponge wasn't found at a site and 1 if it was
         tl2=ifelse(Tl==0,0,1),
         th2=ifelse(Th==0,0,1))%>%
  ungroup()%>%
  group_by(Nitrate.cat)%>%
  summarize(n.sites=n(),
            n.efr=sum(efr2,rm.na=T),# this gives me the total number of sites where Efr was found in each category of pH (because I grouped by my pH categorical variable)
            n.tl=sum(tl2,rm.na=T),
            th=sum(th2,rm.na=T))
print(sp2)
ggplot(data=sp2,aes(x=Nitrate.cat,y=n.efr)) +        
  geom_bar(stat="identity")+
  scale_x_discrete(limits = c("none","low",
                              "medium","high","outlier"))

sp2$Nitrate.cat<-factor(sp2$Nitrate.cat,levels =c("none", "low", "medium", "high",
                                                  "outlier"))
ggplot(data=sp2,aes(x=Nitrate.cat,y=n.efr)) +        
  geom_bar(stat="identity")
#end

