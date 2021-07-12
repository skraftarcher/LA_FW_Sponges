# this script will create categorical measurements for Conductivity Variable

# Written by Abhi Mehrotra, June 30, 2021

# load required packages
if(!require(tidyverse))install.packages("tidyverse");library(tidyverse)

# load required data
sp1<-read.csv("01_odata/Miller_sponge data.csv")

# create histogram
p1<-ggplot(data=sp1)

(p1<-p1+
    geom_histogram(aes(cond),fill="blue",color="red", bins=5)+
    theme_bw())
  
# determine min, max, and mean values of Conductivity variable
  
min(sp2$cond, na.rm=TRUE)
  
max(sp2$cond, na.rm=TRUE)
  
mean(sp2$cond, na.rm=TRUE)

# create categorial variable
sp2<-sp1 %>%
  mutate(cond=as.numeric(cond),
         cond.cat = case_when(
           cond < 50 ~"Low Conductivity",
           cond >= 50 & cond < 300 ~"Moderately Low Conductivity",
           cond >= 300 & cond < 600  ~"Moderately High Conductivity",
           cond >= 600 & cond < 1000 ~"High Conductivity",
           cond >= 1000 ~"Extremely High Conductivity"))

# look at how many sites each sponge was found at within each category
sp2 <- sp2%>%
  mutate(efr2=ifelse(Efr==0,0,1),# first I'm creating a new variable that is 0 if the sponge wasn't found at a site and 1 if it was
         tl2=ifelse(Tl==0,0,1),
         th2=ifelse(Th==0,0,1))%>%
  group_by(cond.cat)%>%
  summarize(n.sites=n(),
            n.efr=sum(efr2,na.rm = T),# this gives me the total number of sites where Efr was found in each category of cond (because I grouped by my cond categorical variable)
            n.tl=sum(tl2,na.rm = T),
            th=sum(th2,na.rm = T))# the rm.na tells it to ignore NAs

# create bar graph with categorical variable for Efr
p2<-ggplot(data=sp2, aes(x=cond.cat, y=n.efr,fill=n.efr)) +        
  geom_bar(stat = "identity")

(p2<-p2+
    theme_bw()+
    theme(axis.text = element_text(size=8, color ="red"),
    panel.grid = element_blank()))


# show distribution of conductivity
ggplot(data=sp1)+
  geom_boxplot(aes(y=cond,group=Efr,fill=as.factor(Efr)))
