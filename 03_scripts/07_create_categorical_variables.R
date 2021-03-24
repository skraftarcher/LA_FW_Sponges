if(!require(tidyverse))install.packages("tidyverse");library(tidyverse)
sp1<-read.csv("01_odata/Miller_sponge data.csv")

sp2<-sp1 %>%
  mutate(pH=as.numeric(pH),
        ph.cat = case_when(
        pH < 6 ~"acidic",
        pH > 6 & pH < 7 ~ "slightly.acidic",
        pH > 7 & pH < 8 ~"slightly alkaline",
        pH >= 8 ~"alkaline"))


# look at how many sites each sponge was found at within each category

sp2%>%
  mutate(efr2=ifelse(Efr==0,0,1),# first I'm creating a new variable that is 0 if the sponge wasn't found at a site and 1 if it was
         tl2=ifelse(Tl==0,0,1),
         th2=ifelse(Th==0,0,1))%>%
  group_by(ph.cat)%>%
  summarize(n.sites=n(),
            n.efr=sum(efr2,rm.na=T),# this gives me the total number of sites where Efr was found in each category of pH (because I grouped by my pH categorical variable)
            n.tl=sum(tl2,rm.na=T),
            th=sum(th2,rm.na=T))# the rm.na tells it to ignore NAs




