#categorical variable turbidity

if(!require(tidyverse))install.packages("tidyverse");library(tidyverse)
sp1<-read.csv("01_odata/Miller_sponge data.csv")

sp2<-sp1 %>%
  mutate(Turb=as.numeric(Turb),
         Turb.cat = case_when(
           Turb > 1000 ~"extremely high",
           Turb < 1000 & Turb > 400 ~ "high",
           Turb < 400 & Turb > 100 ~"moderately high",
           Turb < 100 & Turb > 40 ~"moderately low",
           Turb < 40 & Turb > 10 ~"low",
           Turb < 10 & Turb > 1 ~ "acceptable",
           Turb < 1 ~"drinking water"))


max(sp2$Turb, na.rm=TRUE)

min(sp2$Turb, na.rm=TRUE)

mean(sp2$Turb, na.rm=TRUE)

sp2%>%
  mutate(efr2=ifelse(Efr==0,0,1),# first I'm creating a new variable that is 0 if the sponge wasn't found at a site and 1 if it was
         tl2=ifelse(Tl==0,0,1),
         th2=ifelse(Th==0,0,1))%>%
  group_by(Turb.cat)%>%
  summarize(n.sites=n(),
            n.efr=sum(efr2,rm.na=T),# this gives me the total number of sites where Efr was found in each category of pH (because I grouped by my pH categorical variable)
            n.tl=sum(tl2,rm.na=T),
            th=sum(th2,rm.na=T))# the rm.na tells it to ignore NAs

ggplot(data=sp2, aes(x=Turb.cat)) +        
  geom_bar()



            