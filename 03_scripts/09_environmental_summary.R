# script to pull out summary of environmental conditions for a species

# bring in data
if(!require(tidyverse))install.packages("tidyverse");library(tidyverse)
sp1<-read_rds("02_wdata/sponges_pa_Nov162020.rds")


# decide what species you want to look at

# Example with T. horrida
th.sp<-sp1%>%
  filter(sponges=="Th")%>%
  filter(pa=="present")

# you can use the number of rows in this new dataset to easily 
# get the number of sites you saw the sponge at.
# for example: the th.sp dataset has 24 rows - that means you saw it at 24 sites

# now you can look at the range of different variables

# an example with pH

ph.summary<-th.sp %>%
  select(pH)%>%
  summarize(pH.range=range(pH),mean=mean(pH),median=median(pH))
  
# visualize the range of pH

ggplot(data=th.sp)+
  geom_histogram(aes(x=pH),fill="black",alpha=.2,color="black",bins=5)+
  geom_vline(aes(xintercept=ph.summary$mean[1]),color="red",size=2)+
  geom_text(aes(x=ph.summary$mean[1]*1.05,y=9.5),label="Mean = 7.28",size=5)+
  ylab("Number of Sites")+
  theme_bw()

# look at pH for two species
thef.sp<-sp1%>%
#  filter(sponges=="Th" | sponges=="Efr")%>%
  filter(sponges %in% c("Th","Efr"))%>%
  filter(pa=="present")%>%
  select(pH,sponges)

thef.sp$sponges<-factor(thef.sp$sponges,labels = c("E. fragilis","T. horrida"))
# now make a plot where you can see the pH range for both species

ggplot(data=thef.sp)+
  geom_histogram(aes(x=pH,group=sponges),alpha=.2,color="black",bins=5)+
  facet_wrap(~sponges)+
  theme(strip.text = element_text(face="italic",size=12))
  

