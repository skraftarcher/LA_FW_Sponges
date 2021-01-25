# Script to look at the likelihood of finding T. horrida in lotic vs lentic environments

# written by the whole class Jan 11, 2021, modified by Mary Miller

# load data and packages
if(!require(tidyverse))install.packages("tidyverse");library(tidyverse)
sp1<-read_rds("02_wdata/sponges_pa_Nov162020.rds")

# create the smaller dataset to answer this question

sp2b<-sp1 %>%
  select(Site,movement,sponges,ab) %>%
  filter(sponges=="Th")%>%
  mutate(pa=case_when(
    ab==0~0,
    is.na(ab)~0,
    ab!=0~1))

# running a logistic regression now using glm and a binomial family

Th.glm<-glm(pa~movement,data=sp2,family="binomial")

# now look at results

summary(Th.glm)

# now make a figure

# manipulate data to get it in the right shape to make a bar graph

sp3b<-sp2b %>%
  group_by(movement,pa)%>%
  summarize(n.site=n())%>%
  mutate(pa=ifelse(pa==0,"Absent","Present"))%>%
  pivot_wider(names_from = pa,values_from=n.site)%>%
  mutate(total.sites=Absent+Present,
         Absent=Absent/total.sites,
         Present=Present/total.sites)%>%
  pivot_longer(Present:Absent,names_to = "pa",values_to = "prop.sites")

# now make figure

(Th.p1<-ggplot(data=sp3b)+
    geom_bar(aes(x=movement,y=prop.sites,fill=pa),stat = "identity"))
