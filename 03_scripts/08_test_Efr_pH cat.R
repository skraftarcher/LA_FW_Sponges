if(!require(tidyverse))install.packages("tidyverse");library(tidyverse)
sp1<-read_rds("02_wdata/sponges_pa_Nov162020.rds")

# Transformed pH into categories to manipulate. 

sp2<-sp1 %>%
  mutate(pH=as.numeric(pH),
         ph.cat = case_when(
           pH < 6 ~"acidic",
           pH > 6 & pH < 7 ~ "slightly.acidic",
           pH > 7 & pH < 8 ~"slightly alkaline",
           pH >= 8 ~"alkaline"))

#created a presence/absence file

sp2a<-sp2 %>%
  select(Site,ph.cat,sponges,ab) %>%
  filter(sponges=="Efr")%>%
  mutate(pa=case_when(
    ab==0~0,
    is.na(ab)~0,
    ab!=0~1))

# Ran glm with Efr to test for significance with categories

Efr.glm<-glm(pa~ph.cat,data=sp2a,family="binomial")

# Viewed report with summary (no sign observed)

summary(Efr.glm)

# created a table to visualize data

sp3a.1<-sp2a %>%
  group_by(ph.cat,pa)%>%
  summarize(n.site=n())%>%
  mutate(pa=ifelse(pa==0,"Absent","Present"))%>%
  pivot_wider(names_from = pa,values_from=n.site)%>%
  mutate(total.sites=Absent+Present,
         Absent=Absent/total.sites,
         Present=Present/total.sites)%>%
  pivot_longer(Present:Absent,names_to = "pa",values_to = "prop.sites")

# viewed data

(Efr.p1<-ggplot(data=sp3a.1)+
    geom_bar(aes(x=ph.cat,y=prop.sites,fill=pa),stat = "identity", width = 0.5)) +
  labs(title = expression('Proportion of sites with of '*italic(E.~fragilis)*' based on pH'))

# Question: how to develop a basic site count of presence in each pH range?


