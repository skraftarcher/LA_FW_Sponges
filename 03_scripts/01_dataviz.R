# This script is to look at the sponge and environment data for outliers

# Written by Stephanie K. Archer, 11/16/2020

# load packages

if(!require(tidyverse))install.packages("tidyverse");library(tidyverse)

# load data

sp1<-read.csv("01_odata/Miller_sponge data.csv")

# explore data

glimpse(sp1)

# update species

sp2<-sp1 %>%
  select(-Rcr) %>%
  rename(Rcr=Aa)

glimpse(sp2)

write_rds(sp2,"02_wdata/updated_sponges_Nov162020.rds")

# looking at data

p1<-ggplot(data=sp1)

p1+
  geom_histogram(aes(Si))

p1+
  geom_violin(aes(x=movement,y=Si))

# create presence/absence dataset

sp3<- sp2 %>%
  pivot_longer(Tl:Dr,names_to="sponges",values_to="ab")%>%
  mutate(pa=case_when(
    ab!=0 ~"present",
    ab==0 ~ "absent",
    is.na(ab)~"absent"))

write_rds(sp3,"02_wdata/sponges_pa_Nov162020.rds")
# graph of silica by presence/absence for each sponge species

ggplot(data=sp3)+
  geom_violin(aes(x=pa,y=Si))+
  facet_wrap(~sponges)

ggplot(data=sp3)+
  geom_violin(aes(x=pa,y=Si))+
  facet_grid(sponges~movement)

p2<-ggplot(data=sp3)+
  facet_grid(sponges~movement)


p2+
  geom_violin(aes(x=pa,y=Sulfate))

p3<-ggplot(data=sp3 %>%
         filter(sponges %in% c("Efr","Rcr","Sl","Th","Tl","Tp")))

p3+
  geom_violin(aes(x=pa,y=Sulfate))+
  facet_wrap(~sponges)



# for loop to look at all environmental variables

for(i in 4:24){
  t1<-cbind(sp3$pa,sp3[,i],sp3$sponges)
  colnames(t1)<-c("pa","env","sponges")
  ylabel<-colnames(sp3)[i]
  ggplot(data=t1)+
    geom_violin(aes(x=pa,y=env))+
    ylab(ylabel)+
    facet_wrap(~sponges)
  ggsave(paste0("04_figures/",ylabel,".jpg"))
}


for(i in 1:5){
  print(paste("hello for the ",i,"th time"))
}



