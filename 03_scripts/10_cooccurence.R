install.packages("cooccur")
library(cooccur)
library(tidyverse)
library(vegan)

sp1<-read_rds("02_wdata/sponges_pa_Nov162020.rds")

sp2<-data.frame(sp1 %>%
  select(Site,sponges,ab)%>%
  mutate(ab  = ifelse(is.na(ab),0,ab))%>%
  pivot_wider(names_from = Site,values_from = ab))

rownames(sp2)<-sp2$sponges  

sp2<-sp2[,-1]

sp2<-sp2[,colSums(sp2)!=0]

sp3<-decostand(sp2,"pa")
sp.coocur<-cooccur(mat=sp3, type="spp_site",thresh = TRUE,spp_names = TRUE)

summary(sp.coocur)
plot(sp.coocur)

