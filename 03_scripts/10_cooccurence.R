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


library(EcoSimR)
myModel <- cooc_null_model(speciesData=sp3,suppressProg=TRUE)
summary(myModel)

plot(myModel,type = "cooc")
plot(myModel,type = "burn_in")
plot(myModel,type="hist")

library(cooccur)
library(visNetwork)

nodes <- sp3(id = 1:nrow(sponges),
                    label = rownames(sponges),
                    color = “#606482”,
                    shadow = TRUE)

