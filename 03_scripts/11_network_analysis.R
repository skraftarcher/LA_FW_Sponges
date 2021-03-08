# script to create a network figure

library(tidyverse)
if(!require(igraph))install.packages(igraph);library(igraph)

# load data
sp1<-read.csv("01_odata/Miller_sponge data.csv")

sp2<-sp1 %>%
  dplyr::select(Tl:Aa)%>%
  dplyr::select(-Rcr)%>%
  rename(Rcr=Aa)

sp2<-sp2[-73,]

sp3<-decostand(sp2,"pa")
sp.coocur<-cooccur(mat=t(sp3), type="spp_site",thresh = TRUE,spp_names = TRUE)

sp4<-sp.coocur[[2]][,1:2]

sp.names<-colnames(sp3)
sp4<-as.matrix(sp4%>%
  mutate(sp1=sp.names[sp4[,1]],
         sp2=sp.names[sp4[,2]]))

sp.net<-graph_from_edgelist(el=sp4)

plot(sp.net)
