#species co-occurence 
library(tidyverse)
if(!require(cooccur))install.packages("cooccur");library(cooccur)

sp_pa<-read_rds("02_wdata/sponges_pa_Nov162020.rds")

sp_pa<-sp_pa%>%
  select(Site,sponges,ab)%>%
  mutate(pa=case_when(
    ab==0~0,
    ab!=0~1,
    is.na(ab)~0))%>%
  select(-ab)

spnames<-unique(sp_pa$sponges)

sp.mat<-matrix(nrow=length(spnames),ncol = length(spnames),
               dimnames = list(spnames,spnames))
sp.mat2<-matrix(nrow=length(spnames),ncol = length(spnames),
               dimnames = list(spnames,spnames))

spn<-length(spnames)

for(i in 1:(spn-1)){
  for(j in (i+1):spn){
  t1<-filter(sp_pa,sponges==spnames[i])$pa
  t2<-filter(sp_pa,sponges==spnames[j])$pa
  t3<-sum(t1*t2)
  sp.mat[i,j]<-t3
  sp.mat[j,i]<-t3
  sp.mat2[i,j]<-t3/(sum(t1)+sum(t2)-t3)
  sp.mat2[j,i]<-t3/(sum(t1)+sum(t2)-t3)
  }
}

sp_cooccur.n<-data.frame(sp.mat)%>%
  mutate(Sponge1=rownames(.))%>%
  pivot_longer(-Sponge1,names_to = "Sponge2",values_to="N.Sites")%>%
  distinct()

ggplot(sp_cooccur.n)+
  geom_tile(aes(x=Sponge1,y=Sponge2,fill=N.Sites))+
  scale_fill_viridis_c(option="C")

sp_cooccur.p<-data.frame(sp.mat2)%>%
  mutate(Sponge1=rownames(.))%>%
  pivot_longer(-Sponge1,names_to = "Sponge2",values_to="P.Sites")%>%
  distinct()

ggplot(sp_cooccur.p)+
  geom_tile(aes(x=Sponge1,y=Sponge2,fill=P.Sites))+
  scale_fill_viridis_c(option="C")

            