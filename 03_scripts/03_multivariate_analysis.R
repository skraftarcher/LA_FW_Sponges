# this script starts to look at multivariate analysis

# written by Stephanie K. Archer 12/3/2020

# load required packages

if(!require(tidyverse))install.packages("tidyverse");library(tidyverse)
if(!require(vegan))install.packages("vegan");library(vegan)
if(!require(adespatial))install.packages("adespatial");library(adespatial)


# load required data
sp1<-read_rds("02_wdata/updated_sponges_Nov162020.rds")

# first I'm going to remove sites where we don't have all the environmental data
# I'm also just going to remove Ca for now

sp2<-filter(sp1,!is.na(Cl))%>%
  filter(!is.na(temp))%>%
  select(-Ca)

# now I'm going to make two datasets, one of environmental data, and one of the community data
env<-sp2%>%
  select(movement:Ni)

com<-sp2%>%
  select(Tl:Dr)

# now I'm first going to look at the difference (if any) in multivariate space between sites
# that do and do not have any sponges present. To do this I have to calculate how similar sites are 
# to do this we calculate what is called a similarity index. 
# in order to do this I'm going to re-code the movement variable so that if a system is lotic = 1
# if it is lentic = 0

env<-env%>%
  mutate(movement=case_when(
    movement=="Lo"~1,
    movement=="Len"~0))

# now I'm going to standardize the variables so that they are all on the same scale
env.std<-decostand(env,method="hellinger")
# now I'm going to plot them up in 2 dimensional space
env.rda<-rda(env.std)# first I'm doing to do something called a principle component analysis
# now I'm going to make a plot
# you can quickly plot up the results, but this is ugly
plot(env.rda)
# first I'm going to get information for each where each site "goes" on each axis out of what I just did
env.scores<-data.frame(scores(env.rda, choices=1:2, display="sites"))
# now I'm going to make a variable for color out of the sponge data.
# this variable will be the presence/absence variable
sppa<-sp2%>%
  mutate(pa=case_when(
    number==0~"absent",
    number!=0~"present"))%>%
  select(pa)%>%
  bind_cols(env.scores)
# now I can make my plot
(pa.plot<-ggplot()+
  geom_point(data=sppa,aes(x=PC1,y=PC2,color=pa),size=3)+
  theme_bw()+
    theme(panel.grid = element_blank())+
    geom_hline(aes(yintercept=0))+
    geom_vline(aes(xintercept=0)))
# doesn't look like there's a lot going on. 

# now I'm going to look at sites where we see sponges and see if there is anything distiguishing 
# where we see different species
# to do this I need data sets that only include sites where we saw sponges.
# I'm going to do this using base R syntax
env.sp<-env[sp2$number!=0,]# this says give me all the rows of the env dataset where the number
# variable in sp2 (the dataset we made the env dataset from) does not equal 0
# now for community data
com.sp<-com[sp2$number!=0,]

# now to do some transformations to do analysis
com.pa<-decostand(com.sp,"pa")

com.hel<-decostand(com.pa,"hellinger")
(spe.rda.all <- rda(com.hel ~ ., env.sp)) 
anova(spe.rda.all)
# this RDA isn't significant
(R2adj.all <- RsquareAdj(spe.rda.all)$adj.r.squared)
# and explains very little of the variability we're seeing, but we also have an awful lot of 
# explanatory variables in here. we can do a model selection process and see if we can improve our
# results

step.backward <- ordistep(spe.rda.all, 
                         direction="backward", permutations=how(nperm=199))
step.backward
RsquareAdj(step.backward)
# Global test of the RDA result
anova(step.backward, permutations=how(nperm=999))
# this model com.hel ~ movement + temp + cond + Turb + Fe + NH4 + Si + Hg is significant

# Tests of all canonical axes
anova(step.backward, by="axis", permutations=how(nperm=999))
# only one significant axis using this test, but there are other methods

# Apply Kaiser-Guttman criterion to residual axes
step.backward$CA$eig[step.backward$CA$eig > mean(step.backward$CA$eig)]
# this says we can use up to the first three axes. 

# now we have a significant model that explains ~11% of the variance we see in community
# composition

# look at a quick and dirty plot of the results
plot(step.backward)

# when interpreting this remember movement = 1 = lotic (or moving water)

# testing one specific variable

tub.rda<-rda(com.hel~Turb+Si,data=env.sp)
anova(tub.rda, permutations=how(nperm=999))
RsquareAdj(tub.rda)

# Making a nice graph 

env2.scores<-data.frame(scores(step.backward, choices=1:2, display="sites"))
com2.scores<-data.frame(scores(step.backward, choices=1:2, display="species"))
com.good<-goodness(step.backward)
(spr<-data.frame(com2.scores[which(com.good[,1]>=0.15|com.good[,2]>=0.15),]))
spr$Species<-factor(c("T. leidyi","T. horrida","E. fragilis"),levels =c("T. leidyi","T. horrida","E. fragilis")) 
splabs<-c(expression(italic("T. leidyi")),expression(italic("T. horrida")),expression(italic("E. fragilis")))
sig.state<-expression(paste("F"["4,41"]," = 2.14, p = 0.005"))

# pulling environmental variable scores out of the results
model.sum<-summary(step.backward)

env.varscore<-data.frame(model.sum$biplot)

ggplot()+
  geom_point(aes(x=RDA1,y=RDA2),data=env2.scores,size=2)+
  expand_limits(y=c(-1.75,1.5),x=c(-1.75,1.75))+
  theme_bw()+
  geom_hline(aes(yintercept=0),linetype="dashed")+
  geom_vline(aes(xintercept=0),linetype="dashed")+
  geom_point(aes(x=RDA1,y=RDA2,color=Species),data=spr,size=2)+
#  geom_segment(aes(x=c(0,0,0),y=c(0,0,0),xend=RDA1,yend=RDA2,color=Species),
#               data=spr,
#               arrow = arrow(length = unit(0.02, "npc")))+
  scale_color_brewer(palette = "Dark2",labels=splabs)+
  geom_text(aes(x=c(0.5,-.61,-.11),y=c(0.1,0.15,-.5)),label=c("Tl","Th","Efr"))+
  geom_text(aes(x=-1.25,y=1.5),label=sig.state)+
  geom_point(aes(x=RDA1,y=RDA2),data=env.varscore,size=2,color="blue")+
  geom_segment(aes(x=c(0,0,0,0),y=c(0,0,0,0),xend=RDA1,yend=RDA2),
               data=env.varscore,
               arrow = arrow(length = unit(0.02, "npc")))+
  geom_text(aes(x=RDA1+.2,y=RDA2+.2),data=env.varscore,label=c("Temp","Cond","Nitrate","Nitrite"))+
  theme(panel.grid = element_blank())

               