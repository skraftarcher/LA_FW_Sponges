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
    theme(panel.grid = element_blank()))
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
com.hel<-decostand(com.sp,"hellinger")
(spe.rda.all <- rda(com.hel ~ ., env.sp)) 
anova(spe.rda.all)
# this RDA isn't significant
(R2adj.all <- RsquareAdj(spe.rda)$adj.r.squared)
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