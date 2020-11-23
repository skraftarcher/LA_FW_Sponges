# this script is to analyze sponge presence vs environmental variables

# pH, conductivity, chlorine

# Loading required packages
if(!require(tidyverse))install.packages("tidyverse");library(tidyverse)

#bring in data
sp.pa<-read_rds("02_wdata/sponges_pa_Nov162020.rds")
sp1<-read_rds("02_wdata/updated_sponges_Nov162020.rds")

glimpse(sp.pa)
glimpse(sp1)

# create a presence absense variable

sp1$pa<-ifelse(sp1$number==0,"absent","present")

# now lets look at pH

ggplot(data=sp1,aes(x=pa,y=pH))+
  geom_violin()

t.test(pH~pa,data=sp1)

# No difference in the mean value between the two groups. 
# See if we can find a way to look at low value and and if the cutoff is significant
# look at community composition and if its different between the two "lumps"
# of the present distribution

# plot to look at species by species
ggplot(data=sp.pa)+
  geom_violin(aes(x=pa,y=pH))+
  facet_wrap(~sponges)

# Rcr distribution seems to be restricted by pH. 
# Tl seems to have a preference for pH around 7.7 but is found throughout the pH range
# Tp likes pHs around 6.5 but can be found at higher pHs
# Hb distribution also seems to be restricted by pH.
# Th, Sl, Efr do not seem to be strongly influenced by pH.

# now lets look at conductivity

ggplot(data=sp1,aes(x=pa,y=cond))+
  geom_violin()

t.test(cond~pa,sp1)

# plot to look at species by species
ggplot(data=sp.pa)+
  geom_violin(aes(x=pa,y=cond))+
  facet_wrap(~sponges)

# Rcr, Hb, Sl, Tp, Th are restricted in its distribution
# Tl and Efr seem to be found over a wider range of conductivities

# now lets look at chlorine

ggplot(data=sp1,aes(x=pa,y=Cl))+
  geom_violin()

t.test(Cl~pa,sp1)

# plot to look at species by species
ggplot(data=sp.pa)+
  geom_violin(aes(x=pa,y=Cl))+
  facet_wrap(~sponges)

# Hb has a very restricted distribution.
# Th occurs at higher Cl 
# Tl and Tp have opposite distributions.

