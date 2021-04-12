#R worksheet
#Freshwater sponge R Team
#Instructions: Pick one of the environmental variables from the dataset to work with 
#as you complete this spreadsheet. Use previous scripts we have worked on together as 
#examples. Another good source of information is google. I google how to do things in R 
# on a daily basis. I am also available to help- just shoot me an email!

#1.	What variable did you pick? 

# Turbidity!

# Next, load packages you might need. You are likely to need the tidyverse package.
if(!require(tidyverse))install.packages("tidyverse"); library(tidyverse)

# Next, bring in the dataset you want to use. I recommend starting with the “Miller_sponge data.csv” file that Dr. Miller sent us when we started.
sp1<-read.csv("01_odata/Miller_sponge data.csv")

# Now, you are going to create a new categorical variable based on the values of the 
# variable you picked. It might be useful to do a bit of googling to see if there are 
# established categories for your variable – for example, a variable might be an 
# indicator of water quality for Louisiana and therefore have categories defined by 
#Louisiana Department of Environmental Quality such as “healthy”, “acceptable”, and 
# “unhealthy.” If you can easily find categories use those. Otherwise you can create 
# your own categories. 

# Before you create your new categorical variable – it would be useful to look at the 
# distribution of your variable in our dataset. This means looking at the range of 
# values, and how the values are distributed across that range. The easiest way to 
# start this is to make a histogram of your variable. 

hist(sp1$Turb,breaks=30)

# OR

ggplot(data=sp1)+
  geom_histogram(aes(x=Turb))

# 3.	Use R to determine the minimum value of your variable in our dataset. What is it?
min(sp1$Turb,na.rm = T)

# OR

summary(sp1$Turb)
  
# 4.	Use R to determine the maximum value of your variable in our dataset. What is it?
max(sp1$Turb,na.rm = T)

# OR

summary(sp1$Turb) 
  
# 5.	Use R to determine the mean value of your variable in our dataset. What is it?
mean(sp1$Turb,na.rm = T)

# OR

summary(sp1$Turb)

# Now break your variable into at least three categories - what are they?

# Turb > 1000 ~"extremely high",
# Turb < 1000 & Turb > 400 ~ "high",
# Turb < 400 & Turb > 100 ~"moderately high",
# Turb < 100 & Turb > 40 ~"moderately low",
# Turb < 40 & Turb > 10 ~"low",
# Turb < 10 & Turb > 1 ~ "acceptable",
# Turb < 1 ~"drinking water"

sp2<-sp1 %>%
  filter(!is.na(Turb))%>%
  mutate(Turb=as.numeric(Turb),
         Turb.cat = case_when(
           Turb > 1000 ~"extremely high",
           Turb < 1000 & Turb > 400 ~ "high",
           Turb < 400 & Turb > 100 ~"moderately high",
           Turb < 100 & Turb > 40 ~"moderately low",
           Turb < 40 & Turb > 10 ~"low",
           Turb < 10 & Turb > 1 ~ "acceptable",
           Turb < 1 ~"drinking water"),
         efr2=ifelse(Efr==0,0,1),# first I'm creating a new variable that is 0 if the sponge wasn't found at a site and 1 if it was
         tl2=ifelse(Tl==0,0,1),
         th2=ifelse(Th==0,0,1))%>%
  group_by(Turb.cat)%>%
  summarize(n.sites=n(),
            n.efr=sum(efr2,rm.na=T),# this gives me the total number of sites where Efr was found in each category of pH (because I grouped by my pH categorical variable)
            n.tl=sum(tl2,rm.na=T),
            th=sum(th2,rm.na=T))# the rm.na tells it to ignore NAs
#8.	Now, we are going to look at how many times the most commonly found sponge species 
# were found at sites that fell in each of your categories. The sponge species we will 
#look at are: Efr, Tl, Th. Fill in each slot in the table below with the number of sites where each sponge was found:
print(sp2)

#9.	Pick one species from above and make a 
# bar graph showing the information from your table. 

ggplot(data=sp2,aes(x=Turb.cat,y=n.tl)) +        
  geom_bar(stat="identity")




