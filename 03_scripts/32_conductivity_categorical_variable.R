# this script will create categorical measurements for Conductivity Variable

# Written by Abhi Mehrotra, June 30, 2021

# load required packages
if(!require(tidyverse))install.packages("tidyverse");library(tidyverse)

# load required data
sp1 <- read.csv("01_odata/Miller_sponge data.csv")

# create histogram
p1 <- ggplot(data = sp1)

(p1 <- p1+
    geom_histogram(aes(cond),fill = "blue", color = "red", bins = 5)+
    theme_bw())
  
# determine min, max, and mean values of Conductivity variable
min(sp2$cond, na.rm = TRUE)
  
max(sp2$cond, na.rm = TRUE)
  
mean(sp2$cond, na.rm = TRUE)

# create categorical variable
sp2 <- sp1 %>%
  mutate(cond = as.numeric(cond),
         cond.cat = case_when(
           cond < 50 ~"Low Conductivity",
           cond >= 50 & cond < 300 ~"Moderately Low Conductivity",
           cond >= 300 & cond < 600  ~"Moderately High Conductivity",
           cond >= 600 & cond < 1000 ~"High Conductivity",
           cond >= 1000 ~"Extremely High Conductivity"))

# look at how many sites each sponge was found at within each category
sp2 <- sp2 %>%
  mutate(efr2 = ifelse(Efr == 0,0,1),# first I'm creating a new variable that is 0 if the sponge wasn't found at a site and 1 if it was
         tl2 = ifelse(Tl == 0,0,1),
         th2 = ifelse(Th == 0,0,1))%>%
  group_by(cond.cat)%>%
  summarize(n.sites = n(),
            n.efr = sum(efr2, na.rm = T),# this gives me the total number of sites where Efr was found in each category of cond (because I grouped by my cond categorical variable)
            n.tl = sum(tl2, na.rm = T),
            th = sum(th2, na.rm = T))# the rm.na tells it to ignore NAs

# create bar graph with categorical variable for Tl
p2 <- ggplot(data = sp2, aes(y = cond.cat, x = n.tl, fill = n.tl)) +        
  geom_bar(stat = "identity") +
  scale_y_discrete(limits = c("Low Conductivity", "Moderately Low Conductivity", 
                              "Moderately High Conductivity", "High Conductivity", 
                              "Extremely High Conductivity"))

(p2 <- p2+
    theme_bw()+
    theme(axis.text = element_text(size = 8, color = "red"),
    panel.grid = element_blank()))

# show distribution of conductivity
ggplot(data = sp1)+
  geom_boxplot(aes(y = cond, group = Tl, fill = as.factor(Tl)))

# create violin plot
ggplot(data = sp1)+
  geom_violin(aes(y = cond, x = Tl))

# and ridgeline plots
if(!require(ggridges))install.packages("ggridges");library(ggridges)

ggplot(data = sp2)+
  geom_density_ridges(aes(y = cond.cat, x = n.tl))

# another option is to just "see" all the datapoints
ggplot(data = sp2)+
  geom_jitter(aes(x = n.tl, y = cond.cat, color = cond.cat), width = .2)

# now lets make a plot visualizing 2 continuous variables
# this is called a scatter plot
ggplot(data = sp1)+
  geom_point(aes(x = pH,y = cond))

# now lets look at line plots. These are useful when there's an order to your x axis
# i.e., when your x axis is related to time
ggplot(data = sp1)+
  geom_line(aes(x = Site, y = cond))

# before we start looking at ways to make these plots prettier we need to talk about ways
# to convey more than 2 pieces of information with a plot. For example - lets
# say we want to see the relationship between variable1, variable2, and category 2
# we can do this using colors
ggplot(data = sp2)+
  geom_point(aes(x = n.efr, y = n.tl, color = cond.cat))

# or by using facets
ggplot(data = sp2)+
  geom_point(aes(x = n.efr, y = n.tl, color = cond.cat))+
  facet_grid(n.efr ~ n.tl)
