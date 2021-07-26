# This script is designed to get us started learning how to make better plots

#Started by Stephanie K. Archer 4/6/2021

# here are some important resources:
# Fundamentals of Data Visualization by Claus O. Wilke: https://clauswilke.com/dataviz/introduction.html
# GGplot cookbook: http://www.cookbook-r.com/Graphs/
# GGplot cheatsheet: https://www.maths.usyd.edu.au/u/UG/SM/STAT3022/r/current/Misc/data-visualization-2.1.pdf

# First we should talk about the different types of variables.
# When it comes to plotting you can have two basic types of variables:
# discrete and continuous. I like the explanation from Claus Wilke's book

# "Continuous data values are values for which arbitrarily fine intermediates exist. 
# For example, time duration is a continuous value. Between any two durations, 
# say 50 seconds and 51 seconds, there are arbitrarily many intermediates, such as 50.5 seconds, 
# 50.51 seconds, 50.50001 seconds, and so on. 
# By contrast, number of persons in a room is a discrete value. 
# A room can hold 5 persons or 6, but not 5.5....
# Next we’ll consider the types of data we may want to represent in our visualization. 
# You may think of data as numbers, but numerical values are only two out of several types of data we may encounter. 
# In addition to continuous and discrete numerical values, data can come in the form of discrete categories, in the form of dates or times, and as text. 
# When data is numerical we also call it quantitative and when it is categorical we call it qualitative. 
# Variables holding qualitative data are factors, and the different categories are called levels. 
# The levels of a factor are most commonly without order (as in the example of “dog”, “cat”, “fish” in Table 2.1), but factors can also be ordered, 
# when there is an intrinsic order among the levels of the factor (as in the example of “good”, “fair”, “poor” in Table 2.1)."


# The type of plot that you chose to use is, in part, dependent on
# the types of  data that you want to display. This page from the Claus Wilke book is 
# very useful for showing you the types of plots that are possible for different
# types of data/information that you want to display: https://clauswilke.com/dataviz/directory-of-visualizations.html


# we are going to work through the basic types of plots then move on to how
# to improve their design.

# First, lets load  packages
library(tidyverse)
if(!require(lubridate))install.packages("lubridate");library(lubridate)

# lets make an example dataset to use for a bar plot
data1<- data.frame(Condition = factor(x = c("Bad","OK","Fair","Good","Great"),
                                      levels = c("Bad","OK","Fair","Good","Great")),
                   N.Samples = sample(1:100,5))


# now lets make a basic bar plot

p1<-ggplot(data = data1)+
  geom_bar(aes(x=Condition,y=N.Samples),stat = "identity")

# while there are a lot of ways we can improve the design of this plot, it does
# accurately convey our data in a relatively clear way. So it is a perfectly fine plot.

# Now lets make a larger dataset that has a few different types of variables

data2<-data.frame(Category1 = sample(factor(x = c("Bad","OK","Fair","Good","Great"),
                                     levels = c("Bad","OK","Fair","Good","Great")),24,replace = T),
                  Category2 = sample(c("dog","cat","bird","fish"),24,replace = T),
                  Variable1 = sample(1:20,24,replace = T),
                  Variable2 = sample(seq(0,10,.001),24),
                  DateVariable = seq.Date(from = ymd("2019-01-01"),to = ymd("2020-12-01"),"month"))

# now lets make some plots that show the distribution of a single variable

# first lets do a histogram
ggplot(data = data2)+
  geom_histogram(aes(x=Variable1),bins = 20) # play around with the bins number to see what that does

# there are other ways to look at the distribution of a single variable

ggplot(data = data2)+
  geom_density(aes(x=Variable1),fill="black")

# you can used this type of plot to relatively easily compare the distribution of a 
# variable between categories. For example:

ggplot(data = data2)+
  geom_density(aes(x=Variable1,fill = Category1),alpha=.3)#the alpha here makes the 
# fills transparent do you can see through them. Try experimenting with it to see what
# changing the alpha value (can go between 0 and 1) does for your figure.

# another way to compare distributions between categories is boxplots

ggplot(data = data2)+
  geom_boxplot(aes(x = Category1,y = Variable1))

# these plots convey more information than the overlapping density plots. 
# the boxplots show information about the data in a succint way: https://towardsdatascience.com/understanding-boxplots-5e2df7bcbd51

# other plots are the violin plot:

ggplot(data = data2)+
  geom_violin(aes(x = Category1,y = Variable1))

# and ridgeline plots
if(!require(ggridges))install.packages("ggridges");library(ggridges)

ggplot(data = data2)+
  geom_density_ridges(aes(y=Category1, x=Variable1))


# another option is to just "see" all the datapoints

ggplot(data = data2)+
  geom_jitter(aes(x=Category1,y=Variable1,color = Category1),width=.2)

# now lets make a plot visualizing 2 conitnuous variables
# this is called a scatter plot

ggplot(data = data2)+
  geom_point(aes(x=Variable1,y=Variable2))

# now lets look at line plots. These are useful when there's an order to your x axis
# i.e., when your x axis is related to time

ggplot(data = data2)+
  geom_line(aes(x=DateVariable,y=Variable1))

# before we start looking at ways to make these plots prettier we need to talk about ways
# to convery more than 2 pieces of information with a plot. For example - lets
# say we want to see the relationship between variable1, variable2, and category 2
# we can do this using colors

ggplot(data = data2)+
  geom_point(aes(x=Variable1,y=Variable2,color=Category2))

# or by using facets
ggplot(data = data2)+
  geom_point(aes(x=Variable1,y=Variable2,color=Category1))+
  facet_grid(Category1~Category2)

# There's often multiple ways to display the same Lets say we want to
# look at how the value of Variable 2 varies depending on our two categories.
# one way we can do that is a slightly more complicated version of the boxplot

ggplot(data = data2)+
  geom_boxplot(aes(x=Category1,y=Variable2,fill=Category2))

# another way we can do this is with facets
ggplot(data = data2)+
  geom_boxplot(aes(x=Category1,y=Variable2))+
  facet_wrap(~Category2)

# or with a scatterplot (note I find this VERY hard to interpret)
ggplot(data = data2)+
  geom_jitter(aes(x=Category1,y=Variable2,color=Category2),width = .2)

# another way is to reorganize the data and make a heat map
data2%>%
  group_by(Category1,Category2)%>%
  summarize(mean.Variable2 = mean(Variable2))%>%
  ggplot()+
  geom_tile(aes(x=Category1,y=Category2,fill=mean.Variable2))

(p1<-ggplot(data = data1)+
    geom_hline(aes(yintercept=50),linetype="dashed",color="darkgreen",alpha=.6)+
  geom_bar(aes(x=Condition,y=N.Samples,fill=Condition),color="purple",stat = "identity",width=.5))

p1+
  theme_bw()+
  theme(axis.text = element_text(size=12,angle=0,hjust=1,vjust=.5,color="red"),
        plot.title = element_text(hjust = 0.5,size=20,face="italic",family="serif"),
        axis.title.x = element_text(size=15,face="bold"),
        axis.title.y=element_text(size=20,color="green"),
        legend.position = "none",
        panel.grid=element_blank())+
  scale_x_discrete(labels = c("Bad","O \n K","Fa \n ir","Good","Great"))+
  ggtitle("Our ugly plot")+
  ylab("Number of samples")+
  xlab("Something very long so \n its good to break it up")+
  geom_vline(aes(xintercept=2.5))+
  geom_abline(aes(slope=-10,intercept=90))
  
