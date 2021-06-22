#this script should customize a shapefile map for the locations of where samples of T. pennsylvanica were found
# Written by Abhi Mehrotra, June 22, 2021

# install (if necessary) and load needed packages. The following lines of code will check to see if a package is installed and 
# if not it will install it before loading it load it.

# packages necessary for this script----
if(!require(tidyverse))install.packages("tidyverse");library(tidyverse)
if(!require(sf))install.packages("sf");library(sf)
if(!require(rgdal))install.packages("rgdal");library(rgdal)
if(!require(sp)) install.packages('sp');library(sp)
if(!require(readxl)) install.packages('readxl');library(readxl)
if(!require(ggspatial)) install.packages('ggspatial');library(ggspatial)
if(!require(ggmap)) install.packages('ggmap');library(ggmap)
# load data and shapefiles

# another way to do it is to use a shapefile as a background
# this is a shape file with state boundaries
states<-st_read("/Users/abhimehrotra/Documents/states")

# what you'll notice is this shapefile has a much larger spatial extents than we need. We can change that by cropping it
la.b<-st_crop(states,xmin=-94.15,xmax=-89,ymin=29,ymax=33.15)
# another way to do this is to subset down to just the Louisiana polygon
la.c<-states[states$STATE_NAME=="Louisiana",]

plot(la.b)
plot(la.c)

# now I'm going to bring in some example sites- you'll replace this with the correct function and file path to bring in your sites

sites<-read_xlsx("/Users/abhimehrotra/Desktop/Miller_sponge gps coordinates_summer 2021.xlsx", sheet="Tp")

# now to make a pretty map
theme_set(theme_bw())

# First I'm going to make a base map that we can work through adding things to, that way you can pick and chose and make the map your own.
# if you want to use one of the shapefile base maps use this code to make your base map
(map1<-ggplot()+
    geom_sf(data=la.c))# I'm going to start with the cut out of Louisiana I made by subsetting the states shapefile 

# this assigns the answer to x but doesn't show you what it is in your console. This next line does both:
(x<-793*8)

ggplot()+
  geom_sf(data=la.c,fill="blue", color="orange",alpha=.5,size=2)
# the size here increases the thickness of the border. I don't recommend doing this in a map, but I wanted to show you all the
# possibilities

# from here on out everything is going to be the same between google map background and the shapefiles. So I will only demonstrate 
# with the shapefile example, because its a bit cleaner and easier to see what changes

# now to add sites to each map

map1+
  geom_point(aes(x=Long,y=Lat),data=sites,size=2)

# Now that we're done exploring how to change the shape and color of points I will add these to my base map so we can explore other
# changes we can make

(map1<-map1+
    geom_sf(data=la.c)+
    geom_point(aes(x=Long,y=Lat, color=Tp),data=sites,size=2))

# Now I'm going to show you different ways to change the colors of the points.
# if you have just a few points (or categories of points) you can manually assign colors:

# other good ones are:
if(!require(RColorBrewer))install.packages("RColorBrewer");library(RColorBrewer)
# color brewer has a lot of palettes you can see them here: 
display.brewer.all(colorblindFriendly = TRUE) # you can turn off the color blind friendly filter, but that limits the accessibility of your map

# note you can change the title of the legend here too:

(map1<-map1+
    scale_color_brewer(palette = "Dark2",name="Trochospongilla pennsylvanica"))

# now lets look at changing the axis labels. There are a couple ways that you can do this (a good resource: http://www.cookbook-r.com/Graphs/)
# I am going to show you the easiest way if you aren't changing other things about the axes (which you don't typically have to do with a map)
(map1<-map1+
    ylab("Latitude")+
    xlab("Longitude"))

# other things you might want to do

# get rid of the grid lines in the background:
(map1<-map1+
    theme(panel.grid = element_blank()))

# This removes all legends
(map1<-map1+
    theme(legend.position="none"))

# you can also make something bold or italic
(map1<-map1+
    theme(axis.text = element_text(size=12,family="sans"),
          axis.title= element_text(size=15,family="sans")))
