# this script should customize a google map for the locations of where sponge samples were found
# Written by Abhi Mehrotra, June 15, 2021

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

# if you want to use google maps you have to go here and register: https://cloud.google.com/maps-platform/
# for trial purposes you can use my key, but if you decide to keep using google maps for things, please change the key to your own.
register_google(key="AIzaSyC62_TAASDWCV_I6e6k5uzNf5i-hCWeNb4")
# set the boundary box for the backgroun map
la<-c(-94.15,29,-89.5,33.15)
# get a google map background
la.map.h<-get_map(location=la,source="google",maptype="hybrid",crop=FALSE)
# you can take a quick look at this
ggmap(la.map.h)

# now I'm going to bring in some example sites- you'll replace this with the correct function and file path to bring in your sites

sites<-read_xlsx("/Users/abhimehrotra/Desktop/Miller_sponge gps coordinates_summer 2021.xlsx", sheet="Sheet2")

# this assigns the answer to x but doesn't show you what it is in your console. This next line does both:
(x<-793*8)
  
# if you want to use google maps as your base file use this code:
(map2<-ggmap(la.map.h))

# from here on out everything is going to be the same between google map background and the shapefiles. So I will only demonstrate 
# with the shapefile example, because its a bit cleaner and easier to see what changes

# now lets look at changing the axis labels. There are a couple ways that you can do this (a good resource: http://www.cookbook-r.com/Graphs/)
# I am going to show you the easiest way if you aren't changing other things about the axes (which you don't typically have to do with a map)
(map2<-map2+
    ylab("Latitude")+
    xlab("Longitude"))

# other things you might want to do-

# get rid of the grid lines in the background:
map2+
  theme(panel.grid = element_blank())

# move the legend
map2+
  theme(legend.position = "top")

# center the title of the legend
map2+
  theme(legend.title.align = 0.5)

# you can also make something bold or italic

map2+
  theme(legend.title.align = 0.5,
        legend.text = element_text(size=12,family="sans",face = "italic"),
        legend.title = element_text(size=20,family="sans",face = "bold"),
        axis.text = element_text(size=12,family="sans"),
        axis.title= element_text(size=15,family="sans"))

# now to add sites to each map

map2+
  geom_point(aes(x=Long,y=Lat),data=sites,size=2)
