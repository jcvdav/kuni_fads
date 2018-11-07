###Mapping FAD Distribution Caribbean
library(pdftools)
library(tidytext)
library(rtweet)
library(stringr)
library(tidyverse)
library(rvest)
library(magrittr)
library(ggmap)
library(stringr)
library(ggplot2)

##Import world map file and cropping to the Caribbean Region

fadlist <- read_csv(here::here("raw_data", "faddata.csv"))

map.world <- map_data("world")

map.world_joined_fad <- left_join(map.world, fadlist, by = c('region' = 'region'))
View(map.world_joined_fad)
caribmap <- ggplot() +
  geom_polygon(data = map.world_joined_fad, aes(x = long, y = lat, group = group))+
  coord_cartesian(xlim = c(-100, -50),ylim = c(30,5)) +
  labs(x = "", y="",title = "")

##Add presence and absence dots for FADs##
##Need to remove Honduras - or figure out why it's being highlighted#
#Unknown
unknown <- c("Aruba","Bahamas")
ll.unknown <- geocode(unknown, source="dsk")
unknown.x <- ll.unknown$lon
unknown.y <- ll.unknown$lat
#Absent
absent <- c("Barbados","Bonaire","Cayman Islands","Cuba","Jamaica","Montserrat","Turks and Caicos Islands")
ll.absent <- geocode(absent, source="dsk")
absent.x <- ll.absent$lon
absent.y <- ll.absent$lat
#Present
present <- c("Anguilla","Antigua","Virgin Islands","Curacao","Dominica","Dominican Republic","Grenada","Guadeloupe","Haiti","Martinique","Puerto Rico","Saba","Saint Barthelemy","Nevis","Saint Lucia","Saint Martin","Saint Vincent","Tobago")
ll.present <- geocode(present, source="dsk")
present.x <- ll.present$lon
present.y <- ll.present$lat

ggplot() +
  theme_bw() +
  geom_polygon(data = map.world_joined_fad, aes(x = long, y = lat, group = group))+
  coord_cartesian(xlim = c(-100, -60),ylim = c(27,10)) +
  geom_point(aes(x=present.x, y=present.y), color="#f24db3", size=5) +
  geom_point(aes(x=unknown.x, y=unknown.y), color="#43b5d8", size=5) +
  geom_point(aes(x=absent.x, y=absent.y), color="#f4d742", size=5) +
  labs(x = "", y="",title = "")

##Attempt to add scaled FAD numbers##
#problem is that it is scaling every single perimeter point around each country - rather than creating an overall number for each country#

ggplot() +
  theme_bw() +
  geom_polygon(data = map.world_joined_fad, aes(x = long, y = lat, group = group))+
  coord_cartesian(xlim = c(-100, -60),ylim = c(27,10)) +
  geom_point(data = map.world_joined_fad, aes(x = long, y = lat, size = fad.number))
