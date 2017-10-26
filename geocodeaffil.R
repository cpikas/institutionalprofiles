#bonus file - maps where author organizations

library("ggmap")
library("maps")
library(readr)
library(ggplot2)
# cheatsheet: https://www.nceas.ucsb.edu/~frazier/RSpatialGuides/ggmap/ggmapCheatsheet.pdf
# http://stackoverflow.com/questions/11201997/world-map-with-ggmap?rq=1


#example how to get a geocode:
geocode('harbin institute of technology', output="latlona")


#known issues:

#best to update Korea, Republic of and change to South Korea 
#best to update Victoria (state), Australia, from VIC Victoria

#get affil names and times each
apl80_2015affils5 <- read_csv("~/apltopart15/apl80-2015cleanisheraffils5.csv")


get_geocodes<-function(placename){
  
  temp<-geocode(placename,output="latlon")
  
  return(temp) }

test<-get_geocodes(apl80_2015affils5$Affiliation[1:5])

affil.geo<-get_geocodes(apl80_2015affils5$Affiliation)

#didn't get some 47 of them

affilgeoerror<-warnings()

Affils.gcoded<-cbind(apl80_2015affils5,affil.geo)

#lat and longs are characters, need to be integers
Affils.gcoded$lon<-as.numeric(Affils.gcoded$lon)
Affils.gcoded$lat<-as.numeric(Affils.gcoded$lat)

#write to save
write.csv(Affils.gcoded,"apl80-2015-5geocoded.csv")

#fill in any missing ones that looked more interesting (us, national labs, etc)

#saving as is and then keeping the complete cases
Affils.gcodedm<-Affils.gcoded
Affils.gcoded<-Affils.gcoded[complete.cases(Affils.gcoded),]

# here's the original how-to:
# http://rpsychologist.com/working-with-shapefiles-projections-and-world-maps-in-ggplot
# you have to follow the link to get the shape files and extract them into the correct directory

# read shapefile
wmap <- readOGR(dsn="ne_110m_land.shp", layer="ne_110m_land")
# convert to dataframe
wmap_df <- fortify(wmap)

# create a blank ggplot theme
theme_opts <- list(theme(panel.grid.minor = element_blank(),
                         panel.grid.major = element_blank(),
                         panel.background = element_blank(),
                         plot.background = element_rect(fill="#e6e8ed"),
                         panel.border = element_blank(),
                         axis.line = element_blank(),
                         axis.text.x = element_blank(),
                         axis.text.y = element_blank(),
                         axis.ticks = element_blank(),
                         axis.title.x = element_blank(),
                         axis.title.y = element_blank(),
                         plot.title = element_text(size=22)))

# plot map
ggplot(wmap_df, aes(long,lat, group=group)) + 
  geom_polygon() + 
  labs(title="World map (longlat)") + 
  coord_equal() + 
  theme_opts

# reproject from longlat to robinson
wmap_robin <- spTransform(wmap, CRS("+proj=robin"))
wmap_df_robin <- fortify(wmap_robin)
ggplot(wmap_df_robin, aes(long,lat, group=group)) + 
  geom_polygon() + 
  labs(title="World map (robinson)") + 
  coord_equal() +
  theme_opts



# add country borders
countries <- readOGR("ne_110m_admin_0_countries.shp", layer="ne_110m_admin_0_countries") 
countries_robin <- spTransform(countries, CRS("+init=ESRI:54030"))
countries_robin_df <- fortify(countries_robin)

ggplot(wmap_df_robin, aes(long,lat, group=group)) + 
  geom_polygon(fill="white") +
  geom_polygon(data=countries_robin_df, aes(long,lat, group=group, fill=hole)) + 
  geom_path(data=countries_robin_df, aes(long,lat, group=group, fill=hole), color="white", size=0.3) +
  labs(title="World map (Robinson)") + 
  coord_equal() + 
  theme_opts +
  scale_fill_manual(values=c("black", "white"), guide="none") # change colors & remove legend



places_robin_df<-project(cbind(Affils.gcoded$lon, Affils.gcoded$lat), proj="+init=ESRI:54030")
places_robin_df <- as.data.frame(places_robin_df)
names(places_robin_df) <- c("LONGITUDE", "LATITUDE")
places_robin_df$arts<-Affils.gcoded$Articles

ggplot(wmap_df_robin, aes(long,lat, group=group)) + 
  geom_polygon(fill="white") +
  geom_polygon(data=countries_robin_df, aes(long,lat, group=group, fill=hole)) + 
  geom_point(data=places_robin_df, aes(LONGITUDE, LATITUDE, group=NULL, fill=NULL, size=places_robin_df$arts/10), color="#32caf6", alpha=I(7/10)) +
  geom_path(data=countries_robin_df, aes(long,lat, group=group, fill=hole), color="white", size=0.3) +
  labs(title="Collaborating Organizations") + 
  coord_equal() + 
  theme_opts +
  scale_fill_manual(values=c("black", "white"), guide="none")+
  scale_size_continuous(range=c(1,20), guide="none")# change colors & remove legend

####
#below is re-done for open circles
ggplot(wmap_df_robin, aes(long,lat, group=group)) + 
  geom_polygon(fill="white") +
  geom_polygon(data=countries_robin_df, aes(long,lat, group=group, fill=hole)) + 
  geom_point(data=places_robin_df,shape=21, aes(LONGITUDE, LATITUDE, group=NULL, fill=NULL, size=places_robin_df$arts/10), color="#32caf6", alpha=I(10/10)) +
  geom_path(data=countries_robin_df, aes(long,lat, group=group, fill=hole), color="white", size=0.3) +
  labs(title="Organizations") + 
  coord_equal() + 
  theme_opts +
  scale_fill_manual(values=c("black", "white"), guide="none")+
  scale_size_continuous(range=c(1,20), guide="none")# change colors & remove legend


