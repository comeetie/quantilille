library(readr)
library(dplyr)
library(sf)
library(osmdata)

listings = read_csv("../data/listings.csv") %>% 
  st_as_sf(coords = c("longitude", "latitude"),crs = 4326, agr = "constant")

plot(listings.l93 %>% st_geometry())

bb      <- listings %>% st_bbox()
q       <- opq(bbox = bb,timeout = 180)
qm      <- add_osm_feature (q, key = 'station',value = 'subway', value_exact = FALSE)
subway  <- osmdata_sf(qm)
listings.l93 = st_transform(listings,2154)
subway.l93 = st_transform(subway$osm_points,2154) %>% filter(!is.na(name),!duplicated(name))

subway.voronoi = st_collection_extract(st_voronoi(x = st_union(subway.l93 %>% st_geometry())))
bb = st_as_sfc(st_bbox(listings.l93),crs=2154)

subway.voronoi = subway.voronoi %>% st_intersection(bb)    
                                      
plot(subway.voronoi)

plot(subway.voronoi,col="green")
plot(listings.l93 %>% st_geometry(), pch = 20, cex = 1,add=TRUE)
plot(subway.l93 %>% st_geometry(), pch = 20, col = "red", add=TRUE, cex = 1)



subway.poly = subway.l93 %>% select(osm_id,name)
subway.poly$geometry = subway.voronoi



listings.voronoi = subway.poly %>% st_join(listings.l93) %>% 
  group_by(osm_id,name.x) %>% 
  summarize(n=n(),
            price_med=median(price,na.rm=TRUE),
            price_mean=mean(price,na.rm=TRUE),
            price_sd=sd(price,na.rm=TRUE))

listings.voronoi %>% arrange(desc(n))

subway.point = subway.l93 %>% select(osm_id,name) %>% left_join(listings.voronoi %>% st_drop_geometry())

bks <- round(quantile(subway.point$price_med, na.rm=TRUE, probs=seq(0,1,0.2)))
bks
subway.point = subway.point %>% mutate(price_cat= cut(price_med,bks))

roads.geom <- st_read(dsn = "data/osmdata/road.shp", quiet = TRUE)
river.geom <- st_read(dsn = "data/osmdata/river.shp", quiet = TRUE)

library(ggplot2)
library(ggspatial)
ggplot(subway.point)+
  geom_sf(aes(size=n,color=price_cat))+
  geom_sf(data = river.geom, colour = "#87cdde",size=2) +
  geom_sf(data = roads.geom, colour = "#666666",size=0.5) +
  geom_sf_text(data=subway.point%>% filter(n>500),aes(label=name))+
  scale_color_brewer("Prix median",palette="Reds")+
  scale_size(name = "Nombre d'accidents",
             breaks = c(1,100,250,500,750),
             range = c(0,5)) +
  coord_sf(crs = 2154, datum = NA,
           xlim = st_bbox(iris.75)[c(1,3)],
           ylim = st_bbox(iris.75)[c(2,4)]) +
  theme_minimal() +
  theme(panel.background = element_rect(fill = "ivory",color=NA),
        plot.background = element_rect(fill = "ivory",color=NA)) +
  labs(title = "Prix median d'une nuitée Air BnB",
       subtitle = "suivant la station de métro la plus proche.",
       caption = "fichier inside AirBnB, ONISR\nantuki & comeetie, 2021",x="",y="")
