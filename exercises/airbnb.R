library(readr)
library(dplyr)
library(sf)
library(osmdata)
listings = read_csv("../data/listings.csv") %>% st_as_sf(coords = c("longitude", "latitude"),
                                                         crs = 4326, agr = "constant")
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



listings.voronoi = subway.poly %>% st_join(listings.l93) %>% group_by(osm_id,name.x) %>% summarize(n=n(),price=mean(price,na.rm=TRUE))

listings.voronoi %>% arrange(desc(n))

subway.point = subway.l93 %>% select(osm_id,name) %>% left_join(listings.voronoi %>% st_drop_geometry())

library(ggplot2)
library(ggspatial)
ggplot(subway.point)+geom_sf(aes(size=n,color=price))
