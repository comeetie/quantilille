library(dplyr)

### Charger les datasets bruts 

#1. La rubrique CARACTERISTIQUES qui décrit les circonstances générales 
#de l’accident
caracteristiques.2019 <- read.csv2("data/caracteristiques-2019.csv")
#2. La rubrique LIEUX qui décrit le lieu principal de l’accident
#même si celui-ci s’est déroulé à une intersection
lieux.2019 <- read.csv("data/lieux-2019.csv", sep=";")
#3. La rubrique VEHICULES impliqués
vehicules.2019 <- read.csv("data/vehicules-2019.csv", sep=";",
                           stringsAsFactors=TRUE)
#4. La rubrique USAGERS impliqués
usagers.2019 <- read.csv("data/usagers-2019.csv",
                         sep=";",
                         stringsAsFactors=TRUE)


### Constituer le dataset du cours/TP
accidents.2019 <- left_join(
  caracteristiques.2019 %>%
    select(Num_Acc, lat, long, com, int, lum),
  usagers.2019 %>%
    select(Num_Acc,grav,sexe,an_nais)
)
accidents.2019.paris <- accidents.2019 %>% filter(substr(com,1,2)==75)
#saveRDS(accidents.2019.paris,"data/accidents2019_paris.RDS")

# Transformation en objet sf
library(sf)
accidents.2019.paris <- st_as_sf(accidents.2019.paris,
                                 coords = c("long", "lat"),
                                 crs = 4226, agr = "constant") %>% 
  st_transform(2154)
#st_write(accidents.2019.paris,"data/accidents2019_paris.shp")

######## Extraction du fond de carte des iris (IGN)
#https://geoservices.ign.fr/documentation/diffusion/telechargement-donnees-libres.html#contoursiris
#donnees 2019
iris.fra <- st_read("data/shp_iris/CONTOURS-IRIS.shp", stringsAsFactors = F) 
iris.75 <- iris.fra[substr(iris.fra$INSEE_COM, 1,2)=="75",c("CODE_IRIS","INSEE_COM")]
#saveRDS(iris.75,"data/iris_75.RDS")
#st_write(iris.75,"data/iris_75.shp")
