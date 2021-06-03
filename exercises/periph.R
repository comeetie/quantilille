library(sfnetworks)

periph_simple= roads %>% 
  filter(!is.na(name))%>% 
  filter(name %in% c("Boulevard Périphérique Intérieur", "Pont Masséna","Tunnel Lilas","Pont Amont","Pont Aval")) %>% 
  select(name)



net  = as_sfnetwork(st_geometry(periph_simple))
nets = convert(net,to_spatial_smooth)

nets = nets %>% 
  activate(edges) %>% 
  mutate(length=st_length(x)) %>% 
  arrange(desc(length)) %>% 
  mutate(lid=1:n()) %>% 
  filter(lid %in% c(1,2,3,5))

plot(nets)
lines.geom = nets %>% activate(edges) %>% st_geometry() 
lines = st_sf(lines.geom,id=1:length(lines.geom))
plot(lines)





points_eqd = lines  %>% st_line_sample(density = 1/100)
lines_eqd = lines  %>% st_line_sample(density = 1/100) %>% st_cast("LINESTRING")

geo_col = lwgeom::st_split(lines_eqd,points_eqd)
lines_final.geom = st_collection_extract(geo_col,type = "LINESTRING")
lines_final = st_sf(lines_final.geom,id=1:length(lines_final.geom))

plot(lines_final)


periph_count= st_sf(lines_final.geom %>% st_buffer(50),id=1:length(lines_final.geom)) %>% 
  st_join(accidents.2019.paris %>% filter(!duplicated(Num_Acc))) %>% 
  filter(!is.na(Num_Acc)) %>% 
  count(id)

st_geometry(periph_count)=lines_final.geom[match(periph_count$id,lines_final$id)]


ggplot(periph_count) + 
  g#gspatial::annotation_map_tile(zoom=13,type="stamenbw") + 
  geom_sf(data=roads.geom, colour = "#666666",size=0.5)+
  geom_sf(aes(color=n),size=3)+
  scale_color_distiller("",palette = "Reds",direction=1)+
  coord_sf(crs = 2154, datum = NA) +
  theme_minimal() +
  theme(panel.background = element_rect(fill = "white",color=NA),
        plot.background = element_rect(fill = "white",color=NA)) +
  labs(title = "Nombre d'accidents sur le périphérique",
       subtitle = "par portion de 100m",
       caption = "fichier BAAC 2019, ONISR\nantuki & comeetie, 2021",x="",y="")

