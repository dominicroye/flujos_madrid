# packages
library(tidyverse)
library(sf)
library(readxl)
library(ggtext)
library(rnaturalearth)

# limites provincias
prov <- ne_states("Spain", returnclass = "sf") %>% 
            st_make_valid()

# limites areas INE
areas <- st_read("./data/celdas_marzo_2020.shp") %>% 
          st_transform(4326) %>% 
           st_make_valid()

# intersección  
areas <- st_intersection(areas, prov)

# extract coordinadas de areas
areas_center <- st_centroid(areas) %>% 
                  select(ID_GRUPO, region) %>% 
                    cbind(st_coordinates(.))

# excluir Melilla, Ceuta
areas_center <- filter(areas_center, !region %in% c("Ceuta", "Melilla"))

# renombrar para distino, origen
center_distino <- rename(areas_center, x_distino = X,
                         y_distino = Y)

center_origen <- rename(areas_center, x_origin = X,
                        y_origin = Y) 

# distino 
flujo_distino <- read_csv2("./data/FlujosDestino100+_M1_NOV.csv", locale = locale(encoding = "Latin1")) %>% 
                      select(CELDA_ORIGEN, CELDA_DESTINO, FLUJO)

flujo_distino <- left_join(flujo_distino, center_distino, by = c("CELDA_DESTINO"="ID_GRUPO")) %>% 
                    left_join(center_origen, by = c("CELDA_ORIGEN"="ID_GRUPO"))

flujo_distino <- select(flujo_distino, -geometry.x, -geometry.y, -region.y, region = region.x)

# origen
flujo_orig <- read_csv2("./data/FlujosOrigen100+_M1_NOV.csv", locale = locale(encoding = "Latin1")) %>% 
                   select(CELDA_ORIGEN, CELDA_DESTINO, FLUJO)

flujo_orig <- left_join(flujo_orig, center_distino, by = c("CELDA_DESTINO"="ID_GRUPO")) %>% 
                 left_join(center_origen, by = c("CELDA_ORIGEN"="ID_GRUPO"))

flujo_orig <- select(flujo_orig, -geometry.x, -geometry.y, -region.y, region = region.x)


flujo <- bind_rows(flujo_orig, flujo_distino)


#descarga de fuente
sysfonts::font_add_google("Montserrat", "Montserrat")


ggplot() +
  geom_sf(data = filter(areas, region == "Madrid"), fill = "transparent",
          colour = "grey80", size = 0.2) +
  geom_segment(data = filter(flujo, region == "Madrid"), 
               aes(x = x_origin, y = y_origin, 
                   xend = x_distino, yend = y_distino,
                   alpha = FLUJO), 
               col = "#F5D300", 
               size = 0.15,
               show.legend = FALSE) +
  scale_alpha_continuous(range = c(0.2, 0.7)) +
  labs(tag = "Madrid",
       title = "<span style='color:#F5D300;'><strong>Flujos</strong></span> de movilidad en Madrid entre áreas pequeñas en noviembre 2019",
       caption = "Dominic Royé (@dr_xeo) | Data: INE") +
  theme_void() +
  theme(panel.background = element_rect(fill = "black"),
        plot.background = element_rect(fill = "black"),
        plot.title = element_textbox(family = "Montserrat", hjust = 0.5, vjust = -1, colour = "white", size = 15),
        plot.caption = element_text(family = "Montserrat", colour = "white", hjust = .8),
        plot.tag = element_text(family = "Montserrat", colour = "white"),
        plot.tag.position = c(0.5, 0.8))+
  coord_sf()









