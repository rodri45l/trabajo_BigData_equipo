library(tidyverse)
library(rio)
library(plotly)
library(janitor)
library(gganimate)
library(viridis)
library("sf")

url <-("https://raw.githubusercontent.com/rodri45l/plasticwaste/main/trabajo_BigData_equipo/datos/renpwr.csv")

df <- import(url)

df_pwr <- df %>% select(-c(`Series Name`,`Series Code`)) %>% rename(sovereignt = `Country Name`)


#- puedes dejar tu trozo de código para arreglar los nombres, quiza sea lo mejor, 
names(df_pwr) <- stringr::str_remove(string = names(df_pwr), pattern = "\\s.*") 



df_pwr$sovereignt <- recode(df_pwr$sovereignt, 
                            "United States"="United States of America", 
                            "Russian Federation" = "Russia",
                            "Venezuela, RB" = "Venezuela",
                            "Congo, Dem. Rep." = "Democratic Republic of the Congo" ,
                            "	Congo, Rep."="Republic of Congo"	 ,
                            "Egypt, Arab Rep." = "Egypt" ,
                            "Tanzania" = "United Republic of Tanzania",
                            "Cote d'Ivoire" = "Ivory Coast",
                            "Yemen, Rep." = "Yemen",
                            "Iran, Islamic Rep." = "Iran",
                            "Syrian Arab Republic" = "Syria",
                            "Slovak Republic" = "Slovakia",
                            "Serbia" = "Republic of Serbia",
                            "North Macedonia" = "Macedonia",
                            "Kyrgyz Republic" = "Kyrgyzstan",
                            "Korea, Dem. People’s Rep" = "North Korea",
                            "Korea, Rep." = "South Korea",
                            "Lao PDR" = "Laos")


#- estabas usando creo que funciones de Datatable (buen paquete pero no lo hemos visto) No , no era de Datatable puesto que no estaba cargado el paquete
df_pwr2 <- df_pwr %>% pivot_longer(cols = 3:28, names_to = "year") %>% 
           mutate(year = as.numeric(year)) %>% 
           mutate(value = as.numeric(value)) %>% 
          filter(year == 2015)


#- cargamos geometrias
world <- rnaturalearth::ne_countries(scale = "medium", returnclass = "sf")
world_points<- st_centroid(world)
world_points <- cbind(world, st_coordinates(st_centroid(world$geometry)))






#- estabas usando merge, es de R-base, mekjor usar las joinns de dplyr
df_inner_pwr <- inner_join(world, df_pwr2, by= c("sovereignt" = "sovereignt"))

#Primer mapa simple
p <- ggplot(data = df_inner_pwr) + geom_sf() +
  labs(title = "Gráfico 1: Mapa del mundo energias renovables",
       caption = "Datos provenientes de World Bank")


p + geom_sf(aes(fill = value, color =value)) + scale_fill_viridis(direction = 1, label = scales::number,begin = 0, end = 0.9) + scale_color_viridis(direction = 1, label = scales::number,begin = 0, end = 0.9) 


#Intento mapa animado 
df_pwr4 <- df_pwr %>% pivot_longer(cols = 3:28, names_to = "year") %>% 
  mutate(year = as.numeric(year)) %>% 
  mutate(value = as.numeric(value))

df_pwr4 <- inner_join(world, df_pwr4, by= c("sovereignt" = "sovereignt"))

names(df_pwr4)





o <- ggplot() + geom_sf(data = df_pwr4, aes(fill = value)) +  # Aqui ya salta el error , stat_sf no encuentra la geometria
  labs(title = "Gráfico 1: Mapa del mundo energias renovables",
       caption = "Datos provenientes de World Bank")


o #- si sale el plot pero supongo que no vale xq se graficaran todos lo


#- yo creo que algún geom_sf() te sobra
#- supongo q algo se me escapa pero no creo que haya que usar color
oo <- o + geom_sf(data = df_pwr4, aes(fill = value, color = value)) + scale_fill_viridis(direction = 1, label = scales::number,begin = 0, end = 0.9) + scale_color_viridis(direction = 1, label = scales::number,begin = 0, end = 0.9)
 facet_wrap(year)

oo #- sale

oo + facet_wrap(vars(year)) #- sale




#- parece que no hay q usar  transition_reveal con geom_sf(). He buscado rapido e igual puede ser transition_time pero no estoy seguro
df_pwr9 <- df_pwr4 %>% select(sovereignt, year, value)
p <- ggplot() + geom_sf(data = df_pwr9, aes(fill = value), color = "black") + scale_fill_viridis(direction = 1, label = scales::number,begin = 0, end = 0.9)   
p

p + transition_time(year) + labs(title = "year: {as.integer(frame_along)}")
  