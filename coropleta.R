rm(list = ls())


#install.packages("viridis")
library(viridis)

library("sf")
world_points<- st_centroid(world)
world_points <- cbind(world, st_coordinates(st_centroid(world$geometry)))
#mapa co2 per capita
library(tidyverse)
dco2_pc<- read_csv("./Datos/co2_pc.csv")
world <- rnaturalearth::ne_countries(scale = "medium", returnclass = "sf")


dco2_pc[ dco2_pc == ".." ] <- NA




dco_pc <- dco2_pc %>% select(`Country Name`,`2010 [YR2010]`)
dfo3_pc <- dco2_pc %>% rename(sovereignt = `Country Name`,
                        co2_pc = `2010 [YR2010]`
                        
)



dfo3_pc$sovereignt <- recode(dfo3_pc$sovereignt, 
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
             "Lao PDR" = "Laos"
)


df_inner_pc <- merge(world,dfo3_pc,by="sovereignt",all=TRUE)
df_co2_pc <-  transform(df_inner_pc, co2_pc = as.numeric(co2_pc)   
)

p <- ggplot(data = df_co2_pc) + geom_sf() +
  labs(title = "Gráfico 1: Mapa del mundo",
       caption = "Datos provenientes de rnaturalearth")
p + geom_sf(aes(fill = co2_pc)) + scale_fill_viridis_c(option = "B", trans = "sqrt") + geom_text(data= world_points,aes(x=X, y=Y, label=name),
            color = "black", fontface = "bold")

p+ geom_sf(aes(fill = co2_pc, color =co2_pc)) + scale_fill_viridis(direction = -1, label = scales::dollar) + 
  scale_color_viridis(direction = -1, label = scales::dollar) 


#mapa co2 



dco2<- read_csv("./Datos/co2.csv")
world <- rnaturalearth::ne_countries(scale = "medium", returnclass = "sf")


dco2[ dco2 == ".." ] <- NA




dco2 <- dco2 %>% select(`Country Name`,`2010 [YR2010]`)
dfo3 <- dco2 %>% rename(sovereignt = `Country Name`,
                        co2 = `2010 [YR2010]`
                        
)



dfo3$sovereignt <- recode(dfo3$sovereignt, 
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
                          "Lao PDR" = "Laos"
)


df_inner <- merge(world,dfo3,by="sovereignt",all=TRUE)
df_co2 <-  transform(df_inner, co2 = as.numeric(co2)   
)

p <- ggplot(data = df_co2) + geom_sf() +
  labs(title = "Mapa emisiones de co2",
       caption = "Datos provenientes de World Bank")
p + geom_sf(aes(fill = co2, color =co2)) + scale_fill_viridis(direction = -1, label = scales::number,begin = 0, end = 0.8,) + 
  scale_color_viridis(direction = -1, label = scales::number,begin = 0, end = 0.8) 

df_zoom <- df_co2 %>% filter(continent == "Europe") #%>% filter(!(sovereignt == "Russia"))
t <- ggplot(data = df_zoom) + geom_sf() +
  labs(title = "Emisiones de co2 Europa",
       caption = "Datos provenientes de World Bank")
t + geom_sf(aes(fill = co2, color =co2)) + scale_fill_viridis(direction = -1, label = scales::number,begin = 0, end = 0.8,) + 
  scale_color_viridis(direction = -1, label = scales::number,begin = 0, end = 0.8) +
  coord_sf(xlim = c(-35, 50), ylim = c(30, 80))


