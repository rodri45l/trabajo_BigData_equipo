library(tidyverse)
library(gganimate)

library(lubridate)
pct <- function(x) {((x/lag(x)
)-1)*2}
pct2 <- function(x) {((x/lag(x)
)-1)*100}


temp_country <-read_csv("./Datos/GlobalLandTemperaturesByCountry.csv")

temp_2000 <- subset(temp_country,dt> "1899-12-01")

climate_change <- temp_2000 %>% 
  mutate_at(vars(dt), funs(year, month, day)) %>% drop_na() %>% filter(Country == "Spain")

climate_change$month[climate_change$month == 1]<- "01-Enero"
climate_change$month[climate_change$month == 2]<- "02-Febrero"
climate_change$month[climate_change$month == 3]<- "03-Marzo"
climate_change$month[climate_change$month == 4]<- "04-Abril"
climate_change$month[climate_change$month == 5]<- "05-Mayo"
climate_change$month[climate_change$month == 6]<- "06-Junio"
climate_change$month[climate_change$month == 7]<- "07-Julio"
climate_change$month[climate_change$month == 8]<- "08-Agosto"
climate_change$month[climate_change$month == 9]<- "09-Septiembre"
climate_change$month[climate_change$month == 10]<- "10-Octubre"
climate_change$month[climate_change$month == 11]<- "11-Noviembre"
climate_change$month[climate_change$month == 12]<- "12-Diciembre"



ggplot(climate_change, aes(year, AverageTemperature, size = AverageTemperatureUncertainty , color = month )) + geom_point(alpha = 0.4, show.legend = FALSE) +
  facet_wrap(~month) +

  labs(title = 'year: {as.integer(frame_time)}', x = 'Año', y = 'Temperatura Media') +
  transition_time(year) +
  ease_aes('linear')




#paises <- c("United States","China")
#temp_2000 <-temp_2000 %>% filter(Country == paises)
t_df <- temp_2000 %>% drop_na() %>%
mutate(month = format(dt, "%m"), year = format(dt, "%Y")) %>%
group_by(month, year) %>%
summarise(mean = mean(AverageTemperature))




t_df2 <- t_df %>% group_by(year) %>% summarise(tmean= mean(mean))
 t_df2 <-  transform(t_df2, year = as.numeric(year))

 w_df2 <- w_df %>% filter(year > 1899)
 w_df3 <- w_df2  %>%  mutate_each(funs(pct), n) %>% drop_na()
t_df3 <- t_df2  %>% mutate_each(funs(pct2), tmean) %>% drop_na()
w_df4 <- w_df3 %>% mutate(tco2 = cumsum(n))
t_df4 <- t_df3  %>% mutate(temp = cumsum(tmean))
 




y <-ggplot() +
  
  
  geom_line(data =w_df4,aes(year,tco2),colour = "green") +
  #geom_smooth(data = w_df4,aes(year,tco2)) +
 #theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) +
   #geom_smooth(data = t_df4,aes(year,temp),colour= "purple") +
    geom_line(data = t_df4,aes(year,temp,group=1),colour = "purple2") 
  
      
  
  
y+ transition_reveal(year) +
  labs(title = "Año: {as.integer(frame_along)}",
       x = "Año",
       y = "Temperatura y CO2")


wt_df <- inner_join(w_df2,t_df2)
l<- ggplot(wt_df,aes(n,tmean)) +

  geom_smooth() 

l + 
  stat_density_2d(aes(fill = ..level..), geom="polygon")+
  scale_fill_gradient(low="blue", high="red")


l +  geom_bin2d()



  
smoothScatter(w_df2,ylab = "Em.Co2", xlab = "Año")
  
  
  
  
  
  cor(wt_df, method = "spearman", use = "complete.obs")
  
  







