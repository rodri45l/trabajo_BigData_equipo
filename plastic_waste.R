coast_vs_waste <- readr::read_csv("https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2019/2019-05-21/coastal-population-vs-mismanaged-plastic.csv")

mismanaged_vs_gdp <- readr::read_csv("https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2019/2019-05-21/per-capita-mismanaged-plastic-waste-vs-gdp-per-capita.csv")

waste_vs_gdp <- readr::read_csv("https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2019/2019-05-21/per-capita-plastic-waste-vs-gdp-per-capita.csv")




  coast_vs_waste <- coast_vs_waste %>% drop_na()
    mismanaged_vs_gdp <- mismanaged_vs_gdp %>% drop_na()
      waste_vs_gdp <- waste_vs_gdp %>% drop_na()
      coast_vs_waste2 <- coast_vs_waste %>% select(`Mismanaged plastic waste (tonnes)`, `Coastal population`)
      cor(coast_vs_waste2)

      library(plotly)

      j <-waste_vs_gdp %>% ggplot(aes(`GDP per capita, PPP (constant 2011 international $) (constant 2011 international $)`,`Per capita plastic waste (kilograms per person per day)`)) +
          geom_point() +
          geom_smooth()
    j
 waste_vs_gdp2 <- waste_vs_gdp %>% select(`Per capita plastic waste (kilograms per person per day)`,`GDP per capita, PPP (constant 2011 international $) (constant 2011 international $)`) %>% drop_na()
    cor(waste_vs_gdp2,method = "pearson")
str(waste_vs_gdp2)
    mismanaged_vs_gdp2 <- mismanaged_vs_gdp %>% select(`Per capita mismanaged plastic waste (kilograms per person per day)`,`GDP per capita, PPP (constant 2011 international $) (Rate)`)
    cor(mismanaged_vs_gdp2, method = "pearson")

