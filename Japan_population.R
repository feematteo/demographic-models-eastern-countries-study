library(readxl)
Japan_population <- read_excel("Japan_population_data.xlsx")
plot(y=Japan_population$Population,x=Japan_population$year, main ="Japan population 1960-2021",ylab = "population",xlab = "year",
     col="blue", pch=19)
