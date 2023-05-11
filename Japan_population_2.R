library(readxl)
Japan_population <- read_excel("Japan_population_data.xlsx")

plot(y=Japan_population$Population,x=Japan_population$year, main ="Japan population-projection 1960-2050",ylab = "population",xlab = "year",
     col="blue", pch=19)
text(x =1980,y = 100000000,
     labels = "data source statista + www.ipss.go.jp", pos = 4, cex = 0.8)


Japan.over65 <- read_excel("Japan_over65_data.xlsx")
plot(Japan.over65, main ="Japan population over 65",ylab = "% over 65",xlab = "year",
     col="blue", pch=19)

Japan.population.distribution.age <- read_excel("Japan_population_age_distribution.xlsx")
# 
# bar_width <- 0.2
# 
# barplot(height =c(Japan.population.distribution.age$`0-14 years`,Japan.population.distribution.age$`15-64 years`,Japan.population.distribution.age$`65 years +`),
#         beside = TRUE,
#         col = c("red", "blue", "green"),
#         xlab = "Year",
#         ylab = "Population",
#         main = "Population by Age Group" ,names.arg =Japan.population.distribution.age$year,
#         ylim = c(0, max(c(Japan.population.distribution.age$`0-14 years`, Japan.population.distribution.age$`15-64 years`, 
#                           Japan.population.distribution.age$`65 years +`))) + 50)
# legend("topright", legend = c("0-14", "15-64", "65+"), fill = c("red", "blue", "green"))
# 
# 
# barplot(height = cbind(Japan.population.distribution.age$`0-14 years`, Japan.population.distribution.age$`15-64 years`, Japan.population.distribution.age$`65 years +`),
#         beside = TRUE,
#         col = c("red", "blue", "green"),
#         xlab = "Year",
#         ylab = "Population",
#         main = "Population by Age Group",
#         names.arg = Japan.population.distribution.age$year,
#         ylim = c(0, max(c(Japan.population.distribution.age$`0-14 years`, Japan.population.distribution.age$`15-64 years`, Japan.population.distribution.age$`65 years +`))) + 50)




# Convert list to data frame
df <- data.frame(year = Japan.population.distribution.age$year,
                 `0-14 years` = Japan.population.distribution.age$`0-14 years`,
                 `15-64 years` = Japan.population.distribution.age$`15-64 years`,
                 `65+ years` = Japan.population.distribution.age$`65 years +`)

# Load ggplot2 library
library(ggplot2)

# Melt data frame to long format
library(reshape2)
df_melted <- melt(df, id.vars = "year")

# Create bar plot using ggplot2
ggplot(df_melted, aes(x = year, y = value, fill = variable)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(x = "Year", y = "Population", fill = "Age Group") +
  ggtitle("Population by Age Group in Japan") +
  scale_fill_manual(values = c("red", "blue", "green")) +
  theme_bw()


Japan.over.100 <- read_excel("Japan_over_100.xlsx")

Japan.over.100_df <- data.frame(Japan.over.100)

# Plot with ggplot
library(ggplot2)

ggplot(Japan.over.100_df, aes(x = Year)) +
  geom_bar(aes(y = Male), stat = "identity", fill = "blue", alpha = 0.5) +
  geom_bar(aes(y = Female), stat = "identity", fill = "pink", alpha = 0.5) +
  labs(title = "Population Over 100 Years in Japan",
       x = "Year",
       y = "Population",
       fill = "Gender") +
  scale_y_continuous(labels = function(x) format(x, scientific = FALSE)) +
  scale_fill_manual(values = c("blue", "pink")) +
  theme_bw()
