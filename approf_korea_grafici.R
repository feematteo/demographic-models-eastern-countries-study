library(readxl)
Korea_approf_data <- read_excel("approfondimento_korea/dati_korea.xlsx")
plot(Korea_approf_data,type="l",main="Total fertility rate of South Korea 1900-2022",
     col="blue")
text(Korea_approf_data$`Total fertility rate of South Korea 1900-2020`, Korea_approf_data$...2, labels = round(Korea_approf_data$...2, 2), pos = 3)

Korea_opiniom_data <- read_excel("approfondimento_korea/dati_korea_2.xlsx")


barplot(rbind(Korea_opiniom_data$Singles, Korea_opiniom_data$Married), beside = TRUE, names.arg = Korea_opiniom_data$`Opinion on the reasons of decreased birth rate in South Korea in 2018, by marital status`,
        legend.text = c("Single", "Married"), args.legend = list(x = "topleft"))
text(x = barplot(Korea_opiniom_data$Singles, beside = TRUE, plot = FALSE) + 3.50, y = Korea_opiniom_data$Singles,
     labels = Korea_opiniom_data$Singles, pos = 3)
text(x = barplot(Korea_opiniom_data$Married, beside = TRUE, plot = FALSE) - 1.50, y = Korea_opiniom_data$Married,
     labels = Korea_opiniom_data$Married, pos = 3)

