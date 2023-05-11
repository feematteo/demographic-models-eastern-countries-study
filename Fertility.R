library(demography)

S.korea <- hmd.mx("KOR", "email", "password", "S.korea") #2003-2020
Taiwan<- hmd.mx("TWN", "email", "password!", "Taiwan") #1970 - 2020
Japan<- hmd.mx("JPN", "email", "password!", "Japan") #1947 - 2020
HongKong<- hmd.mx("HKG", "email", "password!", "HongKong") #1986 - 2020

#install.packages("pyramid")
library(pyramid)

#Piramidi età #----

####Japan #----
par(mfrow=c(1,2))
Japan.female.1947 <- Japan$pop$female[,1]
Japan.male.1947 <- Japan$pop$male[,1]  
Japan.age <- Japan$age  
Japan.data.pyramid.1947 <- data.frame(Japan.male.1947,Japan.female.1947,Japan.age)
pyramid(Japan.data.pyramid.1947,Llab="Males",Rlab="Females",Clab="Ages",Laxis=seq(0,1500000,len=3),
                AxisFM="d", AxisBM=",", Csize=0.8,  Cstep=10, 
                main="Age pyramid, Japan. 1947")
  
Japan.female.2020 <- Japan$pop$female[,75]
Japan.male.2020 <- Japan$pop$male[,75]  
Japan.data.pyramid.2020 <- data.frame(Japan.male.2020,Japan.female.2020,Japan.age)
pyramid(Japan.data.pyramid.2020,Llab="Males",Rlab="Females",Clab="Ages",Laxis=seq(0,1500000,len=3),
        AxisFM="d", AxisBM=",", Csize=0.8,  Cstep=10, 
        main="Age pyramid, Japan. 2020")
####Taiwan #----
#par(mfrow=c(1,2))
Taiwan.female.1970 <- Taiwan$pop$female[,1]
Taiwan.male.1970 <- Taiwan$pop$male[,1]
Taiwan.age <- Taiwan$age
Taiwan.data.pyramid.1970 <- data.frame(Taiwan.male.1970,Taiwan.female.1970,Taiwan.age)
pyramid(Taiwan.data.pyramid.1970,Llab="Males",Rlab="Females",Clab="Ages",Laxis=seq(0,350000,len=3),
        AxisFM="d", AxisBM=",", Csize=0.8,  Cstep=10, 
        main="Age pyramid, Taiwan. 1970")

Taiwan.female.2020 <- Taiwan$pop$female[,50]
Taiwan.male.2020 <- Taiwan$pop$male[,50]
Taiwan.data.pyramid.2020 <- data.frame(Taiwan.male.2020,Taiwan.female.2020,Taiwan.age)
pyramid(Taiwan.data.pyramid.2020,Llab="Males",Rlab="Females",Clab="Ages",Laxis=seq(0,350000,len=3),
        AxisFM="d", AxisBM=",", Csize=0.8,  Cstep=10, main="Age pyramid, Taiwan. 2020")
####HongKong #----
#par(mfrow=c(1,2))
HongKong.female.1986 <- HongKong$pop$female[,1]
HongKong.male.1986 <- HongKong$pop$male[,1]
HongKong.age <- HongKong$age
HongKong.data.pyramid.1986 <- data.frame(HongKong.male.1986,HongKong.female.1986,HongKong.age)
pyramid(HongKong.data.pyramid.1986,Llab="Males",Rlab="Females",Clab="Ages",Laxis=seq(0,75000,len=3),
        AxisFM="d", AxisBM=",", Csize=0.8,  Cstep=10, 
        main="Age pyramid, HongKong. 1986")

HongKong.female.2020 <- HongKong$pop$female[,35]
HongKong.male.2020 <- HongKong$pop$male[,35]
HongKong.data.pyramid.2020 <- data.frame(HongKong.male.2020,HongKong.female.2020,HongKong.age)
pyramid(HongKong.data.pyramid.2020,Llab="Males",Rlab="Females",Clab="Ages",Laxis=seq(0,75000,len=3),
        AxisFM="d", AxisBM=",", Csize=0.8,  Cstep=10, main="Age pyramid, HongKong. 2020")

####S.korea #----
#par(mfrow=c(1,2))
S.korea.female.2003 <- S.korea$pop$female[,1]
S.korea.male.2003 <- S.korea$pop$male[,1]
S.korea.age <- S.korea$age
S.korea.data.pyramid.2003 <- data.frame(S.korea.male.2003,S.korea.female.2003,S.korea.age)
pyramid(S.korea.data.pyramid.2003,Llab="Males",Rlab="Females",Clab="Ages",Laxis=seq(0,450000,len=3),
        AxisFM="d", AxisBM=",", Csize=0.8,  Cstep=10, 
        main="Age pyramid, S.korea 2003")

S.korea.female.2020 <- S.korea$pop$female[,18]
S.korea.male.2020 <- S.korea$pop$male[,18]
S.korea.data.pyramid.2020 <- data.frame(S.korea.male.2020,S.korea.female.2020,S.korea.age)
pyramid(S.korea.data.pyramid.2020,Llab="Males",Rlab="Females",Clab="Ages",Laxis=seq(0,450000,len=3),
        AxisFM="d", AxisBM=",", Csize=0.8,  Cstep=10, main="Age pyramid, S.korea. 2020")


#
#TFR   #----
Tfr.Japan <- read.table("data_txt/JPNtfrRR.txt",header = T,skip = 2)
Tfr.S.korea <- read.table("data_txt/KORtfrRR.txt",header = T,skip = 2)
Tfr.Taiwan <- read.table("data_txt/TWNtfrRR.txt",header = T,skip = 2)

#TFR plot #-----
# add manual legend (!!!!!!!!!!!)

# Japan-blue   S.korea-red    Taiwan-green

library(ggplot2)

ggplot() +
  geom_line(data = Tfr.Japan, aes(x = Year, y = TFR), color = "blue", linetype = "solid") +
  geom_line(data = Tfr.S.korea, aes(x = Year, y = TFR), color = "red", linetype = "solid") +
  geom_line(data = Tfr.Taiwan, aes(x = Year, y = TFR), color = "forestgreen", linetype = "solid") +
  scale_color_manual(values = c("blue", "red", "forestgreen"), 
                     labels = c("Japan", "S. Korea", "Taiwan")) +
  labs(x = "Year", y = "TFR", title = "Period TFR, 1947-2021") +
  scale_y_continuous(limits = c(0, 5)) +
  theme_minimal() +
  theme(legend.position = "top")


# plot(Tfr.Japan$Year,Tfr.Japan$TFR, ylim=c(0, 5), col="blue", xlab="year", ylab="TFR", type="l",
#          lty=1, main = "Period TFR, Japan. 1947-2021")
# lines(Tfr.S.korea$TFR,col="red")
# lines(Tfr.Taiwan$TFR,col="forestgreen")


#MAB #-----
MAb.Japan <- read.table("data_txt/JPNmabRR.txt",header = T,skip = 2)
MAb.S.korea <- read.table("data_txt/KORmabRR.txt",header = T,skip = 2)
MAb.Taiwan <- read.table("data_txt/TWNmabRR.txt",header = T,skip = 2)

plot(MAb.Japan$Year, MAb.Japan$MAB, ylim=c(15, 40), col="blue", xlab="year", ylab="MAB",
           type="l", lty=1, main = "Period Mean Age at Birth, 1947-2021")
lines(MAb.S.korea,col="red")
lines(MAb.Taiwan,col="forestgreen")
legend("bottomright",c("Japan","S.korea", "Taiwan"), col=c("blue","red","forestgreen"),lty=1,cex=0.8)
# ggplot() +
#   geom_line(data = MAb.Japan, aes(x = Year, y = MAB), color = "blue", linetype = "solid") +
#   geom_line(data = MAb.S.korea, aes(x = Year, y = MAB), color = "red", linetype = "solid") +
#   geom_line(data = MAb.Taiwan, aes(x = Year, y = MAB), color = "forestgreen", linetype = "solid") +
#   scale_color_manual(values = c("blue", "red", "forestgreen"), 
#                      labels = c("Japan", "S. Korea", "Taiwan")) +
#   labs(x = "Year", y = "MAB", title = "Period MAB, 1947-2021") +
#   scale_y_continuous(limits = c(0, 5)) +
#   theme_minimal()

# Sd.MAb.Japan <- read.table("data_txt/JPNsdmabRR.txt",header = T,skip = 2)
# plot(x=Sd.MAb.Japan$Year, y=Sd.MAb.Japan$MAB, col="blue", xlab="year", ylab="Sd.MAB",
#      type="l", lty=1, main = "SD in Mean Age at Birth, Japan. 1947-2021")

#Tassi fertilità #----


#install.packages("readxl")
library(readxl)
S.korea.fertility <- read_excel("S_korea_fertility_rates.xlsx")
Japan.fertility <- read_excel("Japan_fertility_rates.xlsx")
China.fertility <- read_excel("China_fertility_rates.xlsx")
N.korea.fertility <- read_excel("N_korea_fertility_rates.xlsx")
# 
# plot(S.korea.fertility$`Fertility rate`,type="l",main="Asian countries fertility rates",col="blue"
#      ,ylim = c(0,8))
# lines(Japan.fertility$`Japan fertility rates`,type="l",col="red")
# lines(China.fertility$`China fertility rates`,type="l",col="forestgreen")
# lines(N.korea.fertility$N.Korea,type="l",col="purple")
# legend("topright",legend=c("S.korea","Japan","China", "N.korea"),
#        fill =c("blue","red","forestgreen","purple"),cex=0.7)

library(ggplot2)
# Combine the data into a single dataframe
fertility_data <- data.frame(
  Year = seq(1960, 2020),
  S_Korea = S.korea.fertility$`Fertility rate`,
  Japan = Japan.fertility$`Japan fertility rates`,
  China = China.fertility$`China fertility rates`,
  N_Korea = N.korea.fertility$N.Korea
)

# Convert the data to long format for plotting
fertility_data_long <- tidyr::gather(fertility_data, Country, Fertility_Rate, -Year)

# Create the plot using ggplot2
ggplot(data = fertility_data_long, aes(x = Year, y = Fertility_Rate, color = Country)) +
  geom_line(size = 1) +
  scale_x_continuous(limits = c(1960, 2020)) +
  labs(title = "Asian countries fertility rates", x = "Year", y = "Fertility Rate") +
  theme_bw() +
  theme(legend.position = "top")


