library(demography)

S.korea <- hmd.mx("KOR", "email", "password", "S.korea") #2003-2020
Taiwan<- hmd.mx("TWN", "memail", "password", "Taiwan") #1970 - 2020
Japan<- hmd.mx("JPN", "email", "password", "Japan") #1947 - 2020
HongKong<- hmd.mx("HKG", "email", "password", "HongKong") #1986 - 2020

###models
#Lee-Carter model #----
S.korea.LC.pop <- lca(S.korea, series=names(S.korea$rate)[3], 
                      years=S.korea$year)
Japan.LC.pop <- lca(Japan, series=names(Japan$rate)[3], 
                    years=1986:2021)
Taiwan.LC.pop <- lca(Taiwan, series=names(Taiwan$rate)[3], 
                     years=1986:2021)
HongKong.LC.pop <- lca(HongKong, series=names(HongKong$rate)[3], 
                       years=1986:2021,interpolate = T)


#FORECASTING #----
####S.korea #----
S.korea.forecast <- lifetable(forecast(S.korea.LC.pop))

e_0.forecast.S.korea <- life.expectancy(forecast(S.korea.LC.pop), 
                                type = c("period"))
e_60.forecast.S.korea <- life.expectancy(forecast(S.korea.LC.pop), 
                                 type = c("period"),age = 60)
e_80.forecast.S.korea <- life.expectancy(forecast(S.korea.LC.pop), 
                                 type = c("period"),age = 80)
e_20.forecast.S.korea <- life.expectancy(forecast(S.korea.LC.pop), 
                                 type = c("period"),age = 20)
#
e_0.joining.S.korea <- life.expectancy(S.korea, series = 
                                         names(S.korea$rate)[3], years = S.korea$year, 
                                       type = c("period"))
e_0.joint.S.korea <- c(e_0.joining.S.korea, e_0.forecast.S.korea)

#
e_20.joining.S.korea <- life.expectancy(S.korea, series = 
                                         names(S.korea$rate)[3], years = S.korea$year, 
                                       type = c("period"),age = 20)
e_20.joint.S.korea <- c(e_20.joining.S.korea, e_20.forecast.S.korea)

#
e_60.joining.S.korea <- life.expectancy(S.korea, series = 
                                          names(S.korea$rate)[3], years = S.korea$year, 
                                        type = c("period"),age = 60)
e_60.joint.S.korea <- c(e_60.joining.S.korea, e_60.forecast.S.korea)

#
e_80.joining.S.korea <- life.expectancy(S.korea, series = 
                                          names(S.korea$rate)[3], years = S.korea$year, 
                                        type = c("period"),age = 80)
e_80.joint.S.korea <- c(e_80.joining.S.korea, e_80.forecast.S.korea)
##
years.S.korea <- seq(2003, 2070, 1)

par(mfrow=c(2,2))
plot(years.S.korea, e_0.joint.S.korea, type="l", main = 
       "Past-Forecast Life Expectancy
2003-2070",ylab="e0")
plot(years.S.korea, e_20.joint.S.korea, type="l", main = 
       "Past-Forecast Life Expectancy
2003-2070",ylab="e20")
plot(years.S.korea, e_60.joint.S.korea, type="l", main = 
       "Past-Forecast Life Expectancy 
2003-2070",ylab="e60")
plot(years.S.korea, e_80.joint.S.korea, type="l", main = 
       "Past-Forecast Life Expectancy 
2003-2070",ylab="e80")

####Japan #----
Japan.forecast <- lifetable(forecast(Japan.LC.pop))

e_0.forecast.Japan <- life.expectancy(forecast(Japan.LC.pop), 
                                        type = c("period"))
e_60.forecast.Japan <- life.expectancy(forecast(Japan.LC.pop), 
                                         type = c("period"),age = 60)
e_80.forecast.Japan <- life.expectancy(forecast(Japan.LC.pop), 
                                         type = c("period"),age = 80)
e_20.forecast.Japan <- life.expectancy(forecast(Japan.LC.pop), 
                                         type = c("period"),age = 20)
#
e_0.joining.Japan <- life.expectancy(Japan, series = 
                                         names(Japan$rate)[3], years = Japan$year, 
                                       type = c("period"))
e_0.joint.Japan <- c(e_0.joining.Japan, e_0.forecast.Japan)

#
e_20.joining.Japan <- life.expectancy(Japan, series = 
                                       names(Japan$rate)[3], years = Japan$year, 
                                     type = c("period"),age = 20)
e_20.joint.Japan <- c(e_20.joining.Japan, e_20.forecast.Japan)
years.Japan <- seq(1947, 2071, 1)

#
e_60.joining.Japan <- life.expectancy(Japan, series = 
                                        names(Japan$rate)[3], years = Japan$year, 
                                      type = c("period"),age = 60)
e_60.joint.Japan <- c(e_60.joining.Japan, e_60.forecast.Japan)

#
e_80.joining.Japan <- life.expectancy(Japan, series = 
                                        names(Japan$rate)[3], years = Japan$year, 
                                      type = c("period"),age = 80)
e_80.joint.Japan <- c(e_80.joining.Japan, e_80.forecast.Japan)
years.Japan <- seq(1947, 2071, 1)

##
par(mfrow=c(2,2))
plot(years.Japan, e_0.joint.Japan, type="l", main = 
       "Past-Forecast Life Expectancy 
1970-2070",ylab="e0")
plot(years.Japan, e_20.joint.Japan, type="l", main = 
       "Past-Forecast Life Expectancy 
1970-2070",ylab="e20")
plot(years.Japan, e_60.joint.Japan, type="l", main = 
       "Past-Forecast Life Expectancy 
1970-2070",ylab="e60")
plot(years.Japan, e_80.joint.Japan, type="l", main = 
       "Past-Forecast Life Expectancy 
1970-2070",ylab="e80")

####Taiwan #----
Taiwan.forecast <- lifetable(forecast(Taiwan.LC.pop))

e_0.forecast.Taiwan <- life.expectancy(forecast(Taiwan.LC.pop), 
                                      type = c("period"))
e_60.forecast.Taiwan <- life.expectancy(forecast(Taiwan.LC.pop), 
                                       type = c("period"),age = 60)
e_80.forecast.Taiwan <- life.expectancy(forecast(Taiwan.LC.pop), 
                                       type = c("period"),age = 80)
e_20.forecast.Taiwan <- life.expectancy(forecast(Taiwan.LC.pop), 
                                       type = c("period"),age = 20)
#
e_0.joining.Taiwan <- life.expectancy(Taiwan, series = 
                                       names(Taiwan$rate)[3], years = Taiwan$year, 
                                     type = c("period"))
e_0.joint.Taiwan <- c(e_0.joining.Taiwan, e_0.forecast.Taiwan)

#
e_20.joining.Taiwan <- life.expectancy(Taiwan, series = 
                                        names(Taiwan$rate)[3], years = Taiwan$year, 
                                      type = c("period"),age=20)
e_20.joint.Taiwan <- c(e_20.joining.Taiwan, e_20.forecast.Taiwan)

#
e_60.joining.Taiwan <- life.expectancy(Taiwan, series = 
                                         names(Taiwan$rate)[3], years = Taiwan$year, 
                                       type = c("period"),age=60)
e_60.joint.Taiwan <- c(e_60.joining.Taiwan, e_60.forecast.Taiwan)
#
e_80.joining.Taiwan <- life.expectancy(Taiwan, series = 
                                         names(Taiwan$rate)[3], years = Taiwan$year, 
                                       type = c("period"),age=80)
e_80.joint.Taiwan <- c(e_80.joining.Taiwan, e_80.forecast.Taiwan)

years.Taiwan <- seq(1970, 2069, 1)


length(e_0.joint.Taiwan)
length(years.Taiwan)

par(mfrow=c(2,2))
plot(years.Taiwan, e_0.joint.Taiwan, type="l", main = 
       "Past - Forecast Life Expectancy 
1970-2070",ylab="e0")
plot(years.Taiwan, e_20.joint.Taiwan, type="l", main = 
       "Past - Forecast Life Expectancy 
1970-2070",ylab="e20")
plot(years.Taiwan, e_60.joint.Taiwan, type="l", main = 
       "Past - Forecast Life Expectancy 
1970-2070",ylab="e60")
plot(years.Taiwan, e_80.joint.Taiwan, type="l", main = 
       "Past - Forecast Life Expectancy 
1970-2070",ylab="e80")

####HongKong #----
HongKong.forecast <- lifetable(forecast(HongKong.LC.pop))

e_0.forecast.HongKong <- life.expectancy(forecast(HongKong.LC.pop), 
                                       type = c("period"))
e_60.forecast.HongKong <- life.expectancy(forecast(HongKong.LC.pop), 
                                        type = c("period"),age = 60)
e_80.forecast.HongKong <- life.expectancy(forecast(HongKong.LC.pop), 
                                        type = c("period"),age = 80)
e_20.forecast.HongKong <- life.expectancy(forecast(HongKong.LC.pop), 
                                        type = c("period"),age = 20)
#
e_0.joining.HongKong <- life.expectancy(HongKong, series = 
                                        names(HongKong$rate)[3], years = HongKong$year, 
                                      type = c("period"))
e_0.joint.HongKong <- c(e_0.joining.HongKong, e_0.forecast.HongKong)

#
e_20.joining.HongKong <- life.expectancy(HongKong, series = 
                                          names(HongKong$rate)[3], years = HongKong$year, 
                                        type = c("period"),age = 20)
e_20.joint.HongKong <- c(e_20.joining.HongKong, e_20.forecast.HongKong)
#
e_60.joining.HongKong <- life.expectancy(HongKong, series = 
                                           names(HongKong$rate)[3], years = HongKong$year, 
                                         type = c("period"),age = 60)
e_60.joint.HongKong <- c(e_60.joining.HongKong, e_60.forecast.HongKong)
#

e_80.joining.HongKong <- life.expectancy(HongKong, series = 
                                           names(HongKong$rate)[3], years = HongKong$year, 
                                         type = c("period"),age = 80)
e_80.joint.HongKong <- c(e_80.joining.HongKong, e_80.forecast.HongKong)
#
years.HongKong <- seq(1986, 2070, 1)

length(e_0.joint.HongKong)
length(years.HongKong)

par(mfrow=c(2,2))
plot(years.HongKong, e_0.joint.HongKong, type="l", main = 
       "Past - Forecast Life Expectancy 
1970-2070",ylab="e0")
plot(years.HongKong, e_20.joint.HongKong, type="l", main = 
       "Past - Forecast Life Expectancy 
1970-2070",ylab="e20")
plot(years.HongKong, e_60.joint.HongKong, type="l", main = 
       "Past - Forecast Life Expectancy 
1970-2070",ylab="e60")
plot(years.HongKong, e_80.joint.HongKong, type="l", main = 
       "Past - Forecast Life Expectancy 
1970-2070",ylab="e80")



#Problema #----
## plots   ### -mpdify years x-axis
plot(e_0.joint.Japan, type="l", main = 
       "Past and Forecast Life Expectancy. 
1947-2070",ylim=c(50,95),col="blue")
lines(e_0.joint.S.korea,col="red")
lines(e_0.joint.Taiwan,col="forestgreen",lwd=2)
lines(e_0.joint.HongKong,col="purple",lwd=1)
legend("bottomright",c("Japan","S.korea", "Taiwan","HongKong"), col=c("blue","red","forestgreen","purple"),lty=1,cex=0.8)

## PROBLEMA!!!  115- 190


# create a sequence of years for the x-axis
years.x <-1947:2071

# plot the life expectancy values for each country
plot(years.x, e_0.joint.Japan, type="l", main = "Past and Forecast Life Expectancy. 1947-2070",
     ylim=c(50,95),col="blue", xlab="Year", ylab="Life Expectancy")
lines(years.x[1:length(e_0.joint.S.korea)], e_0.joint.S.korea, col="red")
lines(years.x[1:length(e_0.joint.Taiwan)], e_0.joint.Taiwan, col="forestgreen", lwd=2)
lines(years.x[1:length(e_0.joint.HongKong)], e_0.joint.HongKong, col="purple", lwd=1)

# add a legend
legend("bottomright", c("Japan","S.korea", "Taiwan","HongKong"), col=c("blue","red","forestgreen","purple"), lty=1, cex=0.8)

#STESSO PROBLEMA!!!


####        ### ###
length(years.skorea)
length(e_0.joint.S.korea)
# create sequences of years for the x-axis for each country
years.japan <- 1947:2071
years.skorea <- 2003:2070
years.taiwan <- 1970:2071
years.hk <- 1986:2071

# plot the life expectancy values for each country using the appropriate years
plot(years.japan, e_0.joint.Japan, type="l", main = "Past and Forecast Life Expectancy. 1947-2070",
     ylim=c(50,95),col="blue", xlab="Year", ylab="Life Expectancy")
lines(years.skorea, e_0.joint.S.korea[1:length(years.skorea)-length(years.japan)+1], col="red")
lines(years.taiwan, e_0.joint.Taiwan[1:length(years.taiwan)-length(years.japan)+1], col="forestgreen", lwd=2)
lines(years.hk, e_0.joint.HongKong[1:length(years.hk)-length(years.japan)+1], col="purple", lwd=1)

# add a legend
legend("bottomright", c("Japan","S.korea", "Taiwan","HongKong"), col=c("blue","red","forestgreen","purple"), lty=1, cex=0.8)


# NON VA!!!

### ## ## ## ##

library(ggplot2)

# combine the data into a data frame
data <- data.frame(
  years = c(1947:2071, 2003:2071, 1970:2069, 1970:2069),
  country = rep(c("Japan", "S.korea", "Taiwan", "HongKong"), 
                c(length(e_0.joint.Japan), length(e_0.joint.S.korea), 
                  length(e_0.joint.Taiwan), length(e_0.joint.HongKong))),
  e_0 = c(e_0.joint.Japan, rep(NA, 2004-1947), e_0.joint.S.korea, 
          rep(NA, 1970-2003), e_0.joint.Taiwan, e_0.joint.HongKong)
)

# create the plot
ggplot(data, aes(x=years, y=e_0, color=country)) +
  geom_line() +
  scale_color_manual(values=c("blue", "red", "forestgreen", "purple")) +
  xlim(c(1947, 2071)) +
  ylim(c(50, 95)) +
  labs(title = "Past and Forecast Life Expectancy. 1947-2070",
       x = "Year",
       y = "Life Expectancy") +
  theme_bw() +
  theme(legend.position = "bottomright")

#NON VA!!!!

### ## ## ### ## ## ##

#Life Expectancy with Prediction Interval  #----

#Japan
Japan.LC.e0 <- lca(Japan,adjust="e0",years=1970:2021)
Japan.LC.e0.forecast <- forecast(Japan.LC.e0)
Japan.LC.e0.forecast.simulation <- e0(Japan.LC.e0.forecast,PI=TRUE,nsim=200)
#plot(Japan.LC.e0.forecast.simulation)


#S.korea
S.korea.LC.e0 <- lca(S.korea,adjust="e0",years=S.korea$year)
S.korea.LC.e0.forecast <- forecast(S.korea.LC.e0)
S.korea.LC.e0.forecast.simulation <- e0(S.korea.LC.e0.forecast,PI=TRUE,nsim=200)
#plot(S.korea.LC.e0.forecast.simulation)

#Taiwan
Taiwan.LC.e0 <- lca(Taiwan,adjust="e0",years=Taiwan$year)
Taiwan.LC.e0.forecast <- forecast(Taiwan.LC.e0)
Taiwan.LC.e0.forecast.simulation <- e0(Taiwan.LC.e0.forecast,PI=TRUE,nsim=200)
# plot(Taiwan.LC.e0.forecast.simulation)

#HongKong
HongKong.LC.e0 <- lca(HongKong,adjust="e0",years=HongKong$year, interpolate = T)
HongKong.LC.e0.forecast <- forecast(HongKong.LC.e0)
HongKong.LC.e0.forecast.simulation <- e0(HongKong.LC.e0.forecast,PI=TRUE,nsim=200)
# plot(HongKong.LC.e0.forecast.simulation)


#Life Expectancy plot

par(mfrow=c(2,2))
plot(Japan.LC.e0.forecast.simulation,main = "Japan-Forecasts LifExpectancy",ylab = "age",xlab = "years")
plot(S.korea.LC.e0.forecast.simulation,main = "S.korea-Forecasts LifExpectancy",ylab = "age",xlab = "years")
plot(Taiwan.LC.e0.forecast.simulation,main = "Taiwan-Forecasts LifExpectancy",ylab = "age",xlab = "years")
plot(HongKong.LC.e0.forecast.simulation,main = "HongKong-Forecasts LifExpectancy",ylab = "age",xlab = "years")
