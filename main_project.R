#install.packages("demography", dependencies=TRUE)
library(demography)

S.korea <- hmd.mx("KOR", "email", "password", "S.korea") #2003-2020
Taiwan<- hmd.mx("TWN", "memail", "password", "Taiwan") #1970 - 2020
Japan<- hmd.mx("JPN", "email", "password", "Japan") #1947 - 2020
HongKong<- hmd.mx("HKG", "email", "password", "HongKong") #1986 - 2020
#

#Death rate (log) per age #----

#S.korea 
plot(S.korea, series=names(S.korea$rate)[1], year=S.korea$year, main="S.korea-Female, 2003-2020")

plot(S.korea, series=names(S.korea$rate)[2], year=S.korea$year, main="S.korea-Male, 2003-2020")

plot(S.korea, series=names(S.korea$rate)[3], year=S.korea$year, main="S.korea-Pop, 2003-2020")


#Japan
plot(Japan, series=names(Japan$rate)[1], year=Japan$year, main="Japan-Female, 1947-2020")

plot(Japan, series=names(Japan$rate)[2], year=Japan$year, main="Japan-Male, 1947-2020")

plot(Japan, series=names(Japan$rate)[3], year=Japan$year, main="Japan-Pop, 1947-2020")

#Taiwan
plot(Taiwan, series=names(Taiwan$rate)[1], year=Taiwan$year, main="Taiwan-Female, 1970-2020")

plot(Taiwan, series=names(Taiwan$rate)[2], year=Taiwan$year, main="Taiwan-Male, 1970-2020")

plot(Taiwan, series=names(Taiwan$rate)[3], year=Taiwan$year, main="Taiwan-Pop, 1970-2020")

#HongKong
plot(HongKong, series=names(HongKong$rate)[1], year=HongKong$year, main="HongKong-Female, 1986-2020")

plot(HongKong, series=names(HongKong$rate)[2], year=HongKong$year, main="HongKong-Male, 1986-2020")

plot(HongKong, series=names(HongKong$rate)[3], year=HongKong$year, main="HongKong-Pop, 1986-2020")


# 
# ##  Death rates for 2020
# plot(S.korea, series=names(S.korea$rate)[3], year=2020, main="Death rates for 2020",col="blue")
# lines(Japan, series=names(Japan$rate)[3], year=2020,col="red")
# lines(Taiwan, series=names(Taiwan$rate)[3], year=2019,col="forestgreen")
# lines(HongKong, series=names(HongKong$rate)[3], year=2020,col="purple")
# legend("bottomright",c("S.korea","Japan", "Taiwan","HongKong"),col=c("blue","red","forestgreen","purple"),lty=1)
# 
# ##
# 
# ##Death rates for each starting years
# plot(S.korea, series=names(S.korea$rate)[3], year=S.korea$year[1], main="Death rates for each starting years",col="blue")
# lines(Japan, series=names(Japan$rate)[3], year=Japan$year[1],col="red")
# lines(Taiwan, series=names(Taiwan$rate)[3], year=Taiwan$year[1],col="forestgreen")
# lines(HongKong, series=names(HongKong$rate)[3], year=HongKong$year[1],col="purple")
# legend("bottomright",c("S.korea","Japan", "Taiwan","HongKong"),col=c("blue","red","forestgreen","purple"),lty=1)
# ##

#Death rate (log) start - end of each period#
plot(S.korea, series=names(S.korea$rate)[3], year=2020, main="Death rates for start - end",col="blue", lty=1,lwd=2)
lines(Japan, series=names(Japan$rate)[3], year=2020,col="red", lty=1,lwd=2)
lines(Taiwan, series=names(Taiwan$rate)[3], year=2019,col="forestgreen", lty=1,lwd=2)
lines(HongKong, series=names(HongKong$rate)[3], year=2020,col="purple", lty=1,lwd=2)
lines(S.korea, series=names(Japan$rate)[3], year=S.korea$year[1],col="blue", lty=2,lwd=2)
lines(Japan, series=names(Japan$rate)[3], year=Japan$year[1],col="red", lty=2,lwd=2)
lines(Taiwan, series=names(Taiwan$rate)[3], year=Taiwan$year[1],col="forestgreen", lty=2,lwd=2)
lines(HongKong, series=names(HongKong$rate)[3], year=HongKong$year[1],col="purple", lty=2,lwd=2)
legend("bottomright",c("S.korea","Japan", "Taiwan","HongKong","start","end"),
       fill=c("blue","red","forestgreen","purple",NA,NA),lty=c(NA,NA,NA,NA,1,2),cex=0.8)


#expectancy of life at birth -e0- #----
S.korea_e0 <- hmd.e0("KOR", "matteo.ferniani@studio.unibo.it", "HDMi123!")#2003
Taiwan_e0 <- hmd.e0("TWN", "matteo.ferniani@studio.unibo.it", "HDMi123!") #1970
Japan_e0 <- hmd.e0("JPN", "matteo.ferniani@studio.unibo.it", "HDMi123!")  #1947
HongKong_e0 <- hmd.e0("HKG", "matteo.ferniani@studio.unibo.it", "HDMi123!") #1986

#female
plot(Japan_e0[,1],xlim=c(1947,2020),ylim=c(50,90),col="blue",main="Life expectancy Female")
lines(Taiwan_e0[,1],col="red")
lines(S.korea_e0[,1],col="forestgreen",lwd=2)
lines(HongKong_e0[,1],col="purple",lwd=1.5)
legend("bottomright",c("Japan", "Taiwan","S.korea","HongKong"),
       col=c("blue","red","forestgreen","purple"),lty=1,cex=0.8)
#male
plot(Japan_e0[,2],xlim=c(1947,2020),ylim=c(50,85),col="blue",main="Life expectancy Male")
lines(Taiwan_e0[,2],col="red")
lines(S.korea_e0[,2],col="forestgreen",lwd=2)
lines(HongKong_e0[,2],col="purple",lwd=1.5)
legend("bottomright",c("Japan", "Taiwan","S.korea","HongKong"),
       col=c("blue","red","forestgreen","purple"),lty=1,cex=0.8)

#pop
plot(Japan_e0[,3],xlim=c(1947,2020),ylim=c(50,85),col="blue",main="Life expectancy Pop")
lines(Taiwan_e0[,3],col="red")
lines(S.korea_e0[,3],col="forestgreen",lwd=2)
lines(HongKong_e0[,3],col="purple",lwd=1.5)
legend("bottomright",c("Japan", "Taiwan","S.korea","HongKong"),
       col=c("blue","red","forestgreen","purple"),lty=1,cex=0.8)


### Male -vs- Female
plot(Japan_e0[,1],xlim=c(1947,2020),ylim=c(50,90),col="blue",main="Life expectancy Male-vs-Female",lty=1)
lines(Taiwan_e0[,1],col="red",lty=1)
lines(S.korea_e0[,1],col="forestgreen",lwd=2,lty=1)
lines(HongKong_e0[,1],col="purple",lwd=1.5,lty=1)
lines(Japan_e0[,2],col="blue",lty=2)
lines(Taiwan_e0[,2],col="red",lty=2)
lines(S.korea_e0[,2],col="forestgreen",lwd=2,lty=2)
lines(HongKong_e0[,2],col="purple",lwd=1.5,lty=2)
legend("bottomright",legend=c("Japan","Taiwan","S.korea", "HongKong","Male","Female"),fill =c("blue","red","forestgreen","purple",NA,NA),lty = c(NA, NA, NA, NA, 1, 2))


#Lifetable #----
# Lifetable: Construct life tables from mortality rates
# omputes period and cohort lifetables from  mortality rates for multiple years

#South Korea
S.korea.lifetable_female <- lifetable(S.korea, series = names(S.korea$rate)[1], years = S.korea$year, ages = 
                                 S.korea$age, max.age = min(110, max(S.korea$age)), type = c("period"))


S.korea.lifetable_male <- lifetable(S.korea, series = names(S.korea$rate)[2], years = S.korea$year, ages = 
                                        S.korea$age, max.age = min(110, max(S.korea$age)), type = c("period"))  

S.korea.lifetable_pop <- lifetable(S.korea, series = names(S.korea$rate)[3], years = S.korea$year, ages = 
                                      S.korea$age, max.age = min(110, max(S.korea$age)), type = c("period"))  
#Taiwan
Taiwan.lifetable_female <- lifetable(Taiwan, series = names(Taiwan$rate)[1], years = Taiwan$year, ages = 
                                       Taiwan$age, max.age = min(110, max(Taiwan$age)), type = c("period"))

Taiwan.lifetable_male <- lifetable(Taiwan, series = names(Taiwan$rate)[2], years = Taiwan$year, ages = 
                                       Taiwan$age, max.age = min(110, max(Taiwan$age)), type = c("period"))

Taiwan.lifetable_pop <- lifetable(Taiwan, series = names(Taiwan$rate)[3], years = Taiwan$year, ages = 
                                       Taiwan$age, max.age = min(110, max(Taiwan$age)), type = c("period"))
#Japan
Japan.lifetable_female <- lifetable(Japan, series = names(Japan$rate)[1], years = Japan$year, ages = 
                                      Japan$age, max.age = min(110, max(Japan$age)), type = c("period"))

Japan.lifetable_male <- lifetable(Japan, series = names(Japan$rate)[2], years = Japan$year, ages = 
                                    Japan$age, max.age = min(110, max(Japan$age)), type = c("period"))

Japan.lifetable_pop <- lifetable(Japan, series = names(Japan$rate)[3], years = Japan$year, ages = 
                                   Japan$age, max.age = min(110, max(Japan$age)), type = c("period"))

#HongKong
HongKong.lifetable_female <- lifetable(HongKong, series = names(HongKong$rate)[1], years = HongKong$year, ages = 
                                         HongKong$age, max.age = min(110, max(HongKong$age)), type = c("period"))

HongKong.lifetable_male <- lifetable(HongKong, series = names(HongKong$rate)[2], years = HongKong$year, ages = 
                                       HongKong$age, max.age = min(110, max(HongKong$age)), type = c("period"))

HongKong.lifetable_pop <- lifetable(HongKong, series = names(HongKong$rate)[3], years = HongKong$year, ages = 
                                      HongKong$age, max.age = min(110, max(HongKong$age)), type = c("period"))


#lifetable_Korea_fem <- print(S.korea.lifetable_female)
#male
#tot
plot(S.korea.lifetable_female)
plot(S.korea.lifetable_male)
plot(S.korea.lifetable_pop)


##
plot(Japan.lifetable_pop)
plot(Taiwan.lifetable_pop)
plot(HongKong.lifetable_pop)
##
print(ls(Japan.lifetable_pop))

# "dx"     
# "ex"     
# "lx"    
# "Lx"     
# "mx"     
# "qx"     
# "rx"     
# "Tx"    

#lx curves

S.korea.lifetable_pop_2003 <- lifetable(S.korea, series = names(S.korea$rate)[3], years = S.korea$year[1], 
                            ages = S.korea$age, max.age = min(110, max(S.korea$age)), type = c("period"))
lx_Korea_2003 <-S.korea.lifetable_pop_2003$lx

S.korea.lifetable_pop_2020 <- lifetable(S.korea, series = names(S.korea$rate)[3], years = S.korea$year[18], 
                                        ages = S.korea$age, max.age = min(110, max(S.korea$age)), type = c("period"))
lx_Korea_2020 <-S.korea.lifetable_pop_2020$lx

Japan.lifetable_pop_1947 <- lifetable(Japan, series = names(Japan$rate)[3], years = Japan$year[1], 
                                     ages = Japan$age, max.age = min(110, max(Japan$age)), type = c("period"))
Japan.lifetable_pop_2020 <- lifetable(Japan, series = names(Japan$rate)[3], years = Japan$year[75], 
                                      ages = Japan$age, max.age = min(110, max(Japan$age)), type = c("period"))
lx_Japan_1947 <-Japan.lifetable_pop_1947$lx
lx_Japan_2020 <-Japan.lifetable_pop_2020$lx

Taiwan.lifetable_pop_1970 <- lifetable(Taiwan, series = names(Taiwan$rate)[3], years = Taiwan$year[1], 
                                      ages = Taiwan$age, max.age = min(107, max(Taiwan$age)), type = c("period"))
Taiwan.lifetable_pop_2020 <- lifetable(Taiwan, series = names(Taiwan$rate)[3], years = Taiwan$year[50], 
                                      ages = Taiwan$age, max.age = min(107, max(Taiwan$age)), type = c("period"))
lx_Taiwan_1970 <-Taiwan.lifetable_pop_1970$lx
lx_Taiwan_2020 <-Taiwan.lifetable_pop_2020$lx


HongKong.lifetable_pop_1986 <- lifetable(HongKong, series = names(HongKong$rate)[3], years = HongKong$year[1], 
                                       ages = HongKong$age, max.age = min(110, max(HongKong$age)), type = c("period"))
HongKong.lifetable_pop_2020 <- lifetable(HongKong, series = names(HongKong$rate)[3], years = HongKong$year[35], 
                                       ages = HongKong$age, max.age = min(110, max(HongKong$age)), type = c("period"))
lx_HongKong_1986 <-HongKong.lifetable_pop_1986$lx
lx_HongKong_2020 <-HongKong.lifetable_pop_2020$lx


####lx plot #----

#  # S.Korea
# plot(lx_2003, col="blue", xlab="age", ylab="lx", type="l", lty=1, main = "Survivors lx. 
#      S.korea - 2003, 2020")
# lines(lx_2020, col="red", lty=2)
# legend("topright",c("2003", "2020"),col=c("blue","red"), lty=1:2)

# # Japan ##focus
# plot(lx_Japan_1947, col="blue", xlab="age", ylab="lx", type="l", lty=1, main = "Survivors lx. 
#      S.korea - 2003, 2020")
# lines(lx_Japan_2020, col="red", lty=2)
# legend("topright",c("2003", "2020"),col=c("blue","red"), lty=1:2)

# # Taiwan
# plot(lx_Taiwan_1970, col="blue", xlab="age", ylab="lx", type="l", lty=1, main = "Survivors lx. 
#      S.korea - 2003, 2020")
# lines(lx_Taiwan_2020, col="red", lty=2)
# legend("topright",c("2003", "2020"),col=c("blue","red"), lty=1:2)
# 
# # HongKong
# plot(lx_HongKong_1986, col="blue", xlab="age", ylab="lx", type="l", lty=1, main = "Survivors lx. 
#      S.korea - 2003, 2020")
# lines(lx_HongKong_2020, col="red", lty=2)
# legend("topright",c("2003", "2020"),col=c("blue","red"), lty=1:2)



## Survivor lx start-end each country
plot(lx_Japan_1947, col="blue", xlab="age", ylab="lx", type="l", lty=1, main = "Survivors lx. start-end")
lines(lx_Japan_2020, col="blue", lty=2)
lines(lx_Taiwan_1970, col="red",lty=1)
lines(lx_Taiwan_2020, col="red",lty=2)
lines(lx_Korea_2003, col="forestgreen",lty=1)
lines(lx_Korea_2020, col="forestgreen",lty=2)
lines(lx_HongKong_1986, col="purple",lty=1)
lines(lx_HongKong_2020, col="purple",lty=2)
legend("bottomleft",legend=c("Japan","Taiwan","S.korea", "HongKong","start","End"),fill =c("blue","red","forestgreen","purple",NA,NA),lty = c(NA, NA, NA, NA, 1, 2))

#Survivor sex differences #----
####Korea #----
#
S.korea.lifetable_female_2003 <- lifetable(S.korea, series = names(S.korea$rate)[1], years = S.korea$year[1], 
                                        ages = S.korea$age, max.age = min(110, max(S.korea$age)), type = c("period"))
S.korea.lifetable_male_2003 <- lifetable(S.korea, series = names(S.korea$rate)[2], years = S.korea$year[1], 
                                           ages = S.korea$age, max.age = min(110, max(S.korea$age)), type = c("period"))
S.korea.lifetable_female_2020 <- lifetable(S.korea, series = names(S.korea$rate)[1], years = S.korea$year[18], 
                                           ages = S.korea$age, max.age = min(110, max(S.korea$age)), type = c("period"))
S.korea.lifetable_male_2020 <- lifetable(S.korea, series = names(S.korea$rate)[2], years = S.korea$year[18], 
                                         ages = S.korea$age, max.age = min(110, max(S.korea$age)), type = c("period"))

lx_Korea.female_2003 <-S.korea.lifetable_female_2003$lx
lx_Korea.male_2003 <-S.korea.lifetable_male_2003$lx
lx_Korea.female_2020 <-S.korea.lifetable_female_2020$lx
lx_Korea.male_2020 <-S.korea.lifetable_male_2020$lx

plot(lx_Korea.female_2003, col="blue", xlab="age", ylab="lx", type="l", lty=1, main = "Survivors lx. S.Korea start-end, female-vs-male")
lines(lx_Korea.female_2020, col="blue", lty=2)
lines(lx_Korea.male_2003, col="red",lty=1)
lines(lx_Korea.male_2020, col="red",lty=2)
legend("bottomleft",legend=c("female-2003","female-2020","male-2003", "male-2020","start","end"),fill =c("blue","blue","red","red",NA,NA),lty = c(NA, NA, NA, NA, 1, 2))

####Japan #----
Japan.lifetable_female_1947 <- lifetable(Japan, series = names(Japan$rate)[1], years = Japan$year[1], 
                                           ages = Japan$age, max.age = min(110, max(Japan$age)), type = c("period"))
Japan.lifetable_male_1947 <- lifetable(Japan, series = names(Japan$rate)[2], years = Japan$year[1], 
                                         ages = Japan$age, max.age = min(110, max(Japan$age)), type = c("period"))
Japan.lifetable_female_2021 <- lifetable(Japan, series = names(Japan$rate)[1], years = Japan$year[75], 
                                         ages = Japan$age, max.age = min(110, max(Japan$age)), type = c("period"))
Japan.lifetable_male_2021 <- lifetable(Japan, series = names(Japan$rate)[2], years = Japan$year[75], 
                                       ages = Japan$age, max.age = min(110, max(Japan$age)), type = c("period"))
lx_Japan.female_1947 <-Japan.lifetable_female_1947$lx
lx_Japan.female_2021 <-Japan.lifetable_female_2021$lx
lx_Japan.male_1947 <-Japan.lifetable_male_1947$lx
lx_Japan.male_2021 <-Japan.lifetable_male_2021$lx


plot(lx_Japan.female_1947, col="blue", xlab="age", ylab="lx", type="l", lty=1, main = "Survivors lx. Japan start-end, female-vs-male")
lines(lx_Japan.female_2021, col="blue", lty=2)
lines(lx_Japan.male_1947, col="red",lty=1)
lines(lx_Japan.male_2021, col="red",lty=2)
legend("bottomleft",legend=c("female-1947","female-2021","male-1947", "male-2021","start","end"),fill =c("blue","blue","red","red",NA,NA),lty = c(NA, NA, NA, NA, 1, 2))

####Taiwan #----
Taiwan.lifetable_female_1970 <- lifetable(Taiwan, series = names(Taiwan$rate)[1], years = Taiwan$year[1], 
                                         ages = Taiwan$age, max.age = min(110, max(Taiwan$age)), type = c("period"))
Taiwan.lifetable_male_1970 <- lifetable(Taiwan, series = names(Taiwan$rate)[2], years = Taiwan$year[1], 
                                          ages = Taiwan$age, max.age = min(110, max(Taiwan$age)), type = c("period"))
Taiwan.lifetable_female_2019 <- lifetable(Taiwan, series = names(Taiwan$rate)[1], years = Taiwan$year[50], 
                                          ages = Taiwan$age, max.age = min(110, max(Taiwan$age)), type = c("period"))
Taiwan.lifetable_male_2019 <- lifetable(Taiwan, series = names(Taiwan$rate)[2], years = Taiwan$year[50], 
                                        ages = Taiwan$age, max.age = min(110, max(Taiwan$age)), type = c("period"))

lx_Taiwan.female_1970 <-Taiwan.lifetable_female_1970$lx
lx_Taiwan.male_1970 <-Taiwan.lifetable_male_1970$lx
lx_Taiwan.female_2019 <-Taiwan.lifetable_female_2019$lx
lx_Taiwan.male_2019 <-Taiwan.lifetable_male_2019$lx

plot(lx_Taiwan.female_1970, col="blue", xlab="age", ylab="lx", type="l", lty=1, main = "Survivors lx. Taiwan start-end, female-vs-male")
lines(lx_Taiwan.female_2019, col="blue", lty=2)
lines(lx_Taiwan.male_1970, col="red",lty=1)
lines(lx_Taiwan.male_2019, col="red",lty=2)
legend("bottomleft",legend=c("female-1970","female-2019","male-1970", "male-2019","start","end"),fill =c("blue","blue","red","red",NA,NA),lty = c(NA, NA, NA, NA, 1, 2))

####HongKong #----
HongKong.lifetable_female_1986 <- lifetable(HongKong, series = names(HongKong$rate)[1], years = HongKong$year[1], 
                                          ages = HongKong$age, max.age = min(100, max(HongKong$age)), type = c("period"))
HongKong.lifetable_female_2020 <- lifetable(HongKong, series = names(HongKong$rate)[1], years = HongKong$year[35], 
                                            ages = HongKong$age, max.age = min(105, max(HongKong$age)), type = c("period"))
HongKong.lifetable_male_1986 <- lifetable(HongKong, series = names(HongKong$rate)[2], years = HongKong$year[1], 
                                            ages = HongKong$age, max.age = min(100, max(HongKong$age)), type = c("period"))
HongKong.lifetable_male_2020 <- lifetable(HongKong, series = names(HongKong$rate)[2], years = HongKong$year[35], 
                                            ages = HongKong$age, max.age = min(105, max(HongKong$age)), type = c("period"))

lx_HongKong.female_1986 <-HongKong.lifetable_female_1986$lx
lx_HongKong.female_2020 <-HongKong.lifetable_female_2020$lx
lx_HongKong.male_1986 <-HongKong.lifetable_male_1986$lx
lx_HongKong.male_2020 <-HongKong.lifetable_male_2020$lx

plot(lx_HongKong.female_1986, col="blue", xlab="age", ylab="lx", type="l", lty=1, main = "Survivors lx. HongKong start-end, female-vs-male")
lines(lx_HongKong.female_2020, col="blue", lty=2)
lines(lx_HongKong.male_1986, col="red",lty=1)
lines(lx_HongKong.male_2020, col="red",lty=2)
legend("bottomleft",legend=c("female-1986","female-2020","male-1986", "male-2020","start","end"),fill =c("blue","blue","red","red",NA,NA),lty = c(NA, NA, NA, NA, 1, 2))


###

#### dx plot #----
#deaths dx start-end each country
plot(Japan.lifetable_pop_2020$dx, col="blue", xlab="age", ylab="dx", type="l", lty=1, main = "deaths dx start-end dx."
     ,ylim=c(0,0.06),lwd=2)
lines(Japan.lifetable_pop_1947$dx,col="blue", lty=2)
lines(Taiwan.lifetable_pop_1970$dx, col="red",lty=2)
lines(Taiwan.lifetable_pop_2020$dx, col="red",lty=1)
lines(S.korea.lifetable_pop_2020$dx, col="forestgreen",lty=1)
lines(S.korea.lifetable_pop_2003$dx, col="forestgreen",lty=2)
lines(HongKong.lifetable_pop_2020$dx, col="purple",lty=1)
lines(HongKong.lifetable_pop_1986$dx, col="purple",lty=2)
legend("topright",legend=c("Japan", "Taiwan","S.korea", "HongKong","start","End"),fill =c("blue","red","forestgreen","purple",NA,NA),
       lty = c(NA, NA, NA, NA, 1, 2),cex=0.7)

####mx #----
###mortality rate mx start-end each country
plot(Taiwan.lifetable_pop_2020$mx, col="blue", xlab="age", ylab="mx", type="l", lty=1, main = "mortality rate mx start-end" 
     ,ylim=c(0,2),lwd=2,xlim = c(0,115))
lines(Taiwan.lifetable_pop_1970$mx,col="blue", lty=2)
lines(Japan.lifetable_pop_1947$mx, col="red",lty=2)
lines(Japan.lifetable_pop_2020$mx, col="red",lty=1)
lines(S.korea.lifetable_pop_2020$mx, col="forestgreen",lty=1)
lines(S.korea.lifetable_pop_2003$mx, col="forestgreen",lty=2)
lines(HongKong.lifetable_pop_2020$mx, col="purple",lty=1)
lines(HongKong.lifetable_pop_1986$mx, col="purple",lty=2)
legend("topleft",legend=c("Taiwan","Japan","S.korea", "HongKong","start","End"),fill =c("blue","red","forestgreen","purple",NA,NA),
       lty = c(NA, NA, NA, NA, 1, 2),cex=0.8)

# #qx curve
# plot(Japan.lifetable_pop_2020$qx, col="blue", xlab="age", ylab="qx", type="l", lty=1, main = "___ qx. 
#      S.korea - 2003, 2020",ylim=c(0,1.1),lwd=2)
# lines(Japan.lifetable_pop_1947$qx,col="blue", lty=2)
# lines(Taiwan.lifetable_pop_1970$qx, col="red",lty=2)
# lines(Taiwan.lifetable_pop_2020$qx, col="red",lty=1)
# lines(S.korea.lifetable_pop_2020$qx, col="forestgreen",lty=1)
# lines(S.korea.lifetable_pop_2003$qx, col="forestgreen",lty=2)
# lines(HongKong.lifetable_pop_2020$qx, col="purple",lty=1)
# lines(HongKong.lifetable_pop_1986$qx, col="purple",lty=2)
# legend("topleft",legend=c(legendx2),fill =c("blue","red","forestgreen","purple",NA,NA),
#        lty = c(NA, NA, NA, NA, 1, 2),cex=0.6)
# 
# #rx curve
# plot(Japan.lifetable_pop_2020$rx, col="blue", xlab="age", ylab="qx", type="l", lty=1, main = "___ qx. 
#      S.korea - 2003, 2020",ylim=c(0,1),lwd=2)
# lines(Japan.lifetable_pop_1947$rx,col="blue", lty=2)
# lines(Taiwan.lifetable_pop_1970$rx, col="red",lty=2)
# lines(Taiwan.lifetable_pop_2020$rx, col="red",lty=1)
# lines(S.korea.lifetable_pop_2020$rx, col="forestgreen",lty=1)
# lines(S.korea.lifetable_pop_2003$rx, col="forestgreen",lty=2)
# lines(HongKong.lifetable_pop_2020$rx, col="purple",lty=1)
# lines(HongKong.lifetable_pop_1986$rx, col="purple",lty=2)
# legend("bottomleft",legend=c(legendx2),fill =c("blue","red","forestgreen","purple",NA,NA),
#        lty = c(NA, NA, NA, NA, 1, 2),cex=0.6)

#3D Graph #----

#Grafio 3D   lx

#Korea
S.korea.age <- S.korea.lifetable_pop$age
S.korea.year <- S.korea.lifetable_pop$year

#Japan
Japan.age <- Japan.lifetable_pop$age
Japan.year <- Japan.lifetable_pop$year

#Taiwan
Taiwan.age <- Taiwan.lifetable_pop$age
Taiwan.year <- Taiwan.lifetable_pop$year

#HongKong
HongKong.age <- HongKong.lifetable_pop$age
HongKong.year <- HongKong.lifetable_pop$year

par(mfrow=c(2,2))
persp(Japan.age,Japan.year,Japan.lifetable_pop$lx, theta = 45, zlab = "lx", main = "lx. Japan 1947-2020")
persp(S.korea.age,S.korea.year,S.korea.lifetable_pop$lx, theta = 45, zlab = "lx", main = "lx. S.korea 2003-2020")
persp(Taiwan.age,Taiwan.year,Taiwan.lifetable_pop$lx, theta = 45, zlab = "lx", main = "lx. Taiwan 1970-2020")
persp(HongKong.age,HongKong.year,HongKong.lifetable_pop$lx, theta = 45, zlab = "lx", main = "lx. HongKong 1986-2020")


persp(Japan.age,Japan.year,Japan.lifetable_pop$dx, theta = 30, zlab = "dx", main = "dx. Japan 1947-2020")
persp(S.korea.age,S.korea.year,S.korea.lifetable_pop$dx, theta = 30, zlab = "dx", main = "dx. S.korea 2003-2020")
persp(Taiwan.age,Taiwan.year,Taiwan.lifetable_pop$dx, theta = 30, zlab = "dx", main = "dx. Taiwan 1970-2020")
persp(HongKong.age,HongKong.year,HongKong.lifetable_pop$dx, theta = 30, zlab = "dx", main = "dx. HongKong 2003-2020")

## ## ### ### ### #### #### ### ### #### # #### #### ##
#Expectancy of Life e0 (at birth)  


## GIA' FATTO SOPRA (fino a riga 388)


## Korea
e0_female.Korea <- life.expectancy(S.korea, series = names(S.korea$rate)[1], 
                      years = S.korea$year, type = c("period"))
e0_male.Korea <- life.expectancy(S.korea, series = names(S.korea$rate)[2], 
                             years = S.korea$year, type = c("period"))
e0_pop.Korea <- life.expectancy(S.korea, series = names(S.korea$rate)[3], 
                             years = S.korea$year, type = c("period"))
plot(e0_female.Korea,col="blue",lty=1,lwd=2,ylim=c(70,90),main="Expectancy of Life")
lines(e0_male.Korea,col="red",lty=2,lwd=2)
lines(e0_pop.Korea,col="green",lty=3,lwd=2)
legend("bottomright",c("female", "male","pop"),col=c("blue","red","green"), lty=1:3)

##
e0_female.Japan <- life.expectancy(Japan, series = names(Japan$rate)[1], 
                                   years = Japan$year, type = c("period"))
e0_male.Japan <- life.expectancy(Japan, series = names(Japan$rate)[2], 
                                 years = Japan$year, type = c("period"))
e0_pop.Japan <- life.expectancy(Japan, series = names(Japan$rate)[3], 
                                years = Japan$year, type = c("period"))
plot(e0_female.Japan,col="blue",lty=1,lwd=2,ylim=c(47,90),main="Expectancy of Life")
lines(e0_male.Japan,col="red",lty=2,lwd=2)
lines(e0_pop.Japan,col="green",lty=3,lwd=2)
legend("bottomright",c("female", "male","pop"),col=c("blue","red","green"), lty=1:3)

e0_female.HongKong <- life.expectancy(HongKong, series = names(HongKong$rate)[1], 
                                   years = HongKong$year, type = c("period"))
e0_male.HongKong <- life.expectancy(HongKong, series = names(HongKong$rate)[2], 
                                 years = HongKong$year, type = c("period"))
e0_pop.HongKong <- life.expectancy(HongKong, series = names(HongKong$rate)[3], 
                                years = HongKong$year, type = c("period"))
plot(e0_female.HongKong,col="blue",lty=1,lwd=2,ylim=c(70,90),main="Expectancy of Life")
lines(e0_male.HongKong,col="red",lty=2,lwd=2)
lines(e0_pop.HongKong,col="green",lty=3,lwd=2)
legend("bottomright",c("female", "male","pop"),col=c("blue","red","green"), lty=1:3)

e0_pop.Taiwan <- life.expectancy(Taiwan, series = names(Taiwan$rate)[3], 
                                   years = Taiwan$year, type = c("period"))
plot(e0_pop.Japan,col="blue",lty=1,lwd=2,ylim=c(47,90),main="Expectancy of Life")
lines(e0_pop.Taiwan,col="red",lty=2,lwd=2)
lines(e0_pop.Korea,col="forestgreen",lty=3,lwd=2)
lines(e0_pop.HongKong,col="purple",lty=3,lwd=2)
legend("bottomright",legend=c("Japan","Taiwan","S.korea", "HongKong"),
       col =c("blue","red","forestgreen","purple"),lty = 1)


#life expectancy's evolution at 0, 20, 60 e 80  #----
e_0.Korea <- life.expectancy(S.korea, series = names(S.korea$rate)[3], years = 
                         S.korea$year, type = c("period"), age=0)
e_20.Korea <- life.expectancy(S.korea, series = names(S.korea$rate)[3], years = 
                         S.korea$year, type = c("period"), age=20)
e_60.Korea <- life.expectancy(S.korea, series = names(S.korea$rate)[3], years = 
                         S.korea$year, type = c("period"), age=60)
e_80.Korea <- life.expectancy(S.korea, series = names(S.korea$rate)[3], years = 
                         S.korea$year, type = c("period"), age=80)

# plot(e_0.Korea, col="blue", xlab="year", ylab="expectancy", main = 
# "Evolution of e0, e60 and e80 in Korea",ylim=c(0, 85),lty=1)
# lines(e_60.Korea,col="red",ylim=c(0,26),lty=2)
# lines(e_80.Korea,col="green",lty=3)
# lines(e_20.Korea,col="purple",ylim=c(0,26),lty=4)
# legend("center",c("0", "60","80","20"),col=c("blue","red","green","purple"), lty=1:4)


e_0.Japan <- life.expectancy(Japan, series = names(Japan$rate)[3], years = 
                               Japan$year, type = c("period"), age=0)
e_20.Japan <- life.expectancy(Japan, series = names(Japan$rate)[3], years = 
                                Japan$year, type = c("period"), age=20)
e_60.Japan <- life.expectancy(Japan, series = names(Japan$rate)[3], years = 
                                Japan$year, type = c("period"), age=60)
e_80.Japan <- life.expectancy(Japan, series = names(Japan$rate)[3], years = 
                                Japan$year, type = c("period"), age=80)

# plot(e_0.Japan, col="blue", xlab="year", ylab="expectancy", main = 
#        "Evolution of e0, e60 and e80 in Japan",ylim=c(0, 85),lty=1)
# lines(e_60.Japan,col="red",ylim=c(0,26),lty=2)
# lines(e_80.Japan,col="green",lty=3)
# lines(e_20.Japan,col="purple",ylim=c(0,26),lty=4)
# legend("center",c("0", "60","80","20"),col=c("blue","red","green","purple"), lty=1:4)

e_0.Taiwan <- life.expectancy(Taiwan, series = names(Taiwan$rate)[3], years = 
                                Taiwan$year, type = c("period"), age=0)
e_0.HongKong <- life.expectancy(HongKong, series = names(HongKong$rate)[3], years = 
                                  HongKong$year, type = c("period"), age=0)

e_20.Taiwan <- life.expectancy(Taiwan, series = names(Taiwan$rate)[3], years = 
                                Taiwan$year, type = c("period"), age=20)
e_60.Taiwan <- life.expectancy(Taiwan, series = names(Taiwan$rate)[3], years = 
                                Taiwan$year, type = c("period"), age=60)
e_80.Taiwan <- life.expectancy(Taiwan, series = names(Taiwan$rate)[3], years = 
                                Taiwan$year, type = c("period"), age=80)
e_20.HongKong <- life.expectancy(HongKong, series = names(HongKong$rate)[3], years = 
                                  HongKong$year, type = c("period"), age=20)
e_60.HongKong <- life.expectancy(HongKong, series = names(HongKong$rate)[3], years = 
                                  HongKong$year, type = c("period"), age=60)
e_80.HongKong <- life.expectancy(HongKong, series = names(HongKong$rate)[3], years = 
                                  HongKong$year, type = c("period"), age=80)
plot(e_0.Japan, col="blue", xlab="year", ylab="expectancy", main = 
       "Evolution of e_n for each country",ylim=c(0, 85),lty=1)
lines(e_20.Japan,col="blue",lty=2)
lines(e_60.Japan,col="blue",lty=3)
lines(e_80.Japan,col="blue",lty=4)
lines(e_0.Taiwan,col="red",lty=1)
lines(e_20.Taiwan,col="red",lty=2)
lines(e_60.Taiwan,col="red",lty=3)
lines(e_80.Taiwan,col="red",lty=4)
lines(e_0.Korea,col="forestgreen",lty=1,lwd=2)
lines(e_20.Korea,col="forestgreen",lty=2,lwd=2)
lines(e_60.Korea,col="forestgreen",lty=3,lwd=2.5)
lines(e_80.Korea,col="forestgreen",lty=4,lwd=2)
lines(e_0.HongKong,col="purple",lty=1,lwd=1)
lines(e_20.HongKong,col="purple",lty=2,lwd=2)
lines(e_60.HongKong,col="purple",lty=3,lwd=2)
lines(e_80.HongKong,col="purple",lty=4,lwd=2)
legend("topleft",legend=c("Japan","Taiwan","S.korea", "HongKong","e0","e20","e60","e80"),
       fill =c("blue","red","forestgreen","purple",NA,NA,NA,NA),lty = c(NA,NA,NA,NA,1,2,3,4),cex=0.5)


#Lee-Carter model #----
S.korea.LC.pop <- lca(S.korea, series=names(S.korea$rate)[3], 
                 years=S.korea$year)
Japan.LC.pop <- lca(Japan, series=names(Japan$rate)[3], 
                      years=1986:2021)
Taiwan.LC.pop <- lca(Taiwan, series=names(Taiwan$rate)[3], 
                      years=1986:2021)
HongKong.LC.pop <- lca(HongKong, series=names(HongKong$rate)[3], 
                      years=1986:2021,interpolate = T)

#plot(HongKong.LC.pop)
### ax
plot(Japan.LC.pop$ax,type = "l",col="blue",main="ax",ylim = c(-10,0),xlab = "age",ylab = "ax")
lines(S.korea.LC.pop$ax,col="red")
lines(Taiwan.LC.pop$ax,col="forestgreen")
lines(HongKong.LC.pop$ax,col="purple")
legend("bottomright",c("Japan","S.korea", "Taiwan","HongKong"),
       col=c("blue","red","forestgreen","purple"),lty=1,cex=0.8)
#bx
plot(Japan.LC.pop$bx,type = "l",col="blue",main="bx",ylim = c(-0,0.028),xlab = "age",ylab = "bx")
lines(S.korea.LC.pop$bx,col="red")
lines(Taiwan.LC.pop$bx,col="forestgreen")
lines(HongKong.LC.pop$bx,col="purple")
legend("topright",c("Japan","S.korea", "Taiwan","HongKong"),
       col=c("blue","red","forestgreen","purple"),lty=1,cex=0.8)
#kt
plot(Japan.LC.pop$kt,type = "l",col="blue",main="kt",xlab = "age",ylab = "kt",ylim=c(-40,40))
lines(S.korea.LC.pop$kt,col="red")
lines(Taiwan.LC.pop$kt,col="forestgreen")
lines(HongKong.LC.pop$kt,col="purple")
legend("topright",c("Japan","S.korea", "Taiwan","HongKong"),
       col=c("blue","red","forestgreen","purple"),lty=1,cex=0.8)

#residuals
plot(S.korea.LC.pop$residuals)
#fitted
plot(S.korea.LC.pop$fitted)


#RESIDUALS #----
res.S.korea.LC.pop = residuals(S.korea.LC.pop, "residuals")
res.Japan.LC.pop = residuals(Japan.LC.pop, "residuals")
res.Taiwan.LC.pop = residuals(Taiwan.LC.pop, "residuals")
res.HongKong.LC.pop = residuals(HongKong.LC.pop, "residuals")


par(mfrow=c(2,2))
plot(rep(res.S.korea.LC.pop$y,length(res.S.korea.LC.pop$x)),(res.S.korea.LC.pop$z), xlab = "age", ylab 
     = "residuals", main = "Residuals by ages S.korea")

plot(rep(res.Japan.LC.pop$y,length(res.Japan.LC.pop$x)),(res.Japan.LC.pop$z), xlab = "age", ylab 
     = "residuals", main = "Residuals by ages Japan")

plot(rep(res.Taiwan.LC.pop$y,length(res.Taiwan.LC.pop$x)),(res.Taiwan.LC.pop$z), xlab = "age", ylab 
     = "residuals", main = "Residuals by ages Taiwan")

plot(rep(res.HongKong.LC.pop$y,length(res.HongKong.LC.pop$x)),(res.HongKong.LC.pop$z), xlab = "age", ylab 
     = "residuals", main = "Residuals by ages HongKong")





par(mfrow=c(2,2))
plot(rep(res.S.korea.LC.pop$x,length(res.S.korea.LC.pop$y)),(res.S.korea.LC.pop$z), xlab = "years", ylab 
     = "residuals", main = "Residuals by years S.korea")

plot(rep(res.Japan.LC.pop$x,length(res.Japan.LC.pop$y)),(res.Japan.LC.pop$z), xlab = "years", ylab 
     = "residuals", main = "Residuals by years Japan")

plot(rep(res.Taiwan.LC.pop$x,length(res.Taiwan.LC.pop$y)),(res.Taiwan.LC.pop$z), xlab = "years", ylab 
     = "residuals", main = "Residuals by years Taiwan")

plot(rep(res.HongKong.LC.pop$x,length(res.HongKong.LC.pop$y)),(res.HongKong.LC.pop$z), xlab = "years", ylab 
     = "residuals", main = "Residuals by years HongKong")

par(mfrow=c(2,2))
plot(residuals(S.korea.LC.pop),type="image", main = "Residuals heatmap S.korea")
plot(residuals(Japan.LC.pop),type="image", main = "Residuals heatmap Japan")
plot(residuals(Taiwan.LC.pop),type="image", main = "Residuals heatmap Taiwan")
plot(residuals(HongKong.LC.pop),type="image", main = "Residuals heatmap HongKong")

#Forecast of mortality #----
par(mfrow=c(2,2), title("Forecasts from RW w drift"))
plot(forecast(S.korea.LC.pop)$kt,main = "S.korea",ylab = "Kt",xlab = "years")
plot(forecast(Japan.LC.pop)$kt,main = "Japan",ylab = "Kt",xlab = "years")
plot(forecast(Taiwan.LC.pop)$kt,main = "Taiwan",ylab = "Kt",xlab = "years")
plot(forecast(HongKong.LC.pop)$kt,main = "HongKong",ylab = "Kt",xlab = "years")


#COMPARING
plot(S.korea) #actual
plot(fitted(S.korea.LC.pop)) #fitted
plot(forecast(S.korea.LC.pop)) #forecast


#FORECASTING 
S.korea.forecast <- lifetable(forecast(S.korea.LC.pop))
plot(S.korea.forecast)

e_0.forecast <- life.expectancy(forecast(S.korea.LC.pop), 
                                 type = c("period"))
e_60.forecast <- life.expectancy(forecast(S.korea.LC.pop), 
                                type = c("period"),age = 60)
e_80.forecast <- life.expectancy(forecast(S.korea.LC.pop), 
                                type = c("period"),age = 80)
e_20.forecast <- life.expectancy(forecast(S.korea.LC.pop), 
                                 type = c("period"),age = 20)

e_0.joining.S.korea <- life.expectancy(S.korea, series = 
                         names(S.korea$rate)[3], years = S.korea$year, 
                       type = c("period"))
e_0.joint <- c(e_0.joining.S.korea, e_0.forecast)
years <- seq(2003, 2070, 1)

plot(years, e_0.joint, type="l", main = 
       "Past and Forecast Life Expectancy. S.korea 
2003-2070")
###
# Life Expectancy with Prediction Interval
Japan.LC.e0 <- lca(Japan,adjust="e0",years=1970:2020)
Japan.LC.e0.forecast <- forecast(Japan.LC.e0)
Japan.LC.e0.forecast.simulation <- e0(Japan.LC.e0.forecast,PI=TRUE,nsim=200)
plot(Japan.LC.e0.forecast.simulation)


#FERTILITY

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


##   Slide 87
