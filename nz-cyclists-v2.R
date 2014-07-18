/* nz-cyclists-v2.R
   Author: Tim Churches
   Date commenced: 16th July 2014
   Purpose: show data used by Gilham in proper context and with unbiased linear interpolation
   Sources: as shown in comments
*/

year <- 1988:2012
fatalities <- c(NA,NA,27,22,17,17,15,15,13,12,16,8,19,10,14,6,7,12,9,12,10,8,10,9,NA)
police.reported.injuries <- c(NA,NA,1054,1000,941,910,882,813,754,724,626,619,559,696,771,722,716,751,833,880,895,825,844,783,NA)
non.motor.vehicle.hosp <- c(1000,898,921,819,802,964,840,867,868,904,960,851,905,862,878,962,943,1026,1024,1146,1104,1189,1180,1078,1104)
motor.vehicle.hosp <- c(358,311,327,298,253,291,272,217,201,180,156,162,162,169,136,139,164,162,197,168,191,164,169,179,163)
nz.pop.5yrs.plus <- c(3068620,3091186,3128430,3208500,3239820,3278160,3324730,3379040,3439070,3490580,3526110,3551560,3576800,3605810,3669980,3745630,3804200,3851490,3900690,3938390,3971080,4012270,4058360,4093180,4123720)

# Tables 5,6 and 7 from http://www.transport.govt.nz/assets/Import/Documents/Cycling-2013.pdf
library(zoo)
survey.midpoints <- 1988:2012
prop.cycling.surveys <- c(NA,8,NA,NA,NA,NA,NA,NA,NA,4,NA,NA,NA,NA,NA,3,3,NA,4,3,4,4,3,NA,NA)
av.mins.cycling.surveys <- c(NA,15,NA,NA,NA,NA,NA,NA,NA,9,NA,NA,NA,NA,NA,7,7,NA,7,7,8,8,8,NA,NA)
av.kms.cycling.surveys <- c(NA,2.2,NA,NA,NA,NA,NA,NA,NA,2.0,NA,NA,NA,NA,NA,1.3,1.3,NA,1.5,1.4,1.7,1.5,1.6,NA,NA)

prop.cycling.surveys.ts <- zoo(x=prop.cycling.surveys,order.by=survey.midpoints)
av.mins.cycling.surveys.ts <- zoo(x=av.mins.cycling.surveys,order.by=survey.midpoints)
av.kms.cycling.surveys.ts <- zoo(x=av.kms.cycling.surveys,order.by=survey.midpoints)

prop.cycling <- na.fill(prop.cycling.surveys.ts,"extend")
av.mins.cycling <- na.fill(av.mins.cycling.surveys.ts,"extend")
av.kms.cycling <- na.fill(av.kms.cycling.surveys.ts,"extend")
num.cycling <- nz.pop.5yrs.plus * prop.cycling
mins.cycling <- num.cycling * av.mins.cycling
kms.cycling <- num.cycling * av.kms.cycling


n90 <- function (x) { x / rep(x[3],length(x))}

nz.cyclists <- data.frame(ratio=n90(prop.cycling),year=year,measure="Proportion cycling")
nz.cyclists <- rbind(nz.cyclists, data.frame(ratio=n90(av.mins.cycling),year=year,measure="Av. minutes cycling"))
nz.cyclists <- rbind(nz.cyclists, data.frame(ratio=n90(av.kms.cycling),year=year,measure="Av. kms cycled"))
nz.cyclists <- rbind(nz.cyclists, data.frame(ratio=n90(num.cycling),year=year,measure="Number cycling"))
nz.cyclists <- rbind(nz.cyclists, data.frame(ratio=n90(mins.cycling),year=year,measure="Total minutes cycling"))
nz.cyclists <- rbind(nz.cyclists, data.frame(ratio=n90(kms.cycling),year=year,measure="Total kms cycled"))
                                          
fatality.rate.per.cyclist <- fatalities/num.cycling
fatality.rate.per.min <- fatalities/mins.cycling
fatality.rate.per.km <- fatalities/kms.cycling

nz.cyclists <- rbind(nz.cyclists, data.frame(ratio=n90(fatality.rate.per.cyclist),year=year,measure="Fatality rate"))
nz.cyclists <- rbind(nz.cyclists, data.frame(ratio=n90(fatality.rate.per.min),year=year,measure="Fatality rate per minute cycling"))
nz.cyclists <- rbind(nz.cyclists, data.frame(ratio=n90(fatality.rate.per.km),year=year,measure="Fatality rate per km cycled"))
                                                               
police.reported.injuries.rate.per.cyclist <- police.reported.injuries/num.cycling
police.reported.injuries.rate.per.min <- police.reported.injuries/mins.cycling
police.reported.injuries.rate.per.km <- police.reported.injuries/kms.cycling

nz.cyclists <- rbind(nz.cyclists, data.frame(ratio=n90(police.reported.injuries.rate.per.cyclist),year=year,measure="Police-reported injury rate"))
nz.cyclists <- rbind(nz.cyclists, data.frame(ratio=n90(police.reported.injuries.rate.per.min),year=year,measure="Police-reported injury rate per minute cycling"))
nz.cyclists <- rbind(nz.cyclists, data.frame(ratio=n90(police.reported.injuries.rate.per.km),year=year,measure="Police-reported injury rate per km cycled"))


non.motor.vehicle.hosp.rate.per.cyclist <- non.motor.vehicle.hosp/num.cycling
non.motor.vehicle.hosp.rate.per.min <- non.motor.vehicle.hosp/mins.cycling
non.motor.vehicle.hosp.per.km <- non.motor.vehicle.hosp/kms.cycling

nz.cyclists <- rbind(nz.cyclists, data.frame(ratio=n90(non.motor.vehicle.hosp.rate.per.cyclist),year=year,measure="Non-motor vehicle accident cyclist hospitalisation rate"))
nz.cyclists <- rbind(nz.cyclists, data.frame(ratio=n90(non.motor.vehicle.hosp.rate.per.min),year=year,measure="Non-motor vehicle accident cyclist hospitalisation per minute cycling"))
nz.cyclists <- rbind(nz.cyclists, data.frame(ratio=n90(non.motor.vehicle.hosp.per.km),year=year,measure="Non-motor vehicle accident cyclist hospitalisations per km cycled"))

motor.vehicle.hosp.rate.per.cyclist <- motor.vehicle.hosp/num.cycling
motor.vehicle.hosp.rate.per.min <- motor.vehicle.hosp/mins.cycling
motor.vehicle.hosp.per.km <- motor.vehicle.hosp/kms.cycling

nz.cyclists <- rbind(nz.cyclists, data.frame(ratio=n90(motor.vehicle.hosp.rate.per.cyclist),year=year,measure="Motor vehicle accident cyclist hospitalisation rate"))
nz.cyclists <- rbind(nz.cyclists, data.frame(ratio=n90(motor.vehicle.hosp.rate.per.min),year=year,measure="Motor vehicle accident cyclist hospitalisation per minute cycling"))
nz.cyclists <- rbind(nz.cyclists, data.frame(ratio=n90(motor.vehicle.hosp.per.km),year=year,measure="Motor vehicle accident cyclist hospitalisations per km cycled"))

all.hosp=non.motor.vehicle.hosp+motor.vehicle.hosp
all.hosp.rate.per.cyclist <- all.hosp/num.cycling
all.hosp.rate.per.min <- all.hosp/mins.cycling
all.hosp.per.km <- all.hosp/kms.cycling

nz.cyclists <- rbind(nz.cyclists, data.frame(ratio=n90(all.hosp.rate.per.cyclist),year=year,measure="All cyclist hospitalisation rate"))
nz.cyclists <- rbind(nz.cyclists, data.frame(ratio=n90(all.hosp.rate.per.min),year=year,measure="All cyclist hospitalisation per minute cycling"))
nz.cyclists <- rbind(nz.cyclists, data.frame(ratio=n90(all.hosp.per.km),year=year,measure="All cyclist hospitalisations per km cycled"))

library(ggplot2)

nzc <- nz.cyclists[nz.cyclists$measure %in% c("Proportion cycling","Fatality rate per cyclist","All cyclist hospitalisation rate","Motor vehicle accident cyclist hospitalisation rate","Non-motor vehicle accident cyclist hospitalisation rate","Police-reported injury rate"),]
nzc$ratio.surveys <- nzc[,"ratio"] 
nzc[!nzc$year %in% c(1989,1997,2003,2004,2006,2007,2008,2009,2010) | nzc$measure != "Proportion cycling","ratio.surveys"] <- NA

p <- ggplot(data=nzc) + xlim(1988,2020)  
p <- p + geom_point(aes(x=year,y=ratio.surveys,colour=measure, shape=measure, size=1.1)) 
p <- p + geom_line(aes(x=year,y=ratio, colour=measure, alpha=0.4, size=1.1)) 
p <- p + geom_vline(xintercept=1994,color="red") + theme(legend.position="none")
p <- p + ggtitle("New Zealand cycling participation, accident and fatality rates 1988-2012")
p <- p + labs(y="Proportion or rate, relative to 1990 value", x="Year")

p <- p + scale_x_continuous(breaks=1988:2025)

nz.measures <- nzc[nzc$year==2011,c("ratio","measure")]
nz.measures$year <- 2010

p <- p + geom_text(data=nz.measures, x=nz.measures$year, y=nz.measures$ratio-0.05, label=nz.measures$measure, hjust=0.6, vjust=0, size=4)

p <- p + annotate("text", x = 1994.2, y = 2.6, label = "Helmet law commencement", hjust=0, colour="red")
p





