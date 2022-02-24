library(trend)
library(DHARMa)

setwd("C:/Users/Eq_PEP_IEP_SPN_Tco/Dropbox/Otros varios/Artículo abejas")#Pc1
setwd("C:/Users/USUARIO/Dropbox/Otros varios/Artículo abejas")#Pc2
datab=read.table("datos_abejas_usa.txt",h=T)



#get te total of US honey PROD AND COL
a=aggregate(datab$prod,by=list(datab$y),sum)
b=aggregate(datab$col,by=list(datab$y),sum)
prod_thousands=a$x/1000
col_thousands=b$x/1000
par(mfrow=c(1,2))
plot(prod_thousands~a$Group.1,type="p",xaxt="none",pch=19,ylab="",xlab="",yaxt="none")
lines(lowess(prod_thousands~a$Group.1,f=0.5))
mtext(side=2,"Total honey Production in the US (Thousand pounds)",line=3)
axis(1,seq(1985,2019,1),cex.axis=0.8)
axis(2,seq(-10,350,10),las=2,cex.axis=0.8)
plot(col_thousands~a$Group.1,type="p",xaxt="none",pch=19,ylab="",xlab="",yaxt="none")
lines(lowess(col_thousands~a$Group.1,f=0.5))
axis(1,seq(1985,2019,1),cex.axis=0.8)
axis(2,seq(-3,10,0.1),las=2,cex.axis=0.8)
mtext(side=2,"Total bee colonies in the US",line=3)




#separate graph in two parts 1986-200 and 2001-2019
c=aggregate(datab$prod,by=list(datab$y),sum)
prod_thousands=c$x/1000
plot(prod_thousands~a$Group.1,type="p",xaxt="none",pch=19,ylab="",xlab="",yaxt="none")
lines(lowess(prod_thousands[1:15]~a$Group.1[1:15],f=0.5))
lines(lowess(prod_thousands[16:33]~a$Group.1[16:33],f=0.5))
mtext(side=2,"Total honey Production in the US (Thousand pounds)",line=3)
axis(1,seq(1985,2019,1),cex.axis=0.8)
axis(2,seq(-10,350,10),las=2,cex.axis=0.8)
abline(v=2001,lwd=2)

#mann-kendall test for trend
mk.test(a$x[1:15])
mk.test(a$x[16:33])
text(1993,150,"Non significant trend (p=0.9)",cex=0.9)
text(2011,210,"Non significant trend (p=0.15)",cex=0.9)


#make a panel graph with the tree last plots
par(mar=c(0,4,0.2,0.2))
layout(matrix(c(1,2),2,1,byrow = TRUE), c(0.1), c(0.35,0.4))
prod_thousands=a$x/1000
plot(prod_thousands~a$Group.1,type="p",xaxt="none",pch=19,ylab="",xlab="",yaxt="none")
lines(lowess(prod_thousands~a$Group.1,f=0.5))
mtext(side=2,"Total honey production in the USA",line=3,cex=0.8)
axis(2,seq(-10,350,10),las=2,cex.axis=0.8)
text(1985,225,"A")
#
par(mar=c(2.5,4,0,0.2))
plot(prod_thousands~a$Group.1,type="p",xaxt="none",pch=19,ylab="",xlab="",yaxt="none")
lines(lowess(prod_thousands[1:15]~a$Group.1[1:15],f=0.5))
lines(lowess(prod_thousands[16:33]~a$Group.1[16:33],f=0.5))
mtext(side=2,"Total honey production in the USA",line=3,cex=0.8)
axis(1,seq(1985,2019,1),cex.axis=0.8)
axis(2,seq(-10,350,10),las=2,cex.axis=0.8)
abline(v=2001,lwd=2)
text(1985,225,"B")


#check data trend in each state separately
#just change name of state
databTN=datab[datab$st=="TN",]
par(mfrow=c(1,2))
plot(databTN$prod~databTN$y,type="l",ylab="production")
plot(databTN$col~databTN$y,type="l",ylab="colonies")



#LOG-LOG model for 1985 - 2000
datab1=datab[1:546,]
plot(datab1$prod~datab1$col,pch=19)
plot(log(datab1$prod)~log(datab1$col),pch=19)
m1=glm(log(datab1$prod)~log(datab1$col)+datab1$y+datab1$lat+datab1$st,family=gaussian(link="identity"))
summary(m1)
summary.aov(m1)
plot(m1)
res=simulateResiduals(m1)
plotQQunif(m1)
predichos_m1=predict.lm(m1)
points(predichos_m1~log(datab1$col),col="red")
plot(predichos_m1~log(datab1$prod))
# for each 1% increase in number of colonies, honey production increases by about 1.12% - 1.16%



#model for 2001 - 2019
datab2=datab[547:1326,]
plot(datab2$prod~datab2$col,pch=19)
plot(log(datab2$prod)~log(datab2$col),pch=19)
m2=glm(log(datab2$prod)~log(datab2$col)+datab2$y+datab2$lat+datab2$st,family=gaussian(link="identity"))
summary.lm(m2)
summary.glm(m2)
AIC(m2)
plot(m2)
res=simulateResiduals(m2)
plotQQunif(m2)
predichos_m2=predict.lm(m2)
points(predichos_m2~log(datab2$col),col="red")
plot(predichos_m2~log(datab2$prod))
# for each 1% increase in number of colonies, honey production increases by about 1.05% - 1.09%


#last 4 plots in a  panel


a=layout(matrix(c(1,2,3,4),2,2,byrow = TRUE), c(0.12,0.12), c(0.3,0.38))
layout.show(a)
#
par(mar=c(0,4.5,0.5,0.2))
plot(log(datab1$prod)~log(datab1$col),pch=19,ylab="Log(HP)",xlab="",
     xaxt="none",cex.lab=1.5,cex=1.9)
text(3.6,10.8,"1985-2000",cex=1.3)
points(predichos_m1~log(datab1$col),col="red")
#
par(mar=c(0,4.5,0.5,0.2))
plot(predichos_m1~log(datab1$prod),xlab="",ylab="Log(Predicted HP)",
     xaxt="none",cex.lab=1.5)
text(7.5,10.45,"1985-2000",cex=1.3)

#
par(mar=c(4.2,4.5,0,0.2))
plot(log(datab2$prod)~log(datab2$col),pch=19,ylab="Log(HP)",xlab="Log of NC",
     cex.lab=1.5,cex=1.9)
text(3.6,10.8,"2001-2019",cex=1.3)
points(predichos_m2~log(datab2$col),col="red")
#
par(mar=c(4.2,4.5,0,0.2))
plot(predichos_m2~log(datab2$prod),xlab="Log of HP", ylab="Log(Predicted HP)",cex.lab=1.5)
text(7.5,10.65,"2001-2019",cex=1.3)

