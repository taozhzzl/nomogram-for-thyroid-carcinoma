require(rms)
require(survival)
require(Hmisc)
require(MASS)
require(pROC)
require(ggplot2)
par(family="Hei")
setwd("/Users/zhanghaozhi/Desktop/nomogram")
# setwd("E:\\nomogram\\")

rawdata<-read.csv("rawdata.csv")
names(rawdata)

年龄<-rawdata$AGE
位置<-factor(rawdata$POSITION, levels = c(0,1,2,3),
           labels = c("峡叶","上","中","下"))
直径<-factor(rawdata$DIAMETER, levels = c(1,2,3),
           labels = c("小于0.5","0.5-1","大于1"))
纵横比<-factor(rawdata$VHRATIO, levels = c(0,1),
            labels = c("小于1","大于1"))
钙化<-factor(rawdata$CALCIFICATION, levels = c(0,1),
            labels = c("无","有"))
血流<-factor(rawdata$BLOOD, levels = c(0,1),
           labels = c("不丰富","丰富"))
边界<-factor(rawdata$BORDER, levels = c(0,1),
           labels = c("光滑","不规则"))
形状<-factor(rawdata$SHAPE, levels = c(0,1),
           labels = c("规则","不规则"))
回声<-factor(rawdata$ECHO, levels = c(0,1),
           labels = c("等回声或高回声","低回声"))
被膜020<-factor(rawdata$COATING020, levels = c(1,2),
              labels = c("小于0.20","大于0.20"))
被膜025<-factor(rawdata$COATING025, levels = c(1,2),
              labels = c("小于0.25","大于0.25"))
# 被膜<-factor(rawdata$COATING, levels = c(1,2,3),
#               labels = c("小于0.2","0.2-0.25","大于0.25"))
中央区病理<-factor(rawdata$MIDDLEAREAPATHO, levels = c(0,1),
              labels = c("良性","恶性"))
侧颈区病理<-factor(rawdata$LATERALAREAPATHO, levels = c(0,1),
              labels = c("良性","恶性"))
桥本<-factor(rawdata$HASHIMOTO, levels = c(0,1),
           labels = c("否","是"))
多发<-factor(rawdata$MULTIPLE, levels = c(0,1),
           labels = c("否","是"))

logistic1.full <- glm(中央区病理 ~ 年龄 + 位置 + 直径 + 纵横比 + 钙化 + 血流 + 边界 + 形状 + 回声 + 被膜020 + 被膜025 + 桥本 + 多发, family=binomial())
exp(coef(logistic1.full))
summary(logistic1.full)
logistic1.select <- stepAIC(logistic1.full)
exp(coef(logistic1.select))
summary(logistic1.select)

p.logistic1.full <- predict(logistic1.full, type="response")
p.logistic1.select <- predict(logistic1.select, type="response")

roc.logistic1.full <- roc(中央区病理,p.logistic1.full)
roc.logistic1.select <- roc(中央区病理,p.logistic1.select)

ci(roc.logistic1.full)
ci(roc.logistic1.select)
plot(roc.logistic1.full)
plot(roc.logistic1.select,add=TRUE,col=2)

coords(roc.logistic1.select,"b",best.method = "youden",transpose = TRUE)
coords(roc.logistic1.select,"b",best.method = "closest.topleft",transpose = TRUE)


p.ggplot <- ggplot() + geom_line(aes(x = 钙化, y= p.logistic1.select))
p.ggplot + theme(text = element_text(family='Hei'))

ddist<-datadist(年龄,位置,直径,纵横比,钙化,血流,边界,形状,回声,被膜020,被膜025,桥本,多发)
options(datadist='ddist')

f <- lrm(中央区病理 ~ 年龄 + 位置 + 直径 + 纵横比 + 钙化 + 血流 + 边界 + 形状 + 回声 + 被膜020 + 被膜025 + 桥本 + 多发,x=T, y=T)
# f <- lrm(中央区病理 ~ 年龄 + 位置 + 直径 + 纵横比 + 钙化 + 边界 + 回声 + 被膜020 + 被膜025)
nom <- nomogram(f, fun=plogis,
                fun.at=c(.001, .01, .05, seq(.1,.9, by=.1), .95, .99, .999),
                lp=F, funlabel="中央区病理")
plot(nom)

validate(f,method="boot",B=1000,dxy=T) #Bootstrap自抽样法
cal<-calibrate(f,method="boot",B=1000)
plot(cal,scat1d.opts=list(nhistSpike=240,side=1,frac=0.08,lwd=1,nint=50))
lines(cal, lwd=2,lty=3,col=c(rgb(255,0,0,maxColorValue=255)))
abline(0,1,lty =5,lwd=2,col=c(rgb(0,0,255,maxColorValue= 255)))
rcorrcens(中央区病理 ~ predict(f))
