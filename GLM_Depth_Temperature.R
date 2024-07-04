library("openxlsx")
setwd ("/home/yushi/Документы/Широкова/Широкова Ю.А/Важные доки/По работе/Работа над статьями/Статья по магистерской")

#ATP
#Of
atp <- read.xlsx("Raw_data_from_Ommatogammarus_in_experiments.xlsx", sheet=2, rows=c(1, 2:99), cols=1:7)
boxplot(atp$Value ~ atp$Temperature)
boxplot(atp$Value ~ atp$Depth)
latp <- lm(atp$Value ~ atp$Temperature + atp$Depth)
qqnorm(latp$residuals); qqline(latp$residuals, col="red")
shapiro.test(latp$residuals)
plot(latp$fitted.values, latp$residuals)
abline(h=0, col="red")
#summary(latp)
summary(glm(Value ~ Temperature + Depth + Temperature*Depth, data=atp))
#Oa
atp2<- read.xlsx("Raw_data_from_Ommatogammarus_in_experiments.xlsx", sheet = 2, rows=c(1, 100:145), cols=1:7)
boxplot(atp2$Value ~ atp2$Temperature)
boxplot(atp2$Value ~ atp2$Depth)
latp2 <- lm(atp2$Value ~ atp2$Temperature + atp2$Depth)
qqnorm(latp2$residuals); qqline(latp2$residuals, col="red")
shapiro.test(latp2$residuals)
plot(latp2$fitted.values, latp2$residuals)
abline(h=0, col="red")
summary(glm(Value ~ Temperature + Depth + Temperature*Depth, data=atp2))



#ADP
#Of
adp <- read.xlsx("Raw_data_from_Ommatogammarus_in_experiments.xlsx", sheet=3, rows=c(1, 2:107), cols=1:7)
boxplot(adp$Value ~ adp$Temperature)
boxplot(adp$Value ~ adp$Depth)
ladp <- lm(adp$Value ~ adp$Temperature + adp$Depth)
qqnorm(ladp$residuals); qqline(ladp$residuals, col="red")
shapiro.test(ladp$residuals)
plot(ladp$fitted.values, ladp$residuals)
abline(h=0, col="red")
summary(glm(Value ~ Temperature + Depth + Temperature*Depth, data=adp))
#Oa
adp2 <- read.xlsx("Raw_data_from_Ommatogammarus_in_experiments.xlsx", sheet=3, rows=c(1, 108:151), cols=1:7)
boxplot(adp2$Value ~ adp2$Temperature)
boxplot(adp2$Value ~ adp2$Depth)
ladp2 <- lm(adp2$Value ~ adp2$Temperature + adp2$Depth)
qqnorm(ladp2$residuals); qqline(ladp2$residuals, col="red")
shapiro.test(ladp2$residuals)
plot(ladp2$fitted.values, ladp2$residuals)
abline(h=0, col="red")
summary(glm(Value ~ Temperature + Depth + Temperature*Depth, data=adp2))




#AMP
#Of
amp <- read.xlsx("Raw_data_from_Ommatogammarus_in_experiments.xlsx", sheet=4, rows=c(1, 2:107), cols=1:7)
boxplot(amp$Value ~ amp$Temperature)
boxplot(amp$Value ~ amp$Depth)
lamp <- lm(amp$Value ~ amp$Temperature + amp$Depth)
qqnorm(lamp$residuals); qqline(lamp$residuals, col="red")
shapiro.test(lamp$residuals)
plot(lamp$fitted.values, lamp$residuals)
abline(h=0, col="red")
summary(glm(Value ~ Temperature + Depth + Temperature*Depth, data=amp))
#Oa
amp2 <- read.xlsx("Raw_data_from_Ommatogammarus_in_experiments.xlsx", sheet=4, rows=c(1, 108:150), cols=1:7)
boxplot(amp2$Value ~ amp2$Temperature)
boxplot(amp2$Value ~ amp2$Depth)
lamp2 <- lm(amp2$Value ~ amp2$Temperature + amp2$Depth)
qqnorm(lamp2$residuals); qqline(lamp2$residuals, col="red")
shapiro.test(lamp2$residuals)
plot(lamp2$fitted.values, lamp2$residuals)
abline(h=0, col="red")
summary(glm(Value ~ Temperature + Depth + Temperature*Depth, data=amp2))




#AEC
#Of
aec <- read.xlsx("Raw_data_from_Ommatogammarus_in_experiments.xlsx", sheet=5, rows=c(1, 2:99), cols=1:7)
boxplot(aec$Value ~ aec$Temperature)
boxplot(aec$Value ~ aec$Depth)
laec <- lm(aec$Value ~ aec$Temperature + aec$Depth)
qqnorm(laec$residuals); qqline(laec$residuals, col="red")
shapiro.test(laec$residuals)
plot(laec$fitted.values, laec$residuals)
abline(h=0, col="red")
summary(glm(Value ~ Temperature + Depth + Temperature*Depth, data=aec))
#Oa
aec2 <- read.xlsx("Raw_data_from_Ommatogammarus_in_experiments.xlsx", sheet=5, rows=c(1, 100:142), cols=1:7)
boxplot(aec2$Value ~ aec2$Temperature)
boxplot(aec2$Value ~ aec2$Depth)
laec2 <- lm(aec2$Value ~ aec2$Temperature + aec2$Depth)
qqnorm(laec2$residuals); qqline(laec2$residuals, col="red")
shapiro.test(laec2$residuals)
plot(laec2$fitted.values, laec2$residuals)
abline(h=0, col="red")
summary(glm(Value ~ Temperature + Depth + Temperature*Depth, data=aec2))




#Glucose
#Of
glucose <- read.xlsx("Raw_data_from_Ommatogammarus_in_experiments.xlsx", sheet=6, rows=c(1, 2:131), cols=1:7)
boxplot(glucose$Value ~ glucose$Temperature)
boxplot(glucose$Value ~ glucose$Depth)
lglucose <- lm(glucose$Value ~ glucose$Temperature + glucose$Depth)
qqnorm(lglucose$residuals); qqline(lglucose$residuals, col="red")
shapiro.test(lglucose$residuals)
plot(lglucose$fitted.values, lglucose$residuals)
abline(h=0, col="red")
summary(glm(Value ~ Temperature + Depth + Temperature*Depth, data=glucose))
#Oa
glucose2 <- read.xlsx("Raw_data_from_Ommatogammarus_in_experiments.xlsx", sheet=6, rows=c(1, 132:176), cols=1:7)
boxplot(glucose2$Value ~ glucose2$Temperature)
boxplot(glucose2$Value ~ glucose2$Depth)
lglucose2 <- lm(glucose2$Value ~ glucose2$Temperature + glucose2$Depth)
qqnorm(lglucose2$residuals); qqline(lglucose2$residuals, col="red")
shapiro.test(lglucose2$residuals)
plot(lglucose2$fitted.values, lglucose2$residuals)
abline(h=0, col="red")
summary(glm(Value ~ Temperature + Depth + Temperature*Depth, data=glucose2))




#Glycogen
#Of
glycogen <- read.xlsx("Raw_data_from_Ommatogammarus_in_experiments.xlsx", sheet=7, rows=c(1, 2:129), cols=1:7)
boxplot(glycogen$Value ~ glycogen$Temperature)
boxplot(glycogen$Value ~ glycogen$Depth)
lglycogen <- lm(glycogen$Value ~ glycogen$Temperature + glycogen$Depth)
qqnorm(lglycogen$residuals); qqline(lglycogen$residuals, col="red")
shapiro.test(lglycogen$residuals)
plot(lglycogen$fitted.values, lglycogen$residuals)
abline(h=0, col="red")
summary(glm(Value ~ Temperature + Depth + Temperature*Depth, data=glycogen))
#Oa
glycogen2 <- read.xlsx("Raw_data_from_Ommatogammarus_in_experiments.xlsx", sheet=7, rows=c(1, 130:174), cols=1:7)
boxplot(glycogen2$Value ~ glycogen2$Temperature)
boxplot(glycogen2$Value ~ glycogen2$Depth)
lglycogen2 <- lm(glycogen2$Value ~ glycogen2$Temperature + glycogen2$Depth)
qqnorm(lglycogen2$residuals); qqline(lglycogen2$residuals, col="red")
shapiro.test(lglycogen2$residuals)
plot(lglycogen2$fitted.values, lglycogen2$residuals)
abline(h=0, col="red")
summary(glm(Value ~ Temperature + Depth + Temperature*Depth, data=glycogen2))




#POD
#Of
pod <- read.xlsx("Raw_data_from_Ommatogammarus_in_experiments.xlsx", sheet=8, rows=c(1, 2:169), cols=1:7)
boxplot(pod$Value ~ pod$Temperature)
boxplot(pod$Value ~ pod$Depth)
lpod <- lm(pod$Value ~ pod$Temperature + pod$Depth)
qqnorm(lpod$residuals); qqline(lpod$residuals, col="red")
shapiro.test(lpod$residuals)
plot(lpod$fitted.values, lpod$residuals)
abline(h=0, col="red")
summary(glm(Value ~ Temperature + Depth + Temperature*Depth, data=pod))
#Oa
pod2 <- read.xlsx("Raw_data_from_Ommatogammarus_in_experiments.xlsx", sheet=8, rows=c(1, 170:301), cols=1:7)
boxplot(pod2$Value ~ pod2$Temperature)
boxplot(pod2$Value ~ pod2$Depth)
lpod2 <- lm(pod2$Value ~ pod2$Temperature + pod2$Depth)
qqnorm(lpod2$residuals); qqline(lpod2$residuals, col="red")
shapiro.test(lpod2$residuals)
plot(lpod2$fitted.values, lpod2$residuals)
abline(h=0, col="red")
summary(glm(Value ~ Temperature + Depth + Temperature*Depth, data=pod2))




#CAT
#Of
cat <- read.xlsx("Raw_data_from_Ommatogammarus_in_experiments.xlsx", sheet=9, rows=c(1, 2:170), cols=1:7)
boxplot(cat$Value ~ cat$Temperature)
boxplot(cat$Value ~ cat$Depth)
lcat <- lm(cat$Value ~ cat$Temperature + cat$Depth)
qqnorm(lcat$residuals); qqline(lcat$residuals, col="red")
shapiro.test(lcat$residuals)
plot(lcat$fitted.values, lcat$residuals)
abline(h=0, col="red")
summary(glm(Value ~ Temperature + Depth + Temperature*Depth, data=cat))
#Oa
cat2 <- read.xlsx("Raw_data_from_Ommatogammarus_in_experiments.xlsx", sheet=9, rows=c(1, 171:301), cols=1:7)
boxplot(cat2$Value ~ cat2$Temperature)
boxplot(cat2$Value ~ cat2$Depth)
lcat2 <- lm(cat2$Value ~ cat2$Temperature + cat2$Depth)
qqnorm(lcat2$residuals); qqline(lcat2$residuals, col="red")
shapiro.test(lcat2$residuals)
plot(lcat2$fitted.values, lcat2$residuals)
abline(h=0, col="red")
summary(glm(Value ~ Temperature + Depth + Temperature*Depth, data=cat2))




#GST
#Of
gst <- read.xlsx("Raw_data_from_Ommatogammarus_in_experiments.xlsx", sheet=10, rows=c(1, 2:171), cols=1:7)
boxplot(gst$Value ~ gst$Temperature)
boxplot(gst$Value ~ gst$Depth)
lgst <- lm(gst$Value ~ gst$Temperature + gst$Depth)
qqnorm(lgst$residuals); qqline(lgst$residuals, col="red")
shapiro.test(lgst$residuals)
plot(lgst$fitted.values, lgst$residuals)
abline(h=0, col="red")
summary(glm(Value ~ Temperature + Depth + Temperature*Depth, data=gst))
#Oa
gst2 <- read.xlsx("Raw_data_from_Ommatogammarus_in_experiments.xlsx", sheet=10, rows=c(1, 172:302), cols=1:7)
boxplot(gst2$Value ~ gst2$Temperature)
boxplot(gst2$Value ~ gst2$Depth)
lgst2 <- lm(gst2$Value ~ gst2$Temperature + gst2$Depth)
qqnorm(lgst2$residuals); qqline(lgst2$residuals, col="red")
shapiro.test(lgst2$residuals)
plot(lgst2$fitted.values, lgst2$residuals)
abline(h=0, col="red")
summary(glm(Value ~ Temperature + Depth + Temperature*Depth, data=gst2))




#LDH
#Of
ldh <- read.xlsx("Raw_data_from_Ommatogammarus_in_experiments.xlsx", sheet=11, rows=c(1, 2:141), cols=1:7)
boxplot(ldh$Value ~ ldh$Temperature)
boxplot(ldh$Value ~ ldh$Depth)
lldh <- lm(ldh$Value ~ ldh$Temperature + ldh$Depth)
qqnorm(lldh$residuals); qqline(lldh$residuals, col="red")
shapiro.test(lldh$residuals)
plot(lldh$fitted.values, lldh$residuals)
abline(h=0, col="red")
summary(glm(Value ~ Temperature + Depth + Temperature*Depth, data=ldh))
#Oa
ldh2 <- read.xlsx("Raw_data_from_Ommatogammarus_in_experiments.xlsx", sheet=11, rows=c(1, 142:244), cols=1:7)
boxplot(ldh2$Value ~ ldh2$Temperature)
boxplot(ldh2$Value ~ ldh2$Depth)
lldh2 <- lm(ldh2$Value ~ ldh2$Temperature + ldh2$Depth)
qqnorm(lldh2$residuals); qqline(lldh2$residuals, col="red")
shapiro.test(lldh2$residuals)
plot(lldh2$fitted.values, lldh2$residuals)
abline(h=0, col="red")
summary(glm(Value ~ Temperature + Depth + Temperature*Depth, data=ldh2))
