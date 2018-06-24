tourism <- read.csv("C:/Users/praneetha/Desktop/statistics/data_analysis1/Exams/project/project_org.csv",sep=",",header=TRUE)


par(mex=0.5)
pairs(tourism,gap=0,cex.labels = 0.9)

########## fitting a regression line ############

fit<-lm(best_destination~Climate+Housing+HealthCare+Crime+Transportation+Education+Arts+Recreation+economy,data=tourism)
summary(fit)
anova(fit)

############Checking for normality ########
plot(fitted(fit),resid(fit))
qq<-qqnorm(resid(fit))

#############Comparing 2 models ##########
m1<-lm(best_destination~Climate+Housing+HealthCare+Crime+Transportation+Education+Arts+Recreation+economy,data=tourism)
m2<-lm(best_destination~Climate+Housing+HealthCare+Transportation+Education+Arts+economy,data=tourism)
anova(m1,m2)
######checking for constant error variance#########
var.test(m1,m2)

#########Checking linearity##############
library(car)
cr.plots(fit)
cr.plots(fit,ask=FALSE)

############ Correlation test #################
cor.test(tourism$best_destination,tourism$Climate+tourism$Housing+tourism$HealthCare+tourism$Crime+tourism$Transportation+tourism$Education+tourism$Arts+tourism$Recreation+tourism$economy)

######### spotting multi collinearity ############
spreadLevelPlot(fit)
vif(fit)

########outlier test#########
lev=hat(model.matrix(fit))
plot(lev)


###### Backward model selection ##########3
step(m1,data=tourism,direction = "backward")

######### Predict ################
predicted<-predict(m2, data=tourism)
sort(predicted)

library(xlsx)
write.xlsx(predicted,"predicted.xlsx")


