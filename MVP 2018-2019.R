# Used library
library(pheatmap)
library(leaps)
library(ggplot2)
library(dplyr)
library(ggrepel)
library(glmnet)
library(class)
library(tree)
library(data.table)
library(xtable)

# Load and manipulate data
fullstat<-read.csv("PlayerStat_1.csv")
fullstat$P.A=fullstat$PS.G*fullstat$AST
fullstat$P.A.R=fullstat$P.A*fullstat$TRB
stat<-fullstat[,c(1,3,4,7:10,14,16,19,24:27,30,31,44)]
stat$P.A=stat$PS.G*stat$AST
stat$P.A.R=stat$P.A*stat$TRB
adv.stat<-fullstat[,c(1,3,7:9,32:49)]
pred.2019<-read.csv("Predict.csv")
pred.2019$P.A=pred.2019$PS.G*pred.2019$AST
pred.2019$P.A.R=pred.2019$P.A*pred.2019$TRB


## Predictors selection
cor1<-cor(stat[,c(5,7:19)])
cor2<-cor(adv.stat[,c(4,6:23)])
pheatmap(cor1,display_numbers = TRUE)
# -FGperc-TS, since corr with eFGperc
pheatmap(cor2,display_numbers = TRUE)
# -ORBperc-DRBperc, due to corr wuth TRBperc,-VORP-DWS-OWS, due to corr with WS; -WS.48-BPM due to corr with OBPM
stat<-select(stat,-FGperc,-TS)
adv.stat<-select(adv.stat,-ORBperc,-DRBperc,-VORP,-DWS,-OWS,-WS.48,-BPM)

## Methodology 1: Linear regression

BSS<-regsubsets(MVPGrade~TmRk+G+X3Pperc+eFGperc+AST+TRB+BLK+STL+PS.G+P.A+P.A.R,data = stat,nvmax=16)
BSS.summary<-summary(BSS)
which.max(BSS.summary$adjr2)
coef(BSS,which.max(BSS.summary$adjr2))
lm1.1<-lm(MVPGrade~TmRk+G+AST+TRB+BLK+PS.G+P.A+P.A.R,data = stat)
summary(lm1.1)
aa<-predict(lm1.1,newdata=pred.2019,interval = "confidence")
sort(aa[,1],decreasing = TRUE)[1:5]
name<-pred.2019[c(42,7,8,14,10),1]
Grade<-aa[c(42,7,8,14,10),1]
#Using Latex to outut the reuslts table
df1<-data.frame(name,Grade)
colnames(df1)<-c("Name","MVPGrade")
xtable(df1,digits = 5,caption = "Basic linear regression top 5")
P.Aplot<-filter(stat,MVPGrade>0&P.A>mean(P.A))
ggplot(P.Aplot,aes(x=P.A,y=MVPGrade,size=BLK))+geom_point()+geom_smooth(method = "lm")
# RESULTS: Russell Westbrook,James Harden,Giannis Antetokounmpo,LeBron James,Nikola Jokic


BSS2<-regsubsets(MVPGrade~X3PAr+FTr+TRBperc+ASTperc+STLperc+BLKperc
                 +TOVperc+USGperc+WS,data = adv.stat)
BSS2.summary<-summary(BSS2)
coef(BSS2,which.max(BSS2.summary$adjr2))
lm2.1<-lm(MVPGrade~X3PAr+FTr+ASTperc+TOVperc+USGperc+TOVperc+WS,data = adv.stat)
summary(lm2.1)
adv.pred<-predict(lm2.1,newdata = pred.2019,interval = "confidence")
Grade2.1<-sort(adv.pred[,1],decreasing = TRUE)[1:5]#7,8,19,10,18
dddd<-lm(MVPGrade~WS,data = adv.stat)
Name2.1<-pred.2019[c(7,8,19,10,18),1]
#Using Latex to outut the reuslts table
df2<-data.frame(Name2.1,Grade2.1)
colnames(df2)<-c("Name","MVPGrade")
df2
xtable(df2,digits=5,caption = "Advanced linear regression top 5")
WSplot<-filter(adv.stat,WS>mean(WS)&MVPGrade>0)
ggplot(WSplot,aes(x=WS,y=MVPGrade))+geom_point()+geom_smooth(method = "lm")

#James Harden, Giannis Antetokounmpo,Paul George,,Nikola Jokic, Kevin Durant

summary(adv.stat)
plot.adv<-filter(adv.stat,WS>4)
ggplot(plot.adv,aes(x=OBPM,y=DBPM,size=USGperc,col=MVP))+geom_point()+geom_smooth()+geom_text_repel(aes(label=..Name),subset(plot.adv,MVP==1),size=6)

## Methodology 2: Lasso
x.basic<-model.matrix(MVPGrade~TmRk+G+X3Pperc+eFGperc+AST+TRB+BLK+STL
                +PS.G+P.A+P.A.R,data = fullstat)[,-1]
y.basic<-stat$MVPGrade
set.seed(2)
cv.out<-cv.glmnet(x.basic,y.basic,nfolds=10)
bestlam1=cv.out$lambda.min
bestlam1
lasso1<-glmnet(x.basic,y.basic,alpha=1,lambda=bestlam1)
coef(lasso1)

x.adv<-model.matrix(MVPGrade~TS+X3PAr+FTr+TRBperc+ASTperc+STLperc+BLKperc
                +TOVperc+USGperc+WS+BPM
                ,data = fullstat)[,-1]
y.adv<-stat$MVPGrade
set.seed(2)
cv.out<-cv.glmnet(x.adv,y.adv,nfolds=10)
bestlam2=cv.out$lambda.min
bestlam2
lasso1<-glmnet(x.adv,y.adv,alpha=1,lambda=bestlam2)
coef(lasso2)

x<-model.matrix(MVPGrade~TmRk+G+X3Pperc+eFGperc+AST+TRB+BLK+STL+PS.G+TS+P.A+P.A.R
                +X3PAr+FTr+TRBperc+ASTperc+STLperc+BLKperc
                +TOVperc+USGperc+WS+BPM
                ,data = fullstat)[,-1]
y<-stat$MVPGrade
set.seed(2)
cv.out<-cv.glmnet(x,y,nfolds=10)
bestlam3=cv.out$lambda.min
bestlam3
lasso3<-glmnet(x,y,alpha=1,lambda=bestlam3)
coef(lasso1)

x.2019<-select(pred.2019,"TmRk","G","X3Pperc","eFGperc","AST","TRB","BLK","STL","PS.G",
               "TS","P.A","P.A.R","X3PAr","FTr","TRBperc","ASTperc","STLperc","BLKperc"
               ,"TOVperc","USGperc","WS","BPM")
x.2019<-as.matrix(x.2019)
lasso.pred.full<-predict(lasso3,s=bestlam3,newx=x.2019)
arrayInd(order(lasso.pred.full,decreasing=TRUE)[1:5],dim(lasso.pred.full))
#Using Latex to outut the reuslts table
df3<-data.frame(pred.2019[c(7,42,8,14,10),1],lasso.pred.full[c(7,42,8,14,10),1])
colnames(df3)<-c("Name","MVPGrade")
df3
xtable(df3,digits=5,caption = "LASSO top 5 players")
#RESULT
#James Harden, Russell Westbrook, Giannis Antetokounmpo, LeBron James  Nikola Jokic 
#1.4145872 1.3267262 1.2110337 0.9766391 0.8897581


## Methodology 3: Logistic Regression
glm.train<-glm(MVP~TmRk+G+X3Pperc+eFGperc+AST+TRB+BLK+STL+PS.G+P.A+P.A.R
               ,data = stat,family=binomial)
summary(glm.train)
newpred<-read.csv("Predict_LogReg.csv")
newpred$P.A=newpred$PS.G*newpred$AST
newpred$P.A.R=newpred$P.A*newpred$TRB
glm.pred=predict(glm.train,newdata = newpred,type = "response")
sort(glm.pred,decreasing = TRUE)[1:5]
newpred[c(190,103,158,107,245),1]
#Using Latex to outut the reuslts table
df5<-data.frame(newpred[c(190,103,158,107,245),1],sort(glm.pred,decreasing = TRUE)[1:5])
colnames(df5)<-c("Name","Probabilty")
xtable(df5,digits=5,caption = "Logistic regression top 5 players (Basic)")
# RESULTS:
# James Harden,  Stephen Curry,   Paul George, Kawhi Leonard,  Russell Westbrook          
# 0.8828692 0.4494207 0.4058348 0.4013147 0.1473801 

glm.train.adv<-glm(MVP~X3PAr+FTr+TRBperc+ASTperc+STLperc+BLKperc
                  +TOVperc+USGperc+WS,data = adv.stat)

glm.pred.adv=predict(glm.train.adv,newdata=newpred,type="response")
sort(glm.pred.adv,decreasing = TRUE)[1:5]
newpred[c(190,25,158,78,66),1]
#Using Latex to outut the reuslts table
df6<-data.frame(newpred[c(190,25,158,78,66),1],sort(glm.pred.adv,decreasing = TRUE)[1:5])
colnames(df6)<-c("Name","Probability")
xtable(df6,digits = 5,caption = "Logistic regression top 5 players (advanced)")
# RESULTS:
# James Harden   Giannis Antetokounmpo Paul George  Nikola Jokic  Kevin Durant 
# 0.04425399 0.03728022 0.03224269 0.03144246 0.02904269 

## Methodology 4: KNN

qqq<-select(pred.2019,"TmRk","G","X3Pperc","eFGperc","AST","TRB","BLK","STL","PS.G","P.A"
            ,"P.A.R","X3PAr","FTr","TRBperc","ASTperc","STLperc","BLKperc"
            ,"TOVperc","USGperc","WS")
xxtrain<-fullstat[,c("TmRk","G","X3Pperc","eFGperc","AST","TRB","BLK","STL","PS.G","P.A"
                     ,"P.A.R","X3PAr","FTr","TRBperc","ASTperc","STLperc","BLKperc"
                     ,"TOVperc","USGperc","WS")]

qqq<-select(pred.2019,"TmRk","G","PS.G","P.A.R","TRBperc","ASTperc","STLperc","BLKperc"
                     ,"TOVperc","USGperc","WS","BPM")
xxtrain<-fullstat[,c("TmRk","G","PS.G","P.A.R","TRBperc","ASTperc","STLperc","BLKperc"
                     ,"TOVperc","USGperc","WS","BPM")]

# -FGperc-TS, since corr with eFGperc
# -ORBperc-DRBperc, due to corr wuth TRBperc,-VORP-DWS-OWS, due to corr with WS; -WS.48-BPM due to corr with OBPM


yytrain<-fullstat[,"MVP"]
knn.pred3<-knn(xxtrain,qqq,yytrain,k=3)
summary(knn.pred3)
filter(pred.2019,knn.pred3==1)[,1]
# James Harden

## Conclusion
install.packages("RColorBrewer")
library(RColorBrewer)
library(ggplot2)
library(dplyr)
fullstat$ref=fullstat$..Name&fullstat$Season
ggplot(subset(fullstat,MVPGrade>0),aes(x=OBPM,y=DBPM,color=MVP))+geom_point(size=4)+geom_smooth()+geom_text(aes(label=ifelse(MVP>0,as.character(ï..Name),'')),hjust=0,vjust=0)
ggplot(subset(fullstat,MVPGrade>0),aes(x=OWS,y=DWS,color=MVP))+geom_point(size=4)+geom_smooth()+geom_text(aes(label=ifelse(MVP>0,as.character(ï..Name),'')),hjust=0,vjust=0)
colnames(fullstat)
ggplot(pred.2019,aes(x=OWS,y=DWS))+geom_point(size=3,color = brewer.pal(7, "Set1")[2])+geom_text(aes(label=ifelse(ï..Name=="James Harden",as.character(ï..Name),'')),hjust=0.7,vjust=-1,size=7,color="red")
ggplot(pred.2019,aes(x=OBPM,y=DBPM))+geom_point(size=3,color = brewer.pal(7, "Set1")[2])+geom_text(aes(label=ifelse(ï..Name=="James Harden",as.character(ï..Name),'')),hjust=0.5,vjust=-1,size=4)

pred.2019[7,c(1,42:44)]
summary(pred.2019$DWS)
summary(pred.2019$WS)
which.max(pred.2019$WS)
pred.2019[8,1]
