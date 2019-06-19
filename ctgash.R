
CTG<-read.csv("C:/Users/AKHIL/Desktop/New folder/CTG.csv")
dim(CTG)
View(head(CTG))
library(party)
a<-table(CTG$NSP)
a
CTG$NSP<-factor(CTG$NSP)



View(a)
set.seed(121)
CTG_sam<-sample(2,nrow(CTG),replace=TRUE,prob=c(0.8,0.2))
CTG_train<-CTG[CTG_sam==1,]
View(CTG_train)
CTG_test<-CTG[CTG_sam==2,]

library(dplyr)


CTG_1<-filter(CTG_train,NSP == 3)
View(CTG_1) 
dim(CTG_1)
head(CTG_1)

CTG_2<-filter(CTG_train,NSP == 2)
View(CTG_2) 
dim(CTG_2)

CTG_r<-rbind(CTG_train,CTG_1,CTG_1,CTG_1,CTG_1,CTG_2,CTG_2,CTG_2,CTG_2)

CTG_mod2<-ctree(NSP~.,data=CTG_r)#controls = ctree_control(mincriterion = 0.6,minsplit = 200))
CTG_pred2<-predict(CTG_mod2,CTG_test,type="response")
View(CTG_pred2)
CTG_df2<-data.frame(CTG_pred2,CTG_test$NSP)
View(CTG_df2)
colnames(CTG_df2)<-c("predict","actual")

tab2<-table(CTG_df2$predict,CTG_df2$actual)
tab2
acc_r<-sum(diag(tab2))/sum(tab2)
acc_r
plot(CTG_mod2)




CTG_mod1<-ctree(NSP~LB+AC,data=CTG_train)
CTG_pred1<-predict(CTG_mod1,CTG_test,type="response")
View(CTG_pred1)
CTG_df1<-data.frame(CTG_pred1,CTG_test$NSP)
View(CTG_df1)
colnames(CTG_df1)<-c("predict","actual")

tab1<-table(CTG_df1$predict,CTG_df1$actual)
tab1


CTG_mod3<-ctree(NSP~.,data=CTG_train)
CTG_pred3<-predict(CTG_mod3,CTG_test,type="response")
View(CTG_pred3)
CTG_df3<-data.frame(CTG_pred3,CTG_test$NSP)
View(CTG_df3)
colnames(CTG_df3)<-c("predict","actual")

tab3<-table(CTG_df3$predict,CTG_df3$actual)
tab3

