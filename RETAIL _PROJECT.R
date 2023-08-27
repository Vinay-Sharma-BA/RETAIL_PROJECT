## RETAIL PROJECT

library(tidymodels)
library(visdat)
library(pROC)
library(car)
library(ggplot2)
library(ROCit)
library(dplyr)
library(stringr)
library(recipes)
library(dplyr) 
library(stringr) 
library(caTools) 
library(caret)
library(knitr)
library(vip)


#coding begins
store_train=read.csv("C:/Users/HP/Downloads/FOR PROJECT SUBMISSION/RETAIL/store_train.csv",stringsAsFactors = FALSE)
store_train$country=as.character(store_train$country)
store_train$State=as.character(store_train$State)
store_train$CouSub=as.character(store_train$CouSub)
store_train$storecode=substr(store_train$storecode,1,5)
table(store_train$storecode)
table(store_train$countyname)
store_test=read.csv("C:/Users/HP/Downloads/FOR PROJECT SUBMISSION/RETAIL/store_test.csv",stringsAsFactors = FALSE)
store_test$country=as.character(store_test$country)
store_test$State=as.character(store_test$State)
store_test$CouSub=as.character(store_test$CouSub)
store_test$storecode=substr(store_test$storecode,1,5)

glimpse(store_train)
vis_dat(store_train)

#DATA PREPARATION



#recipe

dp_pipe=recipe(store~.,data=store_train) %>% 
  update_role(Id,new_role = "drop_vars") %>%
  update_role(country,State,CouSub,countyname,storecode,Areaname,countytownname,state_alpha,store_Type,new_role="to_dummies") %>% 
  step_rm(has_role("drop_vars")) %>% 
  step_unknown(has_role("to_dummies"),new_level="__missing__") %>% 
  step_other(has_role("to_dummies"),threshold =0.05,other="__other__") %>% 
  step_dummy(has_role("to_dummies")) %>%
  step_impute_median(all_numeric(),-all_outcomes())
dp_pipe=prep(dp_pipe)

train=bake(dp_pipe,new_data=NULL)
test=bake(dp_pipe,new_data=store_test)

vis_dat(train)

set.seed(2)
s=sample(1:nrow(train),0.8*nrow(train))
t1=train[s,]
t2_test=train[-s,]


for_vif=lm(store~. 
          -CouSub_X__other__ 
          -storecode_X__other__ 
          -state_alpha_ME
          -state_alpha_NH 
          -state_alpha_TX 
          -state_alpha_VT 
          -state_alpha_X__other__  
          -store_Type_X__other__
          -sales0
          -sales2
          -sales3
          ,data=t1)
vif(for_vif)
sort(vif(for_vif),decreasing=T)[1:5]
summary(for_vif)

log_fit=glm(store~.
            -CouSub_X__other__ 
            -storecode_X__other__ 
            -state_alpha_ME
            -state_alpha_NH 
            -state_alpha_TX 
            -state_alpha_VT 
            -state_alpha_X__other__  
            -store_Type_X__other__
            -sales0
            -sales2
            -sales3
            ,data=t1,
            family = "binomial")
summary(log_fit)

log_fit=stats::step(log_fit)
summary(log_fit)
formula(log_fit)
#
log_fit=glm(store ~ storecode_NCNTY + countytownname_X__other__, data=t1,family="binomial")

summary(log_fit)

#performance on t2_test with auc score

val.score=predict(log_fit,newdata = t2_test,type='response')

pROC::auc(pROC::roc(t2_test$store,val.score))


###now fitting the model on the entire data

for_vif=lm(store~storecode_NCNTY + countytownname_X__other__
           ,data=train)

vif(for_vif)
sort(vif(for_vif),decreasing=T)[1:3]
summary(for_vif)

log_fit_final=glm(store ~ storecode_NCNTY + countytownname_X__other__
            ,data=train,family="binomial")
summary(log_fit_final)

log_fit_final=stats::step(log_fit_final)
summary(log_fit_final)
formula(log_fit_final)

log_fit_final=glm(store ~ storecode_NCNTY + countytownname_X__other__,data=train,family="binomial")

##finding cut off for hard classes
Predicted=predict(log_fit_final,newdata = train,type='response')
real=train$store
cutoff=0.7
cutoffs=seq(0.001,0.999,0.001)
cut_off_data=data.frame(cutoff=99,Sn=99,Sp=99,KS=99,precision=99,F1=99)

for (cutoff in cutoffs) {
  predicted=as.numeric(Predicted>cutoff)
  
  TP=sum(real==1 & predicted==1)
  TN=sum(real==0 & predicted==0)
  FP=sum(real==0 & predicted==1)
  FN=sum(real==1 & predicted==0)
 
  P=TP+FN
  N=TN+FP
  
  Sn=TP/P
  Sp=TN/N
  accuracy=(TP+TN/P+N)
  precision=TP/TP+FP
  recall=Sn
  
  KS=(TP/P)-(FP/N)
  F1=2*precision*recall/precision+recall
  cut_off_data=rbind(cut_off_data,c(cutoff,Sn,Sp,KS,precision,F1))
}

 cut_off_data=cut_off_data[-1,]
 
 ##visualisation
 library(ggplot2)
 ggplot(cut_off_data,aes(x=cutoff,y=KS))+geom_line()
 
 mycut_off=cut_off_data$cutoff[which.max(cut_off_data$KS)]
 
 
 ##submission
 store= predict(log_fit_final,newdata =test,type='response')
 
 store_1= predict(log_fit_final,newdata =train,type='response')
 
 df=data.frame(store,row.names=NULL)
## to find hard classes
 test.predicted=as.numeric(store>mycut_off)
 TEST_PRED=as.numeric(store_1>mycut_off)
 library(MASS)
 write.csv(df,"C:/Users/HP/Downloads/FOR PROJECT SUBMISSION/RETAIL/NEW_SUB.csv")

  ## FINDING AUC
 
 library(pROC)
 
 auc_score=auc(roc(real, TEST_PRED))
 
 pROC::auc(pROC::roc(real, TEST_PRED))

 Area under the curve = 0.843
 
 