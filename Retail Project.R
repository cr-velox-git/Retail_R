{
  library(dplyr)
  library(visdat)
  library(tidyr)
  library(car)
  library(stringr)
  library(pROC)
  setwd("C:\\Users\\Admin\\Desktop\\retail")
  
  
  ld_train = read.csv("store_train.csv",stringsAsFactors = F)
  ld_test  =read.csv("store_test.csv",stringsAsFactors = F)
  
  
  ld_test$store=NA
  
  ld_train$data = "train"
  ld_test$data = "test"
  
  ld_all = rbind(ld_train,ld_test)
  
  
  sales_vector = c("sales0","sales1","sales2","sales3","sales4")
  
  for(i in sales_vector){
    print(sum(is.na(ld_all[,i])))
  }
  
  
  ld_all$sales_mean = rowMeans(subset(ld_all, select = c("sales0","sales1","sales2","sales3","sales4")), na.rm = TRUE)
  
  ld_all = ld_all %>% 
    select(-sales_vector)
  
  table(ld_all$store_Type)
  
  CreateDummies=function(data,var,freq_cutoff=0){
    t=table(data[,var])
    t=t[t>freq_cutoff]
    t=sort(t)
    categories=names(t)[-1]
    
    for( cat in categories){
      name=paste(var,cat,sep="_")
      name=gsub(" ","",name)
      name=gsub("-","_",name)
      name=gsub("\\?","Q",name)
      name=gsub("<","LT_",name)
      name=gsub("\\+","",name)
      name=gsub("\\/","_",name)
      name=gsub(">","GT_",name)
      name=gsub("=","EQ_",name)
      name=gsub(",","",name)
      data[,name]=as.numeric(data[,var]==cat)
    }
    
    data[,var]=NULL
    return(data)
  }
}

#Data Preparation
{
  ld_all = CreateDummies(ld_all,"store_Type",10)
  
  ld_all$storecode = substr(ld_all$storecode, start = 1, stop = 5)
  
  ld_all = CreateDummies(ld_all,"storecode",50)
  
  ld_all$Id=NULL
  ld_all$State=NULL
  ld_all$countyname=NULL
  ld_all$countytownname=NULL
  ld_all$country=NULL
  ld_all$Areaname=NULL
  
  ld_all=CreateDummies(ld_all,"state_alpha",0)
}

#vis_dat(ld_all)
#separation of training data
{
  lgr_final_test = lgr_test=ld_all %>% filter(data=='test') %>% select(-data)
  
  lgr_train=ld_all %>% filter(data=='train') %>% select(-data)
  
  lgr_test=ld_all %>% filter(data=='test') %>% select (-data,-store)
  
  
  set.seed(2)
  s=sample(1:nrow(lgr_train),0.8*nrow(lgr_train))
  lgr_train1=lgr_train[s,] ## 80% Train data
  lgr_train2=lgr_train[-s,] ## 20% Train Test Data
}

#General Linear Model
{
  ld.glm=glm(store~,data=lgr_train1)
  val.IR=predict(ld.glm, newdata = lgr_train2)
  val.IR
}


for_vif=lm(store~.-state_alpha_GU,data = lgr_train1)

sort(vif(for_vif), decreasing = T)[1:3]


log_fit = glm(store~.-state_alpha_GU, data = lgr_train1)

log_fit = step(log_fit)
formula(log_fit)

log_fit= glm(store~ storecode_NCNTY + state_alpha_HI + state_alpha_NC,data = lgr_train1, family ='binomial')

'summary(log_fit)'


val.score=predict(log_fit,newdata = lgr_train2, type='response')
'val.score'

auc(roc(lgr_train2$store,val.score))


log_fit_final= glm(store~ storecode_NCNTY + state_alpha_HI + state_alpha_NC,data = lgr_train, family ='binomial')
'summary(log_fit_final)'

test_prob_score = predict(log_fit_final, newdata = lgr_test, type='response',row.names= F )
'test_prob_score'
write.csv(test_prob_score,"Rajesh_Khan_P2_part2.csv", row.names = F)

