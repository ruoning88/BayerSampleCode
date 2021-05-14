
## sales_regroup_lx 为原始数据， 
## 这一步的目的是为了合并医院等级
library(dplyr)
modeldata <- sales_regroup_lx %>%
  mutate(HospitalTypeName = ifelse(HospitalTypeName == '综合医院','综合医院','其他医院')) %>%
  mutate(id=row_number())
ref_pc <- modeldata %>%
  select(id,ProvinceName,CityName)
ref_hospname <- modeldata[,c(63,1:7)]

## 这一步的目的是为了将数据转化成模型能接受的格式
g <- paste(colnames(modeldata)[c(8,12:56,59:61,63)], collapse = "` + `")
c <- paste('Concentrate', " ~ ", '`',g, '`', sep = "")
formu <- as.formula(c)

m.y <- modeldata$Concentrate
modeldata <-  model.matrix(formu, modeldata)[,-1]
modeldata <- as.data.frame(modeldata)
modeldata<- cbind(m.y,modeldata)
colnames(modeldata)[1] <- 'Concentrate'
modeldata <- modeldata %>%
  left_join(ref_pc,by='id') 



## 这一步的目的是把省份转化成连续变量
library(sampling)
conflict_prefer("slice", "dplyr")
set.seed(123)
n=c(round((nrow(modeldata)-sum(as.numeric(modeldata$Concentrate),na.rm=TRUE))*0.75),round(sum(as.numeric(modeldata$Concentrate),na.rm=TRUE)*0.75))
sub_train=strata(modeldata,stratanames=c("Concentrate"),size=n,method="srswor")


model.train <- modeldata[sub_train$ID_unit,]
model.test <- modeldata[-sub_train$ID_unit,]
fifunc <- data.frame(summary(as.factor(model.train$ProvinceName)))
k_func <- min(fifunc$summary.as.factor.model.train.ProvinceName..)
f_func <- quantile(fifunc$summary.as.factor.model.train.ProvinceName..,0.1)
TBS <- function(ni,k_func,f_func) {
  fi_ni <- 1/(exp(-((ni-k_func)/f_func))+1)
  return(fi_ni)
}
sampleratio = sum(model.train$Concentrate=='1',na.rm=TRUE)/nrow(model.train)
Targ_stat <- model.train %>%
  group_by(ProvinceName) %>%
  mutate(Concentrate= as.numeric(Concentrate)) %>%
  summarize(ni = n(), 
            niY = sum(Concentrate,na.rm=TRUE)) %>%
  ungroup() %>%
  mutate(ratio= niY/ni,
         fiv = TBS(ni,k_func,f_func)) %>%
  mutate(first = ratio*fiv,
         sec = (1-fiv)*sampleratio) %>%
  mutate(result = first+sec) %>%
  select(ProvinceName, result)


model.train <- model.train %>%
  left_join(Targ_stat,by='ProvinceName') %>%
  rename(ProvinceScore=result)

model.test <- model.test %>%
  left_join(Targ_stat,by='ProvinceName') %>%
  rename(ProvinceScore=result)


## 建立模型,计算倾向值
reduce_vars <- c(
  "Potential_Value","count2018","count2019",                 
  "LecFee_Department_2019","SalesAmount_2018","ProvinceScore",             
  "MeetingCost_Department_2019","LecFee_Department_2018","MeetingCost_Department_2018",
 "speaks_Department_2019","speaks_RNC_2019","lecturer_Department_2019",  
  "MeetingCost_RNC_2019","LecFee_RNC_2019","speaks_Department_2018",     
  "CityLevel","lecturer_RNC_2019","MeetingCost_RNC_2018",      
  "LecFee_RNC_2018","lecturer_Department_2018","RegionSouth",               
  "MeetingCost_Oth_2018","lecturer_RNC_2018","speaks_RNC_2018",           
  "HospitalTypeName综合医院","LecFee_Oth_2019","RegionWest",                 
  "LecFee_Oth_2018","MeetingCost_Oth_2019","speaks_Oth_2018" 
)

g <- paste(reduce_vars, collapse = "` + `")
c <- paste('Concentrate', " ~ ", '`',g, '`', sep = "")
formu_reduce <- as.formula(c)

modeldata_prop <- rbind(model.train,model.test)
modeldata_prop <- modeldata_prop %>%
  mutate(Concentrate=as.numeric(Concentrate))

library(twang)
set.seed(127)
ps.lindner <- ps(formu_reduce,
                 data = modeldata_prop ,
                 nrounds = 1000,
                 eta = 0.02,
                 max_depth = 8,
                 gamma = 0.1,
                 colsample_bytree = 0.45,
                 min_child_weight = 5,
                 subsample = 0.5,
                 keep.data=TRUE,
                 version='xgboost',
                 estimand = "ATE",
                 verbose = FALSE)
by.balance <- bal.table(ps.lindner)
by.balance 

summary(ps.lindner)
plot(ps.lindner, plots=2,subset=2)
plot(ps.lindner, plots=3,subset=2)
plot(ps.lindner, plots=4,subset=2)
plot(ps.lindner, plots=5,subset=2)

gbm_prop <- ps.lindner$ps$ks.mean.ATE



## 做匹配--首先运用KNN算法，其次运用Genetic算法
library(MatchIt)

greedyMatching <- matchit(formu_reduce, distance=gbm_prop, data=modeldata_prop,
                          exact=c('ProvinceName'),
                          method='nearest', replace=FALSE, ratio=1, estmand='ATE')

summary(match.data$subclass)
match.data = match.data(greedyMatching) %>%
  as.matrix() %>%
  as.data.frame() %>%
  mutate(subclass=as.numeric(as.character(subclass))) %>%
  select(-weights)



library(rgenoud)
library(Matching)
genericMatching <- matchit(formu_reduce, distance=gbm_prop, data=modeldata_prop,
                           exact=c('ProvinceName'), pop.size=1000,
                           fit.func='pvals', 
                           method='genetic', replace=FALSE, ratio=1, estmand='ATE',
                           verbose=TRUE
)

genetic.data = match.data(genericMatching) %>%
  as.matrix() %>%
  as.data.frame() %>%
  mutate(subclass=as.numeric(as.character(subclass))) %>%
  select(-weights)

plot(greedyMatching, type='qq')
plot(greedyMatching, type='hist')
plot(genericMatching, type='qq')
plot(genericMatching, type='hist')

