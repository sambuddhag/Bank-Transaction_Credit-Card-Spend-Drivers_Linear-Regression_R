########## Linear Regression Case Study #########

setwd("C:\\Users\\sambuddhag\\Desktop\\ANALYTIXLABS\\R Software Installation\\R course\\Linear-Regression Case Study")
getwd()

# Importing the data file

install.packages("readxl")
library(readxl)

LR <- read_xlsx('Linear Regression Case Study.xlsx')


# Understanding the data

str(LR)
names(LR)
View(LR)
head(LR)
tail(LR)
dim(LR)
nrow(LR)
ncol(LR)
summary(LR)

# Creating dependent variable

LR$total_spend <- LR$cardspent + LR$card2spent

# Removing variables

LR1 <- subset(LR,select = -c(cardspent,card2spent,custid,birthmonth,carditems,card2items))
names(LR1)                                  

# Descriptive statistics

mystats=function(x){
  if(class(x)=="numeric"){
    Var_Type=class(x)
    n<-length(x)
    nmiss<-sum(is.na(x))
    mean<-mean(x,na.rm=T)
    std<-sd(x,na.rm=T)
    var<-var(x,na.rm=T)
    min<-min(x,na.rm=T)
    p1<-quantile(x,0.01,na.rm=T)
    p5<-quantile(x,0.05,na.rm=T)
    p10<-quantile(x,0.1,na.rm=T)
    q1<-quantile(x,0.25,na.rm=T)
    q2<-quantile(x,0.5,na.rm=T)
    q3<-quantile(x,0.75,na.rm=T)
    p90<-quantile(x,0.9,na.rm=T)
    p95<-quantile(x,0.95,na.rm=T)
    p99<-quantile(x,0.99,na.rm=T)
    max<-max(x,na.rm=T)
    UC1=mean(x,na.rm=T)+3*sd(x,na.rm=T)
    LC1=mean(x,na.rm=T)-3*sd(x,na.rm=T)
    UC2=quantile(x,0.95,na.rm=T)
    LC2=quantile(x,0.05,na.rm=T)
    iqr=IQR(x,na.rm=T)
    UC3=q3+1.5*iqr
    LC3=q1-1.5*iqr
    ot1<-max>UC1 | min<LC1 
    ot2<-max>UC2 | min<LC2 
    ot3<-max>UC3 | min<LC3
    return(c(Var_Type=Var_Type, n=n,nmiss=nmiss,mean=mean,std=std,var=var,min=min,p1=p1,p5=p5,p10=p10,q1=q1,q2=q2,q3=q3,p90=p90,p95=p95,p99=p99,max=max,ot_m1=ot1,ot_m2=ot2,ot_m2=ot3))
  }
  else{
    Var_Type=class(x)
    n<-length(x)
    nmiss<-sum(is.na(x))
    fre<-table(x)
    prop<-prop.table(table(x))
    x[is.na(x)]<-x[which.max(prop.table(table(x)))]
    
    return(c(Var_Type=Var_Type, n=n,nmiss=nmiss,freq=fre,proportion=prop))
  }
}

diag_stats<-  t(data.frame(apply(LR1,2,FUN = mystats)))

write.csv(diag_stats,"C:/Users/sambuddhag/Desktop/ANALYTIXLABS/R Software Installation/R course/Linear-Regression Case Study/DiagStat.csv")


#Vector of numerical variables

num_var= sapply(LR1,is.numeric)
Other_var= !sapply(LR1,is.numeric)

#Applying above defined function on numerical variables

my_num_data<-t(data.frame(apply(LR1[num_var], 2, mystats)))

my_cat_data<-data.frame(t(apply(LR1[Other_var], 2, mystats)))

View(my_num_data)

write.csv(my_num_data, file = "C:/Users/sambuddhag/Desktop/ANALYTIXLABS/R Software Installation/R course/Linear-Regression Case Study/my_num_data.csv")


# Missing Value Treatment

LR1[,num_var] <- apply(data.frame(LR1[,num_var]), 2, function(x){x <- replace(x, is.na(x), mean(x, na.rm=TRUE))})
LR1[,Other_var] <- apply(data.frame(LR1[,Other_var]), 2, function(x){x <- replace(x, is.na(x), which.max(prop.table(table(x))))})


# Outlier capping with p95 and p5

numeric_vars = names(LR1)[sapply(LR1, FUN=is.numeric)]

outlier_treat <- function(x){
  UC1 = quantile(x, p=0.95,na.rm=T)
  LC1 = quantile(x, p=0.05,na.rm=T)
  x[x>UC1]=UC1
  x[x<LC1]=LC1
  
  return(x)
}

mydata_num_vars = data.frame(apply(LR1[numeric_vars], 2, FUN=outlier_treat))


# Distribution of the Dependent Variable


hist(LR1$total_spend)


LR1$ln_total_spend<-log(LR1$total_spend)
hist(LR1$ln_total_spend)


# Correlation matrix

corrm<- cor(LR1[,numeric_vars])
View(corrm)

write.csv(corrm, file = "corrm1.csv")     


# Variable reduction using anova test (categorical variables)

fit <- lm(formula = total_spend ~ region
          +townsize
          +gender
          +agecat
          +edcat
          +jobcat
          +union
          +employ
          +empcat
          +retire
          +inccat
          +default
          +jobsat
          +marital
          +spousedcat
          +homeown
          +hometype
          +address
          +addresscat
          +cars
          +carown
          +cartype
          +carcatvalue
          +carbought
          +carbuy
          +commute
          +commutecat
          +commutecar
          +commutemotorcycle
          +commutecarpool
          +commutebus
          +commuterail
          +commutepublic
          +commutebike
          +commutewalk
          +commutenonmotor
          +telecommute
          +reason
          +polview
          +polparty
          +polcontrib
          +vote
          +card
          +cardtype
          +cardbenefit
          +cardfee
          +cardtenure
          +cardtenurecat
          +card2
          +card2type
          +card2benefit
          +card2fee
          +card2tenure
          +card2tenurecat
          +active
          +bfast
          +churn
          +tollfree
          +equip
          +callcard
          +wireless
          +multline
          +voice
          +pager
          +internet
          +callid
          +callwait
          +forward
          +confer
          +ebill
          +owntv
          +ownvcr
          +owndvd
          +owncd
          +ownpda
          +ownpc
          +ownipod
          +owngame
          +ownfax
          +news
          +response_01
          +response_02
          +response_03, data = LR1)
b <- anova(fit)
View(b)


# step wise regression

fitt <- step(lm(ln_total_spend ~ region
                +townsize
                +gender
                +agecat
                +edcat
                +employ
                +empcat
                +retire
                +inccat
                +hometype
                +commutebike
                +card
                +card2
                +voice
                +internet
                +age
                +ed
                +income
                +lninc
                +debtinc
                +creddebt
                +lncreddebt
                +othdebt
                +lnothdebt
                +spoused
                +reside
                +pets
                +pets_dogs
                +pets_birds
                +pets_reptiles
                +pets_small
                +pets_saltfish
                +pets_freshfish
                +carvalue
                +tenure
                +longmon
                +longten
                +lnlongmon
                +lnlongten
                +tollmon
                +lntollmon
                +tollten
                +lntollten
                +equipmon
                +lnequipmon
                +equipten
                +lnequipten
                +cardmon
                +lncardmon
                +cardten
                +lncardten
                +wiremon
                +lnwiremon
                +wireten
                +lnwireten
                +hourstv
                +commutetime
                , data=LR1), direction = "both")

summary(fitt)


# Converting categorical Variables into factors 

LR1$gender1 <- as.factor(LR1$gender)
LR1$edcat1 <- as.factor(LR1$edcat)
LR1$hometype1 <- as.factor(LR1$hometype)
LR1$card1 <- as.factor(LR1$card)
LR1$card2_1 <- as.factor(LR1$card2)
LR1$voice_1 <- as.factor(LR1$voice)
LR1$internet_1 <- as.factor(LR1$internet)


# Splitting data into Training, Validaton and Testing Dataset

train_ind <- sample(1:nrow(LR1), size = floor(0.70 * nrow(LR1)))

training<-LR1[train_ind,]
testing<-LR1[-train_ind,]


# Building Models for training dataset

fit <- lm(ln_total_spend ~ gender1 + edcat1 + hometype1 + card1 + card2_1 + voice_1 + internet_1 + 
            age + lninc + pets_reptiles + carvalue + cardmon + cardten + 
            lnwiremon, data = training)

summary(fit) 

install.packages("MASS")
library(MASS)

step3<- stepAIC(fit,direction="both")

ls(step3)
step3$anova


# Calculating cooks'd for influential observation

training$Cd <- cooks.distance(fit)
training1 <- subset(training, Cd< (4/3500))

fit3<-lm(ln_total_spend ~ gender1 + edcat1 + card1 + card2_1 + age + 
           lninc + carvalue + cardmon, data = training1)


summary(fit3)
step4<- stepAIC(fit3,direction="both")

ls(step4)
step4$anova


# Multicollinearity check (using VIF)

install.packages("car")
library(car)

vif(fit3)


# Final Model #

training1$Cd <- cooks.distance(fit3)
training2 <- subset(training1, Cd< (4/3335))

fit4<-lm(ln_total_spend ~ gender1 + edcat1 + card1 + card2_1 + age + 
           lninc + carvalue, data = training2) 

summary(fit4)


# Scores using predict function

t1<-cbind(training2, pred_spent = exp(predict(fit4)))
names(t1)
t1<- transform(t1, APE = abs(pred_spent - total_spend)/total_spend)
mean(t1$APE)
View(t1)


t2<-cbind(testing, pred_spent=exp(predict(fit4,testing)))
t2<- transform(t2, APE = abs(pred_spent - total_spend)/total_spend)
mean(t2$APE)
View(t2)

# Decile Analysis Reports (training)

decLocations <- quantile(t1$pred_spent, probs = seq(0.1,0.9,by=0.1))


t1$decile <- findInterval(t1$pred_spent,c(-Inf,decLocations, Inf))


install.packages("sqldf")
library(sqldf)

t1_DA <- sqldf("select decile, count(decile) as count, avg(pred_spent) as avg_pre_spent,   
               avg(total_spend) as avg_actual_spend
               from t1
               group by decile
               order by decile desc")


View(t1_DA)
write.csv(t1_DA, file = "C:/Users/sambuddhag/Desktop/ANALYTIXLABS/R Software Installation/R course/Linear-Regression Case Study/t1_DA.csv")


#Decile Analysis Reports (testing)

decLocations <- quantile(t2$pred_spent, probs = seq(0.1,0.9,by=0.1))


t2$decile <- findInterval(t2$pred_spent,c(-Inf,decLocations, Inf))


t2_DA <- sqldf("select decile, count(decile) as count, avg(pred_spent) as avg_pre_spent,   
               avg(total_spend) as avg_Actual_spend
               from t2
               group by decile
               order by decile desc")


View(t2_DA)

write.csv(t2_DA, file = "C:/Users/sambuddhag/Desktop/ANALYTIXLABS/R Software Installation/R course/Linear-Regression Case Study/t2_DA.csv")


########## END ###########



