
datacc<-read.csv("CC-GENERAL.csv")

summary(datacc)
nmiss=function(x){  #function to find number of missing values in each variable of the dataset 
  nmiss=sum(is.na(x))
  return(nmiss=nmiss)
}

n_missing=apply(datacc,2,nmiss)
n_missing
nrow(datacc)
max(n_missing)/(nrow(datacc))*100 #Find percentage of missing column value in dataset


#Derive new KPI's
datacc$MONTHLY_AVERAGE_PURCHASE=datacc$PURCHASES/datacc$TENURE
datacc$MONTHLY_AVG_ONEOFF_PURCHASE=datacc$ONEOFF_PURCHASES/datacc$TENURE
datacc$MONTHLY_AVg_INSTALL_PURCHASE=datacc$INSTALLMENTS_PURCHASES/datacc$TENURE
datacc$MONTHLY_CASH_ADVANCE_AMOUNT=datacc$CASH_ADVANCE/datacc$TENURE
datacc$LIMIT_USAGE=datacc$BALANCE/datacc$CREDIT_LIMIT
datacc$MIN_PAYMENT_RATIO=datacc$PAYMENTS/datacc$MINIMUM_PAYMENTS
#BALANCE_PAYMENT_RATIO=payments/balance
datacc$PERC_CASH_ADVANCE=datacc$CASH_ADVANCE/(datacc$CASH_ADVANCE+datacc$PURCHASES)
datacc$PERC_ONEOFF_PURCHASES=datacc$ONEOFF_PURCHASES/(datacc$CASH_ADVANCE+datacc$PURCHASES)
datacc$PERC_INSTALLMENT_PURCHASES=datacc$INSTALLMENTS_PURCHASES/(datacc$CASH_ADVANCE+datacc$PURCHASES)

n_missing=apply(datacc,2,nmiss)
n_missing

datacc2=na.omit(datacc) #remove missing values from the dataset
(1-(nrow(datacc2)/nrow(datacc)))*100 #check %age of data removed due to missing values

n_missing=apply(datacc2,2,nmiss)  #Check if any missing entries remain
n_missing

str(datacc2)
#---Factor analysis-----------------------------------
datacc2$CUST_ID=NULL
corrm= cor(datacc2)                                 ### CORRELATION MATRIX

View(corrm)
require(psych)
require(GPArotation)

### DECIDING NUMBER OF FACTORS USING SCREE PLOT & KAISER TEST(NUMBER OF EIGEN VALUES OVER 1)

scree(corrm, factors=T, pc=T, main="scree plot", hline=NULL, add=FALSE) ### SCREE PLOT

eigen(corrm)$values                                                     ### EIGEN VALUES

require(dplyr)
eigen_values <- mutate(data.frame(eigen(corrm)$values)
                       ,cum_sum_eigen=cumsum(eigen.corrm..values)
                       , pct_var=eigen.corrm..values/sum(eigen.corrm..values)
                       , cum_pct_var=cum_sum_eigen/sum(eigen.corrm..values))  # CALCULATING VARIANCE, CUMULATIVE VARIANCE etc... 

View(eigen_values)
write.csv(eigen_values, "EigenValues.csv")  ### EXPORTING EIGEN VALUE SUMMARY

FA<-fa(r=corrm, 8, rotate="varimax", fm="ml")               ### CONDUCTING FACTOR ANALYSIS, 8 FACTORS
print(FA)                                                    ### PRINT THE RESULTS
FA_SORT<-fa.sort(FA)                                         ### SORTING THE LOADINGS
ls(FA_SORT)                                                  ### LISTING OUT THE OBJECTS

#FA_SORT$e.values                                            ### FINDING EIGEN VALUES FROM THE RESULTS
Loadings<-data.frame(FA_SORT$loadings[1:ncol(datacc2),])
Loadings
write.csv(Loadings,"loadings2.csv")

#----------------outlier treatment------------------------------
stats <- function(x) {
  n=nrow(x)
  a <- x[!is.na(x)]
  nmiss<-sum(is.na(x))
  m <- mean(a)
  std <- sd(a)
  min <- min(a)
  p1<-quantile(a,0.01)
  p5<-quantile(a,0.05)
  median<-quantile(a,0.5)
  p95<-quantile(a,0.95)
  p99<-quantile(a,0.99)
  max <- max(a)
  UC <- m+3*std
  LC <- m-3*std
  outlier_flag<- max>UC | min<LC
  return(c(total_obs=n,nmiss=nmiss, mean=m, stdev=std,min = min, p1=p1,p5=p5, median=median,p95=p95,p99=p99,max=max, UC=UC, LC=LC, outlier_flag=outlier_flag))
}
str(datacc2)
var_audit_report=data.frame(t(apply(datacc2,2,stats)))
var_audit_report
write.csv(var_audit_report,"data audit report.csv")

 #-----------------------function for outlier removal (at 99 pctl)--------

#Variables selected after factor analysis
vars=c("MONTHLY_AVG_ONEOFF_PURCHASE",
       "PURCHASES",
       "MONTHLY_AVERAGE_PURCHASE",
       "PURCHASES_TRX",
       "MONTHLY_CASH_ADVANCE_AMOUNT",
       "CASH_ADVANCE_TRX",
       "PERC_INSTALLMENT_PURCHASES",
       "MONTHLY_AVg_INSTALL_PURCHASE",
       "LIMIT_USAGE",
       "BALANCE",
       "PRC_FULL_PAYMENT",
       "MIN_PAYMENT_RATIO",
       "PERC_ONEOFF_PURCHASES",
       "PERC_CASH_ADVANCE",
       "TENURE")
datacc3=datacc2[vars]     #make new dataset for outlier treatment
n_col=ncol(datacc3)
n_col

#outlier removal for 99 percentile
for(i in seq(1,length(vars),by=1)){
  u=quantile(datacc3[vars[i]],probs=0.99,na.rm=TRUE) #calculating the 99th percentile
  s=sapply(datacc3[vars[i]],sd,na.rm=TRUE) #calculating the standard deviation
  uc=u+s
  v=seq(1,1,length.out = NROW(datacc3))
  v[datacc3[vars[i]]>uc]=0
  datacc3[n_col+i]=v
}
 
View(datacc3[16:30])


datacc3$out_flag=apply(datacc3[16:30],1,sum)
View(datacc3[31])

table(datacc3$out_flag)  #If out_flag=15, then only that observation is NOT an outlier
                        # If out_flag < 15, then that observation is an outlier 

(1-(8354/nrow(datacc3)))*100  #3.25% obervations are outliers

 
inputdata_final=datacc3[datacc3$out_flag==15,1:15]  #inputdata_final has only non-outliers (8354 observations)
                 View(datacc3["out_flag"])           #and only those columns that are required in kmeans analysis
datacc2$out_flag=datacc3$out_flag    
datacc2=datacc2[datacc2$out_flag==15,] #now datacc2 also has only non-outliers (8354 observations)
                                        # but it has all the columns (will be required to calculate an all_columns_profile)

names(datacc3)
View(inputdata_final)
names(inputdata_final)
n_missing=apply(inputdata_final,2,nmiss)
n_missing   #0 missing values

names(inputdata_final) #inputdata_final has the outlier treated data and also has no missing values

#--------------outlier treatment done------------------


inputdata_final_std<- scale(inputdata_final)                 #STANDARDISING THE DATA

#K-Means Cluster Analysis
km_clust_3 <- kmeans(inputdata_final_std,3)                  # 3-CLUSTER SOLUTIONS
km_clust_4 <- kmeans(inputdata_final_std,4)                  # 4-CLUSTER SOLUTION
km_clust_5 <- kmeans(inputdata_final_std,5)                  # 5-CLUSTER SOLUTION
km_clust_6 <- kmeans(inputdata_final_std,6)                  # 6-CLUSTER SOLUTION

cust_data_cluster<-cbind(inputdata_final, clust_3=km_clust_3$cluster, clust_4=km_clust_4$cluster, clust_5=km_clust_5$cluster,clust_6=km_clust_6$cluster)
View(cust_data_cluster)
###Profiling----------------

install.packages("tables")
require(tables)

tt<-tabular( factor(clust_3)+factor(clust_4)+factor(clust_5)
             +factor(clust_6)
             +1 ~ Heading()*mean*All(inputdata_final), data=cust_data_cluster)



datacc2<-cbind(datacc2, clust_3=km_clust_3$cluster, clust_4=km_clust_4$cluster, clust_5=km_clust_5$cluster,clust_6=km_clust_6$cluster)
vars2=c("BALANCE" ,"BALANCE_FREQUENCY", "PURCHASES"                       
        ,"ONEOFF_PURCHASES","INSTALLMENTS_PURCHASES","CASH_ADVANCE"                    
        ,"PURCHASES_FREQUENCY" ,"ONEOFF_PURCHASES_FREQUENCY" , "PURCHASES_INSTALLMENTS_FREQUENCY"
        ,"CASH_ADVANCE_FREQUENCY" ,"CASH_ADVANCE_TRX","PURCHASES_TRX"                   
        ,"CREDIT_LIMIT" , "PAYMENTS" ,"MINIMUM_PAYMENTS"                
        ,"PRC_FULL_PAYMENT" , "TENURE" , "MONTHLY_AVERAGE_PURCHASE"        
        ,"MONTHLY_AVG_ONEOFF_PURCHASE" ,"MONTHLY_AVg_INSTALL_PURCHASE" , "MONTHLY_CASH_ADVANCE_AMOUNT"     
        ,"LIMIT_USAGE" , "MIN_PAYMENT_RATIO"  ,  "PERC_CASH_ADVANCE"               
        ,"PERC_ONEOFF_PURCHASES","PERC_INSTALLMENT_PURCHASES" )
tt_f<-tabular( factor(clust_3)+factor(clust_4)+factor(clust_5)
             +factor(clust_6)
             +1 ~ Heading()*mean*All(datacc2[vars2]), data=datacc2)

tt_f1<-as.data.frame.matrix(tt_f)
tt_f1
rownames(tt_f1)<-c("KM3_1" ,"KM3_2" ,"KM3_3", "KM4_1" ,"KM4_2" ,"KM4_3", "KM4_4" ,"KM5_1" ,"KM5_2", "KM5_3" ,"KM5_4" ,"KM5_5", "KM6_1" ,"KM6_2" ,"KM6_3", "KM6_4" ,"KM6_5" ,"KM6_6", "ALL" )
colnames(tt_f1)<-c("BALANCE" ,"BALANCE_FREQUENCY", "PURCHASES"                       
                   ,"ONEOFF_PURCHASES","INSTALLMENTS_PURCHASES","CASH_ADVANCE"                    
                   ,"PURCHASES_FREQUENCY" ,"ONEOFF_PURCHASES_FREQUENCY" , "PURCHASES_INSTALLMENTS_FREQUENCY"
                   ,"CASH_ADVANCE_FREQUENCY" ,"CASH_ADVANCE_TRX","PURCHASES_TRX"                   
                   ,"CREDIT_LIMIT" , "PAYMENTS" ,"MINIMUM_PAYMENTS"                
                   ,"PRC_FULL_PAYMENT" , "TENURE" , "MONTHLY_AVERAGE_PURCHASE"        
                   ,"MONTHLY_AVG_ONEOFF_PURCHASE" ,"MONTHLY_AVg_INSTALL_PURCHASE" , "MONTHLY_CASH_ADVANCE_AMOUNT"     
                   ,"LIMIT_USAGE" , "MIN_PAYMENT_RATIO"  ,  "PERC_CASH_ADVANCE"               
                   ,"PERC_ONEOFF_PURCHASES","PERC_INSTALLMENT_PURCHASES" )

all_col_profiling=t(tt_f1)
all_col_profiling
write.csv(all_col_profiling, "profiling2.csv") 

#----------------------

tt<-tabular( factor(clust_3)+factor(clust_4)+factor(clust_5)
             +factor(clust_6)
             +1 ~ Heading()*mean*All(inputdata_final), data=cust_data_cluster)

tt1<-as.data.frame.matrix(tt)
tt1
names(cust_data_cluster)

rownames(tt1)<-c("KM3_1" ,"KM3_2" ,"KM3_3", "KM4_1" ,"KM4_2" ,"KM4_3", "KM4_4" ,"KM5_1" ,"KM5_2", "KM5_3" ,"KM5_4" ,"KM5_5", "KM6_1" ,"KM6_2" ,"KM6_3", "KM6_4" ,"KM6_5" ,"KM6_6", "ALL" )
colnames(tt1)<-c("MONTHLY_AVG_ONEOFF_PURCHASE" , "PURCHASES", "MONTHLY_AVERAGE_PURCHASE","PURCHASES_TRX" ,"MONTHLY_CASH_ADVANCE_AMOUNT"
                 ,"CASH_ADVANCE_TRX","PERC_INSTALLMENT_PURCHASES","MONTHLY_AVg_INSTALL_PURCHASE" ,"LIMIT_USAGE"                 
                 ,"BALANCE", "PRC_FULL_PAYMENT" ,  "MIN_PAYMENT_RATIO","PERC_ONEOFF_PURCHASES", 
                 "PERC_CASH_ADVANCE" , "TENURE")
cluster_profiling<-t(tt1)
cluster_profiling

distr=tabular(factor(clust_3)+factor(clust_4)+factor(clust_5)
        +factor(clust_6)~Heading(),data=cust_data_cluster)
distr

distr_of_clust=sapply(distr,function(x) x/nrow(cust_data_cluster)*100)
distr_of_clust=as.data.frame(distr_of_clust)
rownames(distr_of_clust)=c("KM3_1" ,"KM3_2" ,"KM3_3", "KM4_1" ,"KM4_2" ,"KM4_3", "KM4_4" ,"KM5_1" ,"KM5_2", "KM5_3" ,"KM5_4" ,"KM5_5", "KM6_1" ,"KM6_2" ,"KM6_3", "KM6_4" ,"KM6_5" ,"KM6_6")
distr_of_clust  #this has the percentage distribution of data in the various clusters of the solutions

write.csv(cluster_profiling, "profiling.csv") 


#combine the files profile and profile2. profile has only those variables that were used for kmeans.
#profile2 has all the variables.

