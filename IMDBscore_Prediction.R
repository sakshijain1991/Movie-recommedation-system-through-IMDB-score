# the code below has used for all the cleaning and data manipulation binning 
# and all .U will find comments where the actual data modelling starts

library(mice)
library(randomForest)
library(ROCR)
library(DMwR)
library(ROSE)
library(class)
library(proxy)
library(caret)
library(e1071)

#Data cleaning and manipulation

file_data<-read.csv("imdb_data.csv",stringsAsFactors = FALSE)
trim1<-file_data[,-c(2,7,10,11,15,17,18,27,28,38:43)]

# imputing missing valuesin gross, budget using mice
gross_tmp<-mice(trim1,m=5,method = "rf")
impute1<-complete(gross_tmp,1)
impute2<-complete(gross_tmp,2)
impute3<-complete(gross_tmp,3)
impute4<-complete(gross_tmp,4)
impute5<-complete(gross_tmp,5)

trim_data<-impute5

buc_val<-matrix(NA,nrow = nrow(trim_data),ncol= 1)
fin_ratio<-matrix(NA,nrow = nrow(trim_data),ncol= 1)

for(i in seq_len(nrow(trim_data))){
        fin_ratio[i,1]<-trim_data[i,7]/trim_data[i,16]
        if(trim_data[i,19]<=5.0){
                buc_val[i,1]="avoid"
        }else if(trim_data[i,19]>5.0 && trim_data[i,19]<=7){
                buc_val[i,1]="one time"
        }else if(trim_data[i,19]>7.0 && trim_data[i,19]<=8.5){
                buc_val[i,1]="good"
        }else if(trim_data[i,19]>8.5){
                buc_val[i,1]="must _watch"
        }
}
trim_data$imdB_targ<-buc_val[,1]
trim_data$fin_ratio<-fin_ratio[,1]

#function to bin in 3 buckets

three_bins<-function(cut1,cut2,col_num){
        
        i<-NA
        act1<-matrix(NA,nrow=nrow(trim_data),ncol=1)
        count_1 =0
        count_2 =0
        count_3 =0
        likes=0
        
        for(i in seq_len(nrow(trim_data))){
                likes<-trim_data[i,col_num]
                if(likes<cut1){
                        count_1 = count_1+1
                        act1[i,1]= 1
                }else if(likes>=cut1 && likes<cut2){
                        count_2 = count_2+1
                        act1[i,1]=2
                }else if(likes>=cut2){
                        count_3 = count_3+1
                        act1[i,1]= 3
                }
                
        }
        print(c(count_1,count_2,count_3))
        return(as.factor(act1))
        
}


# Function to bin data into 4 buckets
act_rating<- function(cut1,cut2,cut3,col_num=8)
{
        i<-NA
        act1<-matrix(NA,nrow=nrow(trim_data),ncol=1)
        count_1 =0
        count_2 =0
        count_3 =0
        count_4 =0
        likes=0
        
        for(i in seq_len(nrow(trim_data))){
                likes<-trim_data[i,col_num]
                if(likes<cut1){
                        count_1 = count_1+1
                        act1[i,1]= 1
                }else if(likes>=cut1 && likes<cut2){
                        count_2 = count_2+1
                        act1[i,1]=2
                }else if(likes>=cut2 && likes<cut3){
                        count_3 = count_3+1
                        act1[i,1]=3
                }else if(likes>=cut3){
                        count_4 = count_4+1
                        act1[i,1]= 4
                }
                
        }
        print(c(count_1,count_2,count_3,count_4))
        return(as.factor(act1))
}

#generate rating for Actor1
act1_rate<-act_rating(627,1000,11000,6)
trim_data$act1_rating<-act1_rate

#generate rating for Actor2
act2_rate<-act_rating(297,617,938,18)
trim_data$act2_rating<-act2_rate

#generate rating for Actor3
act3_rate<-act_rating(135,375,638,5)
trim_data$act3_rating<-act3_rate

#Generate bins for "cast_total_facebook_likes"
cast_rating<-act_rating(1467,3191,14447,10)
trim_data$cast_total_likes<-cast_rating


#generate bins for duration
length<-act_rating(94,104,118,3)
trim_data$length<-length

#Generate bin for title year
timeline<-act_rating(1999,2005,2011,17)
trim_data$timeline<-timeline

#genrate bins for "num_voted_users"
total_votes<-act_rating(9585,36253,99353,9)
trim_data$total_votes<-total_votes


#generate bins for "num_user_for_reviews" 
user_reviews<-act_rating(68,160,333,12)
trim_data$no_user_reviews<-user_reviews

#generate bins for "num_critic_for_reviews"
critics_reviews<-act_rating(54,113,198,2)
trim_data$critic_reviews<-critics_reviews


#generate 3 bins for ""movie_facebook_likes"
movie_fb<-three_bins(175,4000,20)
trim_data$movie_fb_likes<-movie_fb

#generate 4 bins for fin_ratio
profit<-act_rating(1,10,100,30)
trim_data$profit<-profit






#the code below uses the latest file developed after all the data manipulations
#i.e. the last file forwarded by Sakshi


trim_data<-read.csv("IMDB_THR.csv")
trim_data<-trim_data[-21]

#bin the IMDB Score into 3 baskets
buc_val<-matrix(NA,nrow = nrow(trim_data),ncol= 1)
for(i in seq_len(nrow(trim_data))){
        if(trim_data[i,20]<=5.0){
                buc_val[i,1]="one time"
        }else if(trim_data[i,20]>5.0 && trim_data[i,20]<=7.5){
                buc_val[i,1]="good"
        }else if(trim_data[i,20]>7.5 ){
                buc_val[i,1]="must_watch"
        }
}
trim_data$imdB_targ<-as.factor(buc_val[,1])
final_data<-trim_data[-c(1,3,8,20)]

#split the data 
smp_size<-floor(0.75*nrow(final_data))

set.seed("098")
smp_vec<-sample(seq_len(nrow(final_data)),smp_size)

train_data<-final_data[smp_vec,]
test_data<-final_data[-smp_vec,]
table(train_data$imdB_targ)
table(test_data$imdB_targ)

#generate formula
names<- names(final_data[-17])
var_names<-paste(names,collapse = "+")
formula<-as.formula(paste("imdB_targ",var_names,sep = "~"))

#try smoting (icreases one time which is least)
set.seed("012")
smote_init<-SMOTE(formula,train_data,perc.over = 100, perc.under = 300)
table(smote_init$imdB_targ)

#second smote to increase must_watch which becomes least.
set.seed("0123")
s<-SMOTE(formula,smote_init,perc.over = 300, perc.under = 300)
table(s$imdB_targ)

#use random forest
rf<-randomForest(formula,data = train_data, ntree = 500,importance = TRUE,
                  norm.votes= FALSE)

rf<-randomForest(formula,data = s, ntree = 500,importance = TRUE,
                norm.votes= FALSE)
#get predictions
rf_predict<-predict(rf,test_data)
rf_predict_prob<-predict(rf,test_data,type = 'prob')

#generate ocnfusion matrix
rf_cm<-confusionMatrix(rf_predict,test_data$imdB_targ)
table(test_data$imdB_targ,rf_predict)

varImpPlot(rf,
           sort = T,
           main="Variable Importance",
           n.var=10)

#rownames(rf_predict_prob)<-1:nrow(rf_predict_prob)
#plotting the ROC
lvls = levels(test_data$imdB_targ)

aucs = c()
plot(x=NA, y=NA, xlim=c(0,1), ylim=c(0,1),
     ylab='True Positive Rate',
     xlab='False Positive Rate',
     bty='n')

for (type.id in 2) {
        type = as.factor(train_data$imdB_targ == lvls[type.id])
        print(lvls[type.id])
        
        #score = nbprediction$posterior[, 'TRUE']
        score = rf_predict_prob[,type.id]
        actual.class = test_data$imdB_targ == lvls[type.id]
        
        pred = prediction(score, actual.class)
        nbperf = performance(pred, "tpr", "fpr")
        
        roc.x = unlist(nbperf@x.values)
        roc.y = unlist(nbperf@y.values)
        lines(roc.y ~ roc.x, col=type.id+1, lwd=2)
      
        nbauc = performance(pred, "auc")
        nbauc = unlist(slot(nbauc, "y.values"))
        aucs[type.id] = nbauc
}

lines(x=c(0,1), c(0,1))
mean(aucs)


#use multiple random forests
#create 5 sets of smoted data
set.seed("1")
smote1<-SMOTE(formula,smote_init,perc.over = 300, perc.under =300)
table(smote1$imdB_targ)
set.seed("2")
smote2<-SMOTE(formula,smote_init,perc.over = 300, perc.under = 300)
table(smote2$imdB_targ)
set.seed("3")
smote3<-SMOTE(formula,smote_init,perc.over = 300, perc.under = 300)
table(smote3$imdB_targ)
set.seed("4")
smote4<-SMOTE(formula,smote_init,perc.over = 300, perc.under = 300)
table(smote4$imdB_targ)
set.seed("5")
smote5<-SMOTE(formula,smote_init,perc.over = 300, perc.under = 300)
table(smote5$imdB_targ)

#multiple random forest
rf1<-randomForest(formula,data = smote1, ntree = 500,importance = TRUE,
                  norm.votes= FALSE)
rf2<-randomForest(formula,data = smote2, ntree = 500,importance = TRUE,
                  norm.votes= FALSE)
rf3<-randomForest(formula,data = smote3, ntree = 500,importance = TRUE,
                  norm.votes= FALSE)
rf4<-randomForest(formula,data = smote4, ntree = 500,importance = TRUE,
                  norm.votes= FALSE)
rf5<-randomForest(formula,data = smote5, ntree = 500,importance = TRUE,
                  norm.votes= FALSE)
#rf <- do.call(combine,list(rf1,rf2,rf3,rf4,rf5))
rf_all <- combine(rf1,rf2,rf3,rf4,rf5)
rf_predict_com<-predict(rf_all,test_data)
confusion<-confusionMatrix(rf_predict_com,test_data$imdB_targ)
varImpPlot(rf_all,
           sort = T,
           main="Variable Importance",
           n.var=17)


#use boosting
library(maboost)
gdis<-maboost(formula,data=train_data,iter=50,nu=2
              ,breg="l2", type="sparse",bag.frac=1,random.feature=FALSE
              ,random.cost=FALSE, C50tree=FALSE, maxdepth=6,verbose=TRUE)
##to see the average zeros in the weighting vectors over the 40 rounds of boosting
print(mean(gdis$model$num_zero))
##prediction
pred.gdis= predict(gdis,test_data,type="class");
##variable selection
varplot.maboost(gdis)
table(test_data$imdB_targ,pred.gdis)
confusionMatrix(pred.gdis,test_data$imdB_targ)


#use naive bayes
nb_model<-naiveBayes(formula = formula,data = train_data)
nb_model<-naiveBayes(formula = formula,data = s)
predict_nb<- predict(nb_model,test_data)
predict_nb_prob<- predict(nb_model,test_data,type ='raw')
table(test_data$imdB_targ,predict_nb)
nb_cm<-confusionMatrix(predict_nb,test_data$imdB_targ)