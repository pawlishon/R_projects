
final_data<-read.csv("final_data.csv",stringsAsFactors = FALSE)
library(dplyr)
#zmiana zmiennych na faktory
final_data<-final_data%>%mutate_at(c("Job_type","Marital_status","Home_status","Car_status","Credit_purpose","GEO_region","DefFlag"),as.factor)
# 2 zmienne maja przecinki zamiast kropek, poprawiamy to
final_data$Divorce_number_1000<-as.numeric(sub(",",".",final_data$Divorce_number_1000))
final_data$Average_income<-as.numeric(sub(",",".",final_data$Average_income))
#Podzia³ na zbiór treningowy i testowy
final_train<-final_data[which(!is.na(final_data$DefFlag)),]
final_test<-final_data[which(is.na(final_data$DefFlag)),]

#EKSLORACYJNA ANALIZA ZMIENNYCH
#Jedyna zmienna, ktorej wyrzucenie poprawia jakoœæ modelu to Car_status
ggplot(final_train, aes(x = Car_status, fill = DefFlag)) + geom_bar(position = "fill")+
  ylab("Frequency") +theme_bw()
#Wyrzucam Car_status
final_train<-final_train%>%select(-Car_status)

#KORELACJA ZMIENNYCH
dane_kateg<-final_data[,c(4,5,6,7,11)]
dane_nie_kateg<-final_data[,-c(4,5,6,7,11)]
macierz_korelacji<-as.data.frame(stats::cor(final_data[,c(2,3,8,9,10,12,13)]))#Mo¿na wrzuciæ wszystkie zmienne nie kateg
#ale to nie ma sensu, zmienne geo i behawioralne sa mocne skorelowane, bo jak ktos nie splaca, to raczej nie splaca i jak
#ktos nalezy do regionu to wszystkie wartosci z tego regionu sa mocno skorelowane
#Wprowadzam rozrzutnoœæ, bo Zarobki i wydatnosc sa mocno skorelowane
#final_train[,8]<-final_train$rozrzutnosc<-final_train$Monthly_Spendings/final_train$Monthly_Income
#final_train<-final_train[,-c(9,10)]
#Nie poprawia jakoœci modelu

#Podzia³ danych na kategorie 
app_data<-final_train[,c(2:12)]
behavioral_data<-final_train[,c(51:89)]
geo_data<-final_train[,c(13:50)]

#Polaczenie z h2o
library(h2o)
library(tidyverse)
Sys.setlocale("LC_MESSAGES", 'en_GB.UTF-8')
Sys.setenv(LANG = "en_US.UTF-8")
localH2O <- h2o.init(ip = "localhost", 
                                       port = 54321, 
                                       nthreads = -1, 
                                       min_mem_size = "8g") 

#PCA danych behawioralnych
h2o_beh<-as.h2o(behavioral_data,destination_frame = "beh")
pca_beh <- h2o.prcomp(training_frame = "beh", k = 32, transform = "STANDARDIZE")#zostawilem tylko te z odchyleniem standardowym powyzej 0,015
#Dla tego k model osi¹ga³ najlepszy wynik
short_beh<-h2o.predict(pca_beh,h2o_beh)
short_behavioral<-as.data.frame(short_beh)

#PCA danych geo
h2o_geo<-as.h2o(geo_data,destination_frame = "geo")
pca_geo <- h2o.prcomp(training_frame = "geo", k = 35, transform = "STANDARDIZE")#Najwyzsze Gini dla tego k
short_geo<-h2o.predict(pca_geo,h2o_geo)
short_geog<-as.data.frame(short_geo)
base::colnames(short_geog)<-c("PCG1","PCG2","PCG3","PCG4","PCG5","PCG6","PCG7","PCG8","PCG9","PCG10",
                              "PCG11","PCG12","PCG13","PCG14","PCG15","PCG16","PCG17","PCG18","PCG19","PCG20",
                              "PCG21","PCG22","PCG23","PCG24","PCG25","PCG26","PCG27","PCG28","PCG29","PCG30",
                              "PCG31","PCG32","PCG33","PCG34","PCG35")

final_train<-cbind(final_train[,1],app_data,short_geog,short_behavioral,final_data$DefFlag[which(!is.na(final_data$DefFlag))])
base::colnames(final_train)[c(1,80)]<-c("Application_ID","DefFlag")


#########################  FUNKCJA GINI  ###############################

Gini_value <- function(
  score, #prediction from model
  target #target binary variable (0/1)
){
  
  default <- ifelse(target==0, 'G','B')
  d <- data.frame(FY = default, SCORE = score)
  s <- table(d[,2],d[,1])
  sHeader <- colnames(s)
  s <- cbind(s,apply(s,2,cumsum))
  colnames(s) <- c(sHeader,paste(sHeader,"_cum",sep=""))
  s <- cbind(s , s[,"B_cum"]/max(s[,"B_cum"]) , s[,"G_cum"]/max(s[,"G_cum"]),
             diff(c(0,s[,"G_cum"]))/max(s[,"G_cum"]))
  colnames(s)<-c(sHeader,
                 paste(sHeader,"_cum",sep=""),
                 c("%cum_bad","%cum_good","%good"))
  p <- 1:nrow(s)
  s <- cbind(s, c( s[1,7] , s[p[-1],7]+s[(p-1)[-1],7]) ) 
  s <- cbind(s, c(0,s[1:(nrow(s)-1),"%cum_bad"]))
  colnames(s)[length(colnames(s))] <- "%cum_bad_prev"
  auc <- sum(s[,"%good"]*(s[,"%cum_bad"]+s[,"%cum_bad_prev"])*0.5) 
  gini_value <- abs( 2 * ( auc - 0.5 ) )
  return(gini_value)
  
}


##########################   MODELE   ######################################

#Przeniesienie zbioru testowego
train1<-as.h2o(final_train) 
#Podzia³ zbioru jeszcze raz na zbior treningowy, walidacyjny i testowy, w celu sprawdzenia jakosci modelu na nasz uzytek
h2o.splitFrame(train1,
               ratios = c(0.7, 0.15),
               destination_frames = c("creditcard_train", "creditcard_valid", "creditcard_test"),
               seed = 1234)

card_train <- h2o.getFrame("creditcard_train")
card_valid <- h2o.getFrame("creditcard_valid")
card_test <- h2o.getFrame("creditcard_test")
test1<-as.data.frame(card_test)

#################### RANDOM FOREST ##########################
#Najlepszy model jaki otrzymalismy
rf1 <- h2o.randomForest(
  x = 2:79, 
  y = "DefFlag", 
  training_frame = card_train, 
  validation_frame = card_valid,
  model_id = "rf1", 
  ntrees = 45, 
  max_depth = 15, 
  mtries = 3, 
  sample_rate = 0.6320000291, 
  min_rows = 5, 
  seed = 1234
)

rf1_pred<-h2o.predict(rf1,train1)
rf1_pred1<-as.data.frame(rf1_pred)

test_data <- as.data.frame(cbind(Application_ID = final_train$Application_ID,
                                 Score = rf1_pred1, 
                                 DefFlag = final_train$DefFlag))
colnames(test_data) <- c("Application_ID","pred","p0" ,"Score", "DefFlag")

Gini_value(test_data$Score, test_data$DefFlag)

#Najlepszy model dla ca³ego zbioru treningowego Gini: 0,6889676

#W pêtli pozmienialismy parametry ntrees, mtries, max depth,min_rows najlepszy wynik otrzymalismy 
#dla 45, 3 ,15 , 5 

###############   POZOSTALE MODELE ################## (mo¿na pominaæ, koñcowa predykcja linika 250)

############  GRADIENT BOOSTING MACHINE  #####################

gbm1 <- h2o.gbm(
  x = 2:79,
  y = "DefFlag",
  training_frame = card_train,
  validation_frame = card_valid,
  model_id = "gbm1",
  ntrees = 30, 
  max_depth = 2,
  learn_rate = 0.2, 
  learn_rate_annealing = 1,
  seed = 1234
)
gbm1_pred<-h2o.predict(gbm1,train1)
gbm1_pred1<-as.data.frame(gbm1_pred) 

test_data <- as.data.frame(cbind(Application_ID = final_train$Application_ID,
                                 Score = gbm1_pred1, 
                                 DefFlag = final_train$DefFlag))
colnames(test_data) <- c("Application_ID","predict","p0","Score", "DefFlag")

Gini_value(test_data$Score, test_data$DefFlag)
#0.5836589 <- najlepszy jaki nam wyszedl, sprawdzalismy ntrees, maxdepth, learn rate, learn rate annealing

#################### NEURALNET #########################


nn_1 <- h2o.deeplearning(x = 2:79,
                         y = "DefFlag",
                         training_frame = card_train,
                         validation_frame =card_valid, 
                         distribution = "AUTO",
                         activation = "Rectifier",
                         
                         model_id = "nn_1",
                         
                         l2 =0,
                         ignore_const_cols = FALSE, 
                         hidden = c(40,10,20), 
                         export_weights_and_biases = TRUE, 
                         seed = 4321)

nn_pred<-h2o.predict(nn_1,train1)
nn_pred1<-as.data.frame(nn_pred) 

test_data <- as.data.frame(cbind(Application_ID = final_train$Application_ID,
                                 Score = nn_pred1, 
                                 DefFlag = final_train$DefFlag))
colnames(test_data) <- c("Application_ID","predict","p0","Score", "DefFlag")

Gini_value(test_data$Score, test_data$DefFlag)
#GINI 0.56267

###################  AUTOML  ##############################

aml <- h2o.automl(x = 2:80,
                  y = "DefFlag",
                  training_frame = card_train,
                  validation_frame = card_valid,
                  leaderboard_frame = card_test,
                  nfolds = 5,
                  seed = 1234,
                  max_runtime_secs =5*3600 #UWAGA !!! liczy do 5 godzin
)



aml_pred2<-as.data.frame(h2o.predict((aml@leader),train1))
test_data <- as.data.frame(cbind(Application_ID = final_train$Application_ID,
                                 Score = aml_pred2, 
                                 DefFlag = final_train$DefFlag))
colnames(test_data) <- c("Application_ID","predict","p0", "Score", "DefFlag")

Gini_value(test_data$Score, test_data$DefFlag)
#Gini dla treningowego 0.82 ale duzy overfitting, prawdopodobnie za duzy stacking

#################### MODELE GLM ######################

## 75% of the sample size
smp_size <- floor(0.90 * nrow(final_train))
## set the seed to make your partition reproducible
set.seed(123)
train_ind <- sample(seq_len(nrow(final_train)), size = smp_size)

train <- final_train[train_ind, ]
test <- final_train[-train_ind, ]

#Model GLM
glm1<-glm(data = final_train[,-1], formula = DefFlag ~ ., family = binomial(link = "logit")) 
predictions <- predict(glm1, final_train, type = "response") 
pred_df<-as.data.frame(predictions)

test_data <- as.data.frame(cbind(Application_ID = final_train$Application_ID,
                                 Score = pred_df, 
                                 DefFlag = final_train$DefFlag))
colnames(test_data) <- c("Application_ID", "Score", "DefFlag")

Gini_value(test_data$Score, test_data$DefFlag)
#GINI 0.517474

###########  KONCOWE PREDYKCJE ##########################
#Przygotowanie zmiennych 
final_data<-final_data%>%select(-Car_status)

app<-final_data[,c(2:12)]
behavioral<-final_data[,c(51:89)]
geo<-final_data[,c(13:50)]


h2o_beh_f<-as.h2o(behavioral,destination_frame = "beh_f")
pca_beh_f <- h2o.prcomp(training_frame = "beh_f", k = 32, transform = "STANDARDIZE")

short_beh_f<-h2o.predict(pca_beh_f,h2o_beh_f)
short_behavioral_f<-as.data.frame(short_beh_f)


h2o_geo_f<-as.h2o(geo,destination_frame = "geo_f")
pca_geo_f <- h2o.prcomp(training_frame = "geo_f", k = 35, transform = "STANDARDIZE")
short_geo_f<-h2o.predict(pca_geo_f,h2o_geo_f)
short_geog_f<-as.data.frame(short_geo_f)
base::colnames(short_geog_f)<-c("PCG1","PCG2","PCG3","PCG4","PCG5","PCG6","PCG7","PCG8","PCG9","PCG10",
                              "PCG11","PCG12","PCG13","PCG14","PCG15","PCG16","PCG17","PCG18","PCG19","PCG20",
                              "PCG21","PCG22","PCG23","PCG24","PCG25","PCG26","PCG27","PCG28","PCG29","PCG30",
                              "PCG31","PCG32","PCG33","PCG34","PCG35")

final_data<-cbind(final_data[,1],app,short_geog_f,short_behavioral_f,final_data$DefFlag)
base::colnames(final_data)[c(1,80)]<-c("Application_ID","DefFlag")
#Predykcja
data_h2o<-as.h2o(final_data)
final_predictions<-as.data.frame(h2o.predict(rf1,data_h2o))
test_data <- as.data.frame(cbind(Application_ID = final_data$Application_ID,
                                 Score = final_predictions, 
                                 DefFlag = final_data$DefFlag))
test_data<-test_data[,c(1,3,5)]
colnames(test_data) <- c("Application_ID","Score", "DefFlag")
write.table(test_data[,c(1,2)], "Output.csv",row.names =F, sep=";",dec=".")
