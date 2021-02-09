Jurnal
================
Nabila Bianca Putri

# Library

``` r
library(readr)
library(dplyr)
library(knitr)
```

# Data

``` r
data <- read_csv("PhishingData.csv")
head(data)
```

    ## # A tibble: 6 x 11
    ##      id   SFH popUpWidnow SSLfinal_State Request_URL URL_of_Anchor web_traffic
    ##   <dbl> <dbl>       <dbl>          <dbl>       <dbl>         <dbl>       <dbl>
    ## 1     1     1          -1              1          -1            -1           1
    ## 2     2    -1          -1             -1          -1            -1           0
    ## 3     3     1          -1              0           0            -1           0
    ## 4     4     1           0              1          -1            -1           0
    ## 5     5    -1          -1              1          -1             0           0
    ## 6     6    -1          -1              1          -1            -1           1
    ## # ... with 4 more variables: URL_Length <dbl>, age_of_domain <dbl>,
    ## #   having_IP_Address <dbl>, Result <dbl>

``` r
dataFix <- data[,-1]
head(dataFix)
```

    ## # A tibble: 6 x 10
    ##     SFH popUpWidnow SSLfinal_State Request_URL URL_of_Anchor web_traffic
    ##   <dbl>       <dbl>          <dbl>       <dbl>         <dbl>       <dbl>
    ## 1     1          -1              1          -1            -1           1
    ## 2    -1          -1             -1          -1            -1           0
    ## 3     1          -1              0           0            -1           0
    ## 4     1           0              1          -1            -1           0
    ## 5    -1          -1              1          -1             0           0
    ## 6    -1          -1              1          -1            -1           1
    ## # ... with 4 more variables: URL_Length <dbl>, age_of_domain <dbl>,
    ## #   having_IP_Address <dbl>, Result <dbl>

``` r
str(dataFix)
```

    ## tibble [1,353 x 10] (S3: tbl_df/tbl/data.frame)
    ##  $ SFH              : num [1:1353] 1 -1 1 1 -1 -1 1 1 -1 -1 ...
    ##  $ popUpWidnow      : num [1:1353] -1 -1 -1 0 -1 -1 -1 0 -1 0 ...
    ##  $ SSLfinal_State   : num [1:1353] 1 -1 0 1 1 1 0 1 0 -1 ...
    ##  $ Request_URL      : num [1:1353] -1 -1 0 -1 -1 -1 1 1 -1 -1 ...
    ##  $ URL_of_Anchor    : num [1:1353] -1 -1 -1 -1 0 -1 -1 0 -1 1 ...
    ##  $ web_traffic      : num [1:1353] 1 0 0 0 0 1 0 0 -1 1 ...
    ##  $ URL_Length       : num [1:1353] 1 1 -1 1 -1 0 0 0 -1 0 ...
    ##  $ age_of_domain    : num [1:1353] 1 1 1 1 1 -1 1 1 1 -1 ...
    ##  $ having_IP_Address: num [1:1353] 0 1 0 0 0 0 0 1 0 0 ...
    ##  $ Result           : num [1:1353] 0 1 1 0 1 1 -1 -1 0 1 ...

``` r
dataFix$SFH <- as.factor(dataFix$SFH)
dataFix$popUpWidnow <- as.factor(dataFix$popUpWidnow)
dataFix$SSLfinal_State <- as.factor(dataFix$SSLfinal_State)
dataFix$Request_URL <- as.factor(dataFix$Request_URL)
dataFix$URL_Length <- as.factor(dataFix$URL_Length)
dataFix$URL_of_Anchor <- as.factor(dataFix$URL_of_Anchor)
dataFix$web_traffic <- as.factor(dataFix$web_traffic)
dataFix$age_of_domain <- as.factor(dataFix$age_of_domain)
dataFix$having_IP_Address <- as.factor(dataFix$having_IP_Address)
dataFix$Result <- as.factor(dataFix$Result)

dataFix$Result <- recode(dataFix$Result, '-1'="legitimate", '0'="suspicious", '1'="phishy")


str(dataFix)
```

    ## tibble [1,353 x 10] (S3: tbl_df/tbl/data.frame)
    ##  $ SFH              : Factor w/ 3 levels "-1","0","1": 3 1 3 3 1 1 3 3 1 1 ...
    ##  $ popUpWidnow      : Factor w/ 3 levels "-1","0","1": 1 1 1 2 1 1 1 2 1 2 ...
    ##  $ SSLfinal_State   : Factor w/ 3 levels "-1","0","1": 3 1 2 3 3 3 2 3 2 1 ...
    ##  $ Request_URL      : Factor w/ 3 levels "-1","0","1": 1 1 2 1 1 1 3 3 1 1 ...
    ##  $ URL_of_Anchor    : Factor w/ 3 levels "-1","0","1": 1 1 1 1 2 1 1 2 1 3 ...
    ##  $ web_traffic      : Factor w/ 3 levels "-1","0","1": 3 2 2 2 2 3 2 2 1 3 ...
    ##  $ URL_Length       : Factor w/ 3 levels "-1","0","1": 3 3 1 3 1 2 2 2 1 2 ...
    ##  $ age_of_domain    : Factor w/ 2 levels "-1","1": 2 2 2 2 2 1 2 2 2 1 ...
    ##  $ having_IP_Address: Factor w/ 2 levels "0","1": 1 2 1 1 1 1 1 2 1 1 ...
    ##  $ Result           : Factor w/ 3 levels "legitimate","suspicious",..: 2 3 3 2 3 3 1 1 2 3 ...

``` r
#cek apakah terdapat missing value
sapply(dataFix, function(x) sum(is.na(x)))
```

    ##               SFH       popUpWidnow    SSLfinal_State       Request_URL 
    ##                 0                 0                 0                 0 
    ##     URL_of_Anchor       web_traffic        URL_Length     age_of_domain 
    ##                 0                 0                 0                 0 
    ## having_IP_Address            Result 
    ##                 0                 0

``` r
#Split data
set.seed(123)
sampel <- sample(2,nrow(dataFix),replace = T, prob = c(0.8,0.2))
trainingdat <- dataFix[sampel==1, ]
testingdat <- dataFix[sampel==2, ]
print(paste("Jumlah train data :", nrow(trainingdat)))
```

    ## [1] "Jumlah train data : 1093"

``` r
print(paste("Jumlah test data :", nrow(testingdat)))
```

    ## [1] "Jumlah test data : 260"

# Algoritma Klasifikasi

## Naive Bayes

``` r
library(naivebayes)
library(psych)
library(caret)
```

``` r
modelnaiv <- naive_bayes(Result~.,data=trainingdat, laplace = T)
modelnaiv
```

    ## 
    ## ================================== Naive Bayes ================================== 
    ##  
    ##  Call: 
    ## naive_bayes.formula(formula = Result ~ ., data = trainingdat, 
    ##     laplace = T)
    ## 
    ## --------------------------------------------------------------------------------- 
    ##  
    ## Laplace smoothing: TRUE
    ## 
    ## --------------------------------------------------------------------------------- 
    ##  
    ##  A priori probabilities: 
    ## 
    ## legitimate suspicious     phishy 
    ## 0.53613907 0.06678866 0.39707228 
    ## 
    ## --------------------------------------------------------------------------------- 
    ##  
    ##  Tables: 
    ## 
    ## --------------------------------------------------------------------------------- 
    ##  ::: SFH (Categorical) 
    ## --------------------------------------------------------------------------------- 
    ##     
    ## SFH   legitimate  suspicious      phishy
    ##   -1 0.088285229 0.421052632 0.627002288
    ##   0  0.005093379 0.131578947 0.226544622
    ##   1  0.906621392 0.447368421 0.146453089
    ## 
    ## --------------------------------------------------------------------------------- 
    ##  ::: popUpWidnow (Categorical) 
    ## --------------------------------------------------------------------------------- 
    ##            
    ## popUpWidnow legitimate suspicious     phishy
    ##          -1 0.18336163 0.44736842 0.66361556
    ##          0  0.58064516 0.38157895 0.33409611
    ##          1  0.23599321 0.17105263 0.00228833
    ## 
    ## --------------------------------------------------------------------------------- 
    ##  ::: SSLfinal_State (Categorical) 
    ## --------------------------------------------------------------------------------- 
    ##               
    ## SSLfinal_State legitimate suspicious     phishy
    ##             -1 0.08149406 0.13157895 0.44164760
    ##             0  0.10865874 0.30263158 0.30434783
    ##             1  0.80984720 0.56578947 0.25400458
    ## 
    ## --------------------------------------------------------------------------------- 
    ##  ::: Request_URL (Categorical) 
    ## --------------------------------------------------------------------------------- 
    ##            
    ## Request_URL legitimate suspicious     phishy
    ##          -1 0.35653650 0.80263158 0.51029748
    ##          0  0.27334465 0.18421053 0.38215103
    ##          1  0.37011885 0.01315789 0.10755149
    ## 
    ## --------------------------------------------------------------------------------- 
    ##  ::: URL_of_Anchor (Categorical) 
    ## --------------------------------------------------------------------------------- 
    ##              
    ## URL_of_Anchor legitimate suspicious     phishy
    ##            -1 0.29881154 0.55263158 0.59496568
    ##            0  0.14261460 0.17105263 0.09382151
    ##            1  0.55857385 0.27631579 0.31121281
    ## 
    ## ---------------------------------------------------------------------------------
    ## 
    ## # ... and 4 more tables
    ## 
    ## ---------------------------------------------------------------------------------

``` r
summary(modelnaiv)
```

    ## 
    ## ================================== Naive Bayes ================================== 
    ##  
    ## - Call: naive_bayes.formula(formula = Result ~ ., data = trainingdat,      laplace = T) 
    ## - Laplace: TRUE 
    ## - Classes: 3 
    ## - Samples: 1093 
    ## - Features: 9 
    ## - Conditional distributions: 
    ##     - Bernoulli: 2
    ##     - Categorical: 7
    ## - Prior probabilities: 
    ##     - legitimate: 0.5361
    ##     - suspicious: 0.0668
    ##     - phishy: 0.3971
    ## 
    ## ---------------------------------------------------------------------------------

``` r
prediksiNB <- predict(modelnaiv, testingdat)
```

    ## Warning: predict.naive_bayes(): more features in the newdata are provided as
    ## there are probability tables in the object. Calculation is performed based on
    ## features to be found in the tables.

``` r
conf_NB <- confusionMatrix(table(prediksiNB,testingdat$Result))
conf_NB
```

    ## Confusion Matrix and Statistics
    ## 
    ##             
    ## prediksiNB   legitimate suspicious phishy
    ##   legitimate        106         13      8
    ##   suspicious          1          4      2
    ##   phishy              9         13    104
    ## 
    ## Overall Statistics
    ##                                           
    ##                Accuracy : 0.8231          
    ##                  95% CI : (0.7712, 0.8675)
    ##     No Information Rate : 0.4462          
    ##     P-Value [Acc > NIR] : < 2.2e-16       
    ##                                           
    ##                   Kappa : 0.6877          
    ##                                           
    ##  Mcnemar's Test P-Value : 0.0003618       
    ## 
    ## Statistics by Class:
    ## 
    ##                      Class: legitimate Class: suspicious Class: phishy
    ## Sensitivity                     0.9138           0.13333        0.9123
    ## Specificity                     0.8542           0.98696        0.8493
    ## Pos Pred Value                  0.8346           0.57143        0.8254
    ## Neg Pred Value                  0.9248           0.89723        0.9254
    ## Prevalence                      0.4462           0.11538        0.4385
    ## Detection Rate                  0.4077           0.01538        0.4000
    ## Detection Prevalence            0.4885           0.02692        0.4846
    ## Balanced Accuracy               0.8840           0.56014        0.8808

## Random Forest

``` r
library(randomForest)
library(caret)
library(psych)
```

``` r
set.seed(123)   
model <- randomForest(Result~., data=trainingdat)
model
```

    ## 
    ## Call:
    ##  randomForest(formula = Result ~ ., data = trainingdat) 
    ##                Type of random forest: classification
    ##                      Number of trees: 500
    ## No. of variables tried at each split: 3
    ## 
    ##         OOB estimate of  error rate: 10.25%
    ## Confusion matrix:
    ##            legitimate suspicious phishy class.error
    ## legitimate        544          5     37  0.07167235
    ## suspicious         14         52      7  0.28767123
    ## phishy             42          7    385  0.11290323

``` r
prediksiRF <- predict(model, testingdat)
conf_RF <- confusionMatrix(table(prediksiRF, testingdat$Result))
conf_RF
```

    ## Confusion Matrix and Statistics
    ## 
    ##             
    ## prediksiRF   legitimate suspicious phishy
    ##   legitimate        106          4      5
    ##   suspicious          0         21      0
    ##   phishy             10          5    109
    ## 
    ## Overall Statistics
    ##                                         
    ##                Accuracy : 0.9077        
    ##                  95% CI : (0.8658, 0.94)
    ##     No Information Rate : 0.4462        
    ##     P-Value [Acc > NIR] : < 2e-16       
    ##                                         
    ##                   Kappa : 0.842         
    ##                                         
    ##  Mcnemar's Test P-Value : 0.01367       
    ## 
    ## Statistics by Class:
    ## 
    ##                      Class: legitimate Class: suspicious Class: phishy
    ## Sensitivity                     0.9138           0.70000        0.9561
    ## Specificity                     0.9375           1.00000        0.8973
    ## Pos Pred Value                  0.9217           1.00000        0.8790
    ## Neg Pred Value                  0.9310           0.96234        0.9632
    ## Prevalence                      0.4462           0.11538        0.4385
    ## Detection Rate                  0.4077           0.08077        0.4192
    ## Detection Prevalence            0.4423           0.08077        0.4769
    ## Balanced Accuracy               0.9256           0.85000        0.9267

## Decision Tree

``` r
library(party)
library(psych)
library(caret)
```

``` r
pohon <- ctree(Result~., data=trainingdat)
plot(pohon)
```

![](jurnal_files/figure-gfm/unnamed-chunk-15-1.png)<!-- -->

``` r
prediksiDT <- predict(pohon, testingdat)
conf_DT <- confusionMatrix(table(prediksiDT,testingdat$Result))
conf_DT
```

    ## Confusion Matrix and Statistics
    ## 
    ##             
    ## prediksiDT   legitimate suspicious phishy
    ##   legitimate        103          1      5
    ##   suspicious          1         11      0
    ##   phishy             12         18    109
    ## 
    ## Overall Statistics
    ##                                           
    ##                Accuracy : 0.8577          
    ##                  95% CI : (0.8092, 0.8978)
    ##     No Information Rate : 0.4462          
    ##     P-Value [Acc > NIR] : < 2.2e-16       
    ##                                           
    ##                   Kappa : 0.7517          
    ##                                           
    ##  Mcnemar's Test P-Value : 0.0001114       
    ## 
    ## Statistics by Class:
    ## 
    ##                      Class: legitimate Class: suspicious Class: phishy
    ## Sensitivity                     0.8879           0.36667        0.9561
    ## Specificity                     0.9583           0.99565        0.7945
    ## Pos Pred Value                  0.9450           0.91667        0.7842
    ## Neg Pred Value                  0.9139           0.92339        0.9587
    ## Prevalence                      0.4462           0.11538        0.4385
    ## Detection Rate                  0.3962           0.04231        0.4192
    ## Detection Prevalence            0.4192           0.04615        0.5346
    ## Balanced Accuracy               0.9231           0.68116        0.8753

## SVM

``` r
library(tidyverse)
library(e1071)
library(caret)
```

``` r
modelSVM <- svm(Result~., data=dataFix)
summary(modelSVM)
```

    ## 
    ## Call:
    ## svm(formula = Result ~ ., data = dataFix)
    ## 
    ## 
    ## Parameters:
    ##    SVM-Type:  C-classification 
    ##  SVM-Kernel:  radial 
    ##        cost:  1 
    ## 
    ## Number of Support Vectors:  496
    ## 
    ##  ( 103 200 193 )
    ## 
    ## 
    ## Number of Classes:  3 
    ## 
    ## Levels: 
    ##  legitimate suspicious phishy

``` r
prediksiSVM <- predict(modelSVM, dataFix)
conf_SVM <- confusionMatrix(table(Predicted = prediksiSVM, Actual=dataFix$Result))
conf_SVM
```

    ## Confusion Matrix and Statistics
    ## 
    ##             Actual
    ## Predicted    legitimate suspicious phishy
    ##   legitimate        655         46     38
    ##   suspicious          1          3      1
    ##   phishy             46         54    509
    ## 
    ## Overall Statistics
    ##                                          
    ##                Accuracy : 0.8625         
    ##                  95% CI : (0.843, 0.8804)
    ##     No Information Rate : 0.5188         
    ##     P-Value [Acc > NIR] : < 2.2e-16      
    ##                                          
    ##                   Kappa : 0.7426         
    ##                                          
    ##  Mcnemar's Test P-Value : < 2.2e-16      
    ## 
    ## Statistics by Class:
    ## 
    ##                      Class: legitimate Class: suspicious Class: phishy
    ## Sensitivity                     0.9330          0.029126        0.9288
    ## Specificity                     0.8710          0.998400        0.8758
    ## Pos Pred Value                  0.8863          0.600000        0.8358
    ## Neg Pred Value                  0.9235          0.925816        0.9476
    ## Prevalence                      0.5188          0.076127        0.4050
    ## Detection Rate                  0.4841          0.002217        0.3762
    ## Detection Prevalence            0.5462          0.003695        0.4501
    ## Balanced Accuracy               0.9020          0.513763        0.9023

# Hasil

``` r
data.frame(accuracy = c(paste(round((conf_NB$overall[1])*100,2), "%"),
                        paste(round((conf_RF$overall[1])*100,2), "%"),
                        paste(round((conf_DT$overall[1])*100,2), "%"),
                        paste(round((conf_SVM$overall[1])*100,2), "%")),
           precision = c(paste(round((conf_NB$byClass[3,3])*100,2), "%"),
                         paste(round((conf_RF$byClass[3,3])*100,2), "%"),
                         paste(round((conf_DT$byClass[3,3])*100,2), "%"),
                         paste(round((conf_SVM$byClass[3,3])*100,2), "%")),
           sensitivity = c(paste(round((conf_NB$byClass[3,1])*100,2), "%"),
                         paste(round((conf_RF$byClass[3,1])*100,2), "%"),
                         paste(round((conf_DT$byClass[3,1])*100,2), "%"),
                         paste(round((conf_SVM$byClass[3,1])*100,2), "%"))) %>% 
   `rownames<-`(c("Naive Bayes", "Random Forest", "Decision Tree", "SVM")) %>%
  kable()
```

|               | accuracy | precision | sensitivity |
| :------------ | :------- | :-------- | :---------- |
| Naive Bayes   | 82.31 %  | 82.54 %   | 91.23 %     |
| Random Forest | 90.77 %  | 87.9 %    | 95.61 %     |
| Decision Tree | 85.77 %  | 78.42 %   | 95.61 %     |
| SVM           | 86.25 %  | 83.58 %   | 92.88 %     |
