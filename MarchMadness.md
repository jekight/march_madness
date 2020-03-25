---
title: "2018 March Madness Competition"
author: "Jeremy Kight"
date: "3/16/2020"
output:
  html_document:
    keep_md: true
    toc: true
    toc_depth: 4
    toc_float: yes
---

***

# Abstract

|        For the past several years, Kaggle has hosted a competition where individuals construct machine learning algorithms with the objective of predicting which team will win each game in the NCAA March Madness Tournament. The individual creators determine which model and metrics to use, and each submitted model is evaluated and scored on the log loss metric. After several tests and careful consideration, a neural network was chosen for this report as the primary model to predict wins. The neural network was tuned on a training set that contained all the tournament games from 2003 to 2018. The final tuning modifications resulted in a size parameter of 1 and a decay value of 0.01. Overall, the neural network model can accurately predict the winning teams for the 2018 tournament with an accuracy of 80.6% and a log loss of 0.492. 

# Introduction

|        Since 1939, the best college basketball teams have competed in a single-elimination tournament with one goal: be crowned as the national champion. Logically, it seems the top team should always win; however, basketball is a game where anything can happen. Every year, there is an upset where something completely unpredictable occurs. For example, on March 16, 2018, the University of Maryland, Baltimore County (UMBC) defeated the University of Virginia (UVA) to become the first 16-seed to upset a 1-seed. In fact, the chaotic nature of the basketball games in the tournament is how the term "March Madness" came about.

|        As a fan, it is difficult to determine which teams will win and lose each game. For someone with no knowledge of basketball or the competing teams, it is even more difficult. The odds of an individual randomly predicting each game correctly is roughly 1 in 9.2 quintillion! However, according to [NCAA.com](https://www.ncaa.com/news/basketball-men/bracketiq/2020-01-15/perfect-ncaa-bracket-absurd-odds-march-madness-dream), if an individual understands basketball and has knowledge of the various teams participating in the tournament, that individual's odds increase to about 1 in 120.2 billion. While the odds of predicting a perfect bracket remain incredibly low, there is a clear advantage in knowing the sport and the competing teams. So, what does it mean to “understand basketball”, and what are the most important team characteristics that lead to success in the tournament? If these questions can be correctly answered and applied to machine learning algorithms, it could be possible to perfectly predict the winners of the March Madness Tournament. 



# Prepping The Data

|        This report will focus on the construction of the machine learning model used to predict the outcomes of the 2018 March Madness Tournament. While the finished model is the highlight of the project, the real work was performing exploratory data analysis. A separate report was created in order to get a better understanding as to what made the winningest teams so successful through the use of data visualizations. That report can be found [here](https://jeremykight.netlify.com/marchmadnessvisuals/). After identifying which statistics should probably be considered when predicting match ups, the data was prepped for the model building phase. Data prepping was completed in a different R script for the simple reason of keeping the actual model building scrip shorter. The R script used for data prepping can be found [here](https://github.com/jekight/march_madness/blob/master/data_prep.R). Now, before getting to the actual model building process, it is important to know which team statistics are being used. The table below shows the team statistics along with the abbreviation used in the data prepping code.
<br>

|Statistic Abbreviation|Statistic Name                |
|----------------------|------------------------------|
|POM                   |Ken Pomeroy's Team Rankings   |
|Seed                  |Tournament Seed               |
|Score                 |Score                         |
|FGA                   |Field Goals Attempted         |
|FGM2                  |2pt Shots Made                |
|FGM3                  |3pt Shots Made                |
|Avg3pt                |3pt Average Per Game          |
|EFG                   |Effective Field Goal          |
|FTA                   |Free Throws Attempted         |
|OR                    |Offensive Rebounds            |
|TO                    |Turnovers                     |
|POSS                  |Possessions                   |
|OFF                   |Offensive Ratings             |
|OPP_Score             |Opponent's Score              |
|DEF_EFF               |Defensive Efficiency          |
|BLK                   |Blocks                        |
|DR                    |Defensive Rebounds            |
|AST                   |Assists                       |
|STL                   |Steals                        |
|PF                    |Personal Fouls                |
<br>



|        For every recorded game, there should be two values for each of the statistics above. The reason for this is because each team playing will have their own statistics. One thing that was done to eliminate some of the variables was to take the difference of each statistic between the two teams playing, relative to the lower ID team. This allows the model to compare the teams' statistics using less variables. Next, since not all the variables were on the same scale, the data was normalized using the max-min technique.

$$n = \frac{x-min(x)}{max(x)-min(x)}$$
<br>

|        Lastly, to take into account momentum heading into the tournament only the last 30 days of each season was used in finding the averages for each of the statistics above. Sometimes teams play really well but struggle towards the end of the season. Then once the tournament starts, their struggles continue. Or the opposite occurs and a team starts playing their best ball right before the tournament. Again, to see the full code that was used to prepare the data, click [here](https://github.com/jekight/march_madness/blob/master/data_prep.R).

# Model Building

|        To begin building predictive models, the required libraries need to be loaded into R. If the following libraries have not been installed, then each library would need to be downloaded first. 


```r
library(tidyverse)
library(caret)
library(data.table)
library(Metrics)
library(knitr)
```

|        Now the dataframe called *TourneyNormal*, that was created through the data prepping process, can be loaded into the global environment. It is recommended to look through the data prep code, especially if adjustments were to be made to the created models. Also, the dataframe is being loaded in from a local destination. It is too large to put on Github but by clicking [here](https://github.com/jekight/march_madness/blob/master/data_prep.R) and running the script, the *TourneyNormal* dataframe can be written locally to any computer.


```r
TourneyDiff <- read_csv("TourneyDiff.csv")
```

|        To make sure that the dataframe was loaded in correctly, a quick peak can be taken by using the head() function. To see the entire dataframe, the view() function should be utilized. However, this is a relatively large dataframe so the head() function will be what is used.


```r
head(TourneyDiff,5)
```

```
## # A tibble: 5 x 29
##   Season ID    Results TeamID.x TeamID.y TeamName.x TeamName.y Conference.x
##    <dbl> <chr>   <dbl>    <dbl>    <dbl> <chr>      <chr>      <chr>       
## 1   2003 2003…      NA     1328     1448 Oklahoma   Wake Fore… Big 12 Conf…
## 2   2003 2003…       0     1328     1393 Oklahoma   Syracuse   Big 12 Conf…
## 3   2003 2003…      NA     1328     1329 Oklahoma   Oklahoma … Big 12 Conf…
## 4   2003 2003…      NA     1328     1386 Oklahoma   St Joseph… Big 12 Conf…
## 5   2003 2003…      NA     1328     1335 Oklahoma   Penn       Big 12 Conf…
## # … with 21 more variables: Conference.y <chr>, POM_diff <dbl>,
## #   Seed_diff <dbl>, Score_diff <dbl>, FGA_diff <dbl>, FGM2_diff <dbl>,
## #   FGM3_dff <dbl>, Avg3pt_diff <dbl>, EFG_diff <dbl>, FTA_diff <dbl>,
## #   OR_diff <dbl>, TO_diff <dbl>, POSS_diff <dbl>, OFF_diff <dbl>,
## #   OPP_SCORE_diff <dbl>, DEF_EFF_diff <dbl>, BLK_diff <dbl>, DR_diff <dbl>,
## #   AST_diff <dbl>, STL_diff <dbl>, PF_diff <dbl>
```

|        So this dataframe contains 29 variables and 37,142 observations. The reason why there are so many observations is because it includes every possible match up for every tournament. For example, The University of Oklahoma is matched against every team in the 2003 March Madness Tournament. Most of those match ups never happened because the most amount of different opponents a team can face in one tournament is seven. To differeniate the games that did and did not happen, just look at the "Results" column. A "0" indicates that the lower ID team lost that game and a "1" means that the lower ID team won. Games that did not happen that year are represented by "NA". The filter the dataframe so that only the real games are included, simply run the following code.


```r
RealGames <- TourneyDiff %>%
  filter(!is.na(TourneyDiff$Results))
```

Again, to check to see if the dataframe only contains real games use the head() function.


```r
head(RealGames,5)
```

```
## # A tibble: 5 x 29
##   Season ID    Results TeamID.x TeamID.y TeamName.x TeamName.y Conference.x
##    <dbl> <chr>   <dbl>    <dbl>    <dbl> <chr>      <chr>      <chr>       
## 1   2003 2003…       0     1328     1393 Oklahoma   Syracuse   Big 12 Conf…
## 2   2003 2003…       1     1328     1354 Oklahoma   S Carolin… Big 12 Conf…
## 3   2003 2003…       1     1393     1400 Syracuse   Texas      Big East Co…
## 4   2003 2003…       0     1329     1393 Oklahoma … Syracuse   Big 12 Conf…
## 5   2003 2003…       1     1329     1335 Oklahoma … Penn       Big 12 Conf…
## # … with 21 more variables: Conference.y <chr>, POM_diff <dbl>,
## #   Seed_diff <dbl>, Score_diff <dbl>, FGA_diff <dbl>, FGM2_diff <dbl>,
## #   FGM3_dff <dbl>, Avg3pt_diff <dbl>, EFG_diff <dbl>, FTA_diff <dbl>,
## #   OR_diff <dbl>, TO_diff <dbl>, POSS_diff <dbl>, OFF_diff <dbl>,
## #   OPP_SCORE_diff <dbl>, DEF_EFF_diff <dbl>, BLK_diff <dbl>, DR_diff <dbl>,
## #   AST_diff <dbl>, STL_diff <dbl>, PF_diff <dbl>
```

|        Now that all the games that did not happen are filtered out, the dataframe can be split into training and testing sets. Instead of splitting the data with a 70-30  or 75-25 split, the data will be split by the seasons. The training dataset will contain the games from 2003 to 2018, while the testing dataset will only contain games from the 2019 season. It should be noted that the way basketball is played at the collegiate level has changed throughout the years. Thus, only using recent years would be a logical idea that should be explored. However, the models that are about to be created did not perform so well as compared to having more data (years of the tournament) available. 2003 was chosen as the furthest year back because it is the first year all of the features had data available. Another key note is that the "Results" column needed to be transformed into a factor since this is ultimately going to be a classification problem. The models will try to predict whether the lower ID team will win the match up or lose.


```r
#Training data will be from 2003 to 2018
Train1 <- RealGames %>%
  filter(Season <= 2018) %>%
  select(Results,POM_diff:PF_diff)

#Make Results a factor
Train1$Results <- as.factor(Train1$Results)

#Testing data will be 2019
Test1 <- RealGames %>%
  filter(Season == 2019) %>%
  select(Results,POM_diff:PF_diff)

#Make Results a factor
Test1$Results <- as.factor(Test1$Results)
```

|        The testing dataset will be left alone for now, as the focus will now be on using the training set to build some predictive models. To get an idea as to how each model will perform on the testing data, k-fold repeated cross validation will be utilized. While this does increase the time it takes to build each model, the models do become more consistent. In this case the k-fold cross validation will be repeated 3 times. Also, the metric the models will be measured on is accuracy. 


```r
control <- trainControl(method="repeatedcv", number=10, repeats = 5)
metric <- "Accuracy"
```

|        One of the many benefits of using the Caret package to build machine learning models is the abundance of different models available. In theory, every existing model that could be used for classification could be trained and compared. However, in this report only a few were chosen:

1. K-Nearest Neighbors (knn)
2. Support Vector Machines with Radial Basis Function Kernel (svmRadial)
3. Random Forest (rf)
4. Linear Discriminant Analysis (lda)
5. Boosted Logistic Regression (blr)
6. eXtreme Gradient Boosting Trees (xgbTree)
7. eXtreme Gradient Boosting Linear (xgbLinear)
8. Neural Network (nnet)

Remember to use set.seed() to ensure that results are reproducible!

```r
# kNN
set.seed(3142)
fit.knn <- train(Results~., data=Train1, method="knn",metric=metric, trControl=control)
# SVM
set.seed(3142)
fit.svm <- train(Results~., data=Train1, method="svmRadial",metric=metric, trControl=control)
# Random Forest
set.seed(3142)
fit.rf <- train(Results~., data=Train1, method="rf",metric=metric, trControl=control)
# Linear Algorithm
set.seed(3142)
fit.lda <- train(Results~., data=Train1, method="lda",metric=metric, trControl=control)
#Boosted Logistic Regression
set.seed(3142)
fit.blr <- train(Results~., data=Train1, method="LogitBoost",metric=metric, trControl=control)
#eXtreme Gradient Boosting Trees
set.seed(3142)
fit.xgbTree <- train(Results~., data=Train1, method="xgbTree",metric=metric, trControl=control)
#eXtreme Gradient Boosting Linear
set.seed(3142)
fit.xgbLM <- train(Results~., data=Train1, method="xgbLinear", metric=metric, trControl=control)
#Neural Network nnet
set.seed(3142)
fit.nnet <- train(Results~., data=Train1, method="nnet",metric=metric, trControl=control)
```

|        Now that all of the models are created, it is time to see how each model performed on the training sets. The top model will be chosen for further tuning and testing to see if it can be improved.


```r
# summarize accuracy of models
results <- resamples(list(lda=fit.lda, knn=fit.knn, svm=fit.svm, rf=fit.rf, blr=fit.blr, xgbTree=fit.xgbTree, xgbLM=fit.xgbLM,    nnet=fit.nnet))

dotplot(results)
```

![](MarchMadness_files/figure-html/unnamed-chunk-9-1.png)<!-- -->

|        It appears that the nnet model performed the best, at least on the training data. So from here on the neural network model will be the focus. One of the benefits of utilizing Caret to create models is that is that it tunes the model through the training phase. The parameters that were selected for the neural network can be seen below.


```r
print(fit.nnet)
```

```
## Neural Network 
## 
## 1048 samples
##   20 predictor
##    2 classes: '0', '1' 
## 
## No pre-processing
## Resampling: Cross-Validated (10 fold, repeated 5 times) 
## Summary of sample sizes: 943, 943, 943, 944, 943, 944, ... 
## Resampling results across tuning parameters:
## 
##   size  decay  Accuracy   Kappa    
##   1     0e+00  0.6990183  0.3978856
##   1     1e-04  0.6984451  0.3968017
##   1     1e-01  0.7089634  0.4177459
##   3     0e+00  0.6602839  0.3207143
##   3     1e-04  0.6612930  0.3223530
##   3     1e-01  0.6875952  0.3745876
##   5     0e+00  0.6343242  0.2687897
##   5     1e-04  0.6398681  0.2806304
##   5     1e-01  0.6803278  0.3603912
## 
## Accuracy was used to select the optimal model using the largest value.
## The final values used for the model were size = 1 and decay = 0.1.
```

|        The final parameter values used in the neural network were size = 1 and decay = 0.1. The model will be tested before trying to manually tune the size and decay values to see if a greater performance could be reached. 


```r
prednnet <- predict(fit.nnet, newdata = Test1, type = "raw")
confusionMatrix(prednnet, Test1$Results)
```

```
## Confusion Matrix and Statistics
## 
##           Reference
## Prediction  0  1
##          0 31  7
##          1  7 22
##                                           
##                Accuracy : 0.791           
##                  95% CI : (0.6743, 0.8808)
##     No Information Rate : 0.5672          
##     P-Value [Acc > NIR] : 0.000104        
##                                           
##                   Kappa : 0.5744          
##                                           
##  Mcnemar's Test P-Value : 1.000000        
##                                           
##             Sensitivity : 0.8158          
##             Specificity : 0.7586          
##          Pos Pred Value : 0.8158          
##          Neg Pred Value : 0.7586          
##              Prevalence : 0.5672          
##          Detection Rate : 0.4627          
##    Detection Prevalence : 0.5672          
##       Balanced Accuracy : 0.7872          
##                                           
##        'Positive' Class : 0               
## 
```
|        The neural network model was able to correctly identify the outcomes of the 2019 March Madness tournament at a 79.1% accuracy. To see which features (or variables) that the model relied on the most, a basic plot can be created to show variable importance.


```r
Importance1 <- varImp(fit.nnet)
plot(Importance1)
```

![](MarchMadness_files/figure-html/unnamed-chunk-12-1.png)<!-- -->
<br> 
|        Based on the figure above, the neural network model deemed the Pomeroy Rankings to be the most important factor in determining who won each game. This is logical and should be the most important variable based on the results from the data visualization report, found [here](https://jeremykight.netlify.com/marchmadnessvisuals/). Really there are no surprises with how the neural network ranked the importance of the various features. However, the Effective Field Goal statistic is normally a stat that many individuals use when trying to pick winners but the neural network did not rely on it that much. The neural network prioritized 2 and 3 point field goals made over all other offensive statistics. From the defensive side of the ball, steals and blocks were the defensive stats relied on the most.

|        Next, the bottom 7 features that were not relied on as much will be taken out. Then the neural network will be trained again but on the small dataset.


```r
#Create new dataframe without the bottom seven features
Train2 <- RealGames %>%
  filter(Season <= 2018) %>%
  select(Results,POM_diff:Avg3pt_diff, OPP_SCORE_diff,
         BLK_diff:PF_diff)

#Make Results a factor
Train2$Results <- as.factor(Train2$Results)

#Neural Network nnet2
set.seed(3142)
fit2.nnet <- train(Results~., data=Train2, method="nnet", metric=metric, trControl=control)
```

Now, the performance of the new model (fit2.nnet) will be compared to the original model that contained all of the features.


```r
#Compare accuracy of the Neural Networks
results2 <- resamples(list(nnet1=fit.nnet,nnet2=fit.nnet))

dotplot(results2)
```

![](MarchMadness_files/figure-html/unnamed-chunk-14-1.png)<!-- -->

|        It appears that the accuracy of the second neural network has actually increased slightly. The reduced amount of features for the neural network seems to have helped eliminate some of the noise that was generated by the unused or rarely used variables. The new variable importance plot should be viewed to see if any changes occured with how the neural network utilized the features.


```r
Importance2 <- varImp(fit2.nnet)
plot(Importance2)
```

![](MarchMadness_files/figure-html/unnamed-chunk-15-1.png)<!-- -->

|        Just to see what would happen if the bottom three features (Score_diff, OPP_SCORE_diff, and DR_diff) were removed from the dataset, a third neural network can be trained. The same steps will be repeated as above.


```r
#Create new dataframe without the bottom three features
Train3 <- RealGames %>%
  filter(Season <= 2018) %>%
  select(Results,POM_diff:Seed_diff, FGA_diff:Avg3pt_diff,
         BLK_diff,AST_diff:PF_diff)

#Make Results a factor
Train3$Results <- as.factor(Train3$Results)

#Neural Network nnet3
set.seed(3142)
fit3.nnet <- train(Results~., data=Train3, method="nnet", metric=metric, trControl=control)
```

|        The performance of the new model (fit3.nnet) will be compared to the original model that contained 20 features and the second neural network model that contained 13 features.


```r
#Compare accuracy of the Neural Networks
results3 <- resamples(list(nnet1=fit.nnet,nnet2=fit.nnet,nnet3=fit.nnet))

dotplot(results3)
```

![](MarchMadness_files/figure-html/unnamed-chunk-17-1.png)<!-- -->
Again, the newest neural network has the highest accuracy, though it appears to be by a very small amount. The variable importance rankings can be seen below.


```r
Importance3 <- varImp(fit3.nnet)
plot(Importance3)
```

![](MarchMadness_files/figure-html/unnamed-chunk-18-1.png)<!-- -->

|        The process above could continue to be implemented, however, for this report no more variables will be eleminated for the risk of taking out a potential important factor. Now it is time to tune the neural network.

# Tuning Neural Network

|        The neural network that will be tuned is the third neural network created (nnet3). This neural network maintained a high accuracy with less features. To see the parameters that Caret set during training, run the following code.


```r
print(fit3.nnet)
```

```
## Neural Network 
## 
## 1048 samples
##   10 predictor
##    2 classes: '0', '1' 
## 
## No pre-processing
## Resampling: Cross-Validated (10 fold, repeated 5 times) 
## Summary of sample sizes: 943, 943, 943, 944, 943, 944, ... 
## Resampling results across tuning parameters:
## 
##   size  decay  Accuracy   Kappa    
##   1     0e+00  0.7102857  0.4204235
##   1     1e-04  0.7106667  0.4212246
##   1     1e-01  0.7129524  0.4256111
##   3     0e+00  0.6906612  0.3811915
##   3     1e-04  0.6879872  0.3755584
##   3     1e-01  0.7068608  0.4131938
##   5     0e+00  0.6629872  0.3256704
##   5     1e-04  0.6751905  0.3498147
##   5     1e-01  0.7026575  0.4048880
## 
## Accuracy was used to select the optimal model using the largest value.
## The final values used for the model were size = 1 and decay = 0.1.
```

|        The values for size and decay will be taken into account when tuning the model manually. Above it can be seen that the optimal size was found to be 1, while the decay parameter was set to 0.1. To see if these are the best parameters for this neural network, an expanded tuning grid needs to be created.


```r
nnetgrid <- expand.grid(size = seq(from = 1, to = 5, by = 1),
                        decay = c(0.25,0.2,0.15, 0.1,0.075))
```

|        The size parameter will be tested on values 1 to 10 and the decay values will be tested with values of 0.5, 0.25 ,0.1, 0.05, 0.01, and 0.001. The neural network will be trained with these values and the optimal value will be chosen. The potential values for size and decay were chosen as approximate values close to what Caret chose.


```r
set.seed(3142)
nnet_tune1 <- train(Results~., 
                   data=Train3, 
                   method="nnet", 
                   metric=metric, 
                   trControl=control,
                   tuneGrid = nnetgrid)
```


```r
print(nnet_tune1)
```

```
## Neural Network 
## 
## 1048 samples
##   10 predictor
##    2 classes: '0', '1' 
## 
## No pre-processing
## Resampling: Cross-Validated (10 fold, repeated 5 times) 
## Summary of sample sizes: 943, 943, 943, 944, 943, 944, ... 
## Resampling results across tuning parameters:
## 
##   size  decay  Accuracy   Kappa    
##   1     0.075  0.7125696  0.4249327
##   1     0.100  0.7129524  0.4256200
##   1     0.150  0.7144908  0.4286659
##   1     0.200  0.7144927  0.4286484
##   1     0.250  0.7131538  0.4259513
##   2     0.075  0.7093370  0.4181478
##   2     0.100  0.7097198  0.4189925
##   2     0.150  0.7125824  0.4247271
##   2     0.200  0.7118168  0.4232750
##   2     0.250  0.7112491  0.4221146
##   3     0.075  0.7057106  0.4108962
##   3     0.100  0.7072381  0.4140187
##   3     0.150  0.7104908  0.4206053
##   3     0.200  0.7121978  0.4240488
##   3     0.250  0.7104799  0.4206176
##   4     0.075  0.7024670  0.4046463
##   4     0.100  0.7049267  0.4094115
##   4     0.150  0.7104853  0.4205838
##   4     0.200  0.7112418  0.4221527
##   4     0.250  0.7112454  0.4221228
##   5     0.075  0.6954158  0.3905087
##   5     0.100  0.7068535  0.4133919
##   5     0.150  0.7120110  0.4236825
##   5     0.200  0.7121960  0.4240294
##   5     0.250  0.7114341  0.4224985
## 
## Accuracy was used to select the optimal model using the largest value.
## The final values used for the model were size = 1 and decay = 0.2.
```

|        By setting the tuning grid to values close to the parameters that Caret prescribed, it was found that the optimal size and decay values are 1 and 0.2, respectively. While this is a minor adjustment, it should help the performance of the model on the testing set. Below, the performance of the new model on the testing set can be seen. Remember to take out the same features from the test dataset that was taken out of the training dataset for the third neural network model.


```r
Test2 <- Test1 %>%
  select(Results,POM_diff:Seed_diff, FGA_diff:Avg3pt_diff,
         BLK_diff,AST_diff:PF_diff)

prednnet2 <- predict(nnet_tune1, newdata = Test2, type = "raw")
confusionMatrix(prednnet2, Test2$Results)
```

```
## Confusion Matrix and Statistics
## 
##           Reference
## Prediction  0  1
##          0 31  6
##          1  7 23
##                                           
##                Accuracy : 0.806           
##                  95% CI : (0.6911, 0.8924)
##     No Information Rate : 0.5672          
##     P-Value [Acc > NIR] : 3.393e-05       
##                                           
##                   Kappa : 0.6064          
##                                           
##  Mcnemar's Test P-Value : 1               
##                                           
##             Sensitivity : 0.8158          
##             Specificity : 0.7931          
##          Pos Pred Value : 0.8378          
##          Neg Pred Value : 0.7667          
##              Prevalence : 0.5672          
##          Detection Rate : 0.4627          
##    Detection Prevalence : 0.5522          
##       Balanced Accuracy : 0.8044          
##                                           
##        'Positive' Class : 0               
## 
```
|        Thus by adjusting the tuning parameters slightly, the accuracy of the model was increased to 80.6%. The difference the adjustment made appears to have effected the ability of the model to predict one more winning team correctly. Since this Kaggle competition is ranked on the logloss metric, the logloss should be calculated for the final model. To do this, first the probabilities of each decision should be calculated by the model.


```r
#Get the probalities for nnet_tune1
probnnet <- predict(nnet_tune1, newdata = Test2, type = "prob") %>%
  select(2)
```

|        Next, a new dataframe will be created from the "RealGames" dataframe created earlier. The dataframe will be filtered so that it only contains the games from the 2019 season. Then a new column will be added to display the results from the predictions of the final model. Lastly, the only columns that will be selected are "ID", "Results", and "Predicitons". All the other columns will be discarded.


```r
EffectiveNNET <- RealGames %>%
  filter(Season == 2019) %>%
  mutate(Predictions = prednnet2) %>%
  select(ID,Results,Predictions)
```

|        Now that the new dataframe is created, it can be joined together with the probabilities calculated from the final model. This will be done by column binding "EffectiveNNET" by "probnnet". To be clear, the column with the title "1" will be renamed to "Probability" to indicate the probability of the lower ID team beating the higher ID team.


```r
EffectiveNNET <- cbind(EffectiveNNET,probnnet) %>%
  rename(Probability = "1")
```

With the dataframe now containing everything that is needed, the logloss of the final model can now be calculated. 


```r
#Logloss for NNET model
EffectiveNNET <- EffectiveNNET %>%
  summarise(LogLoss = logLoss(Results,Probability),
            Errors = sum(((Probability < 0.0)+(Probability > 1.0)+(is.na(Probability)))))

print(EffectiveNNET)
```

```
##     LogLoss Errors
## 1 0.4921599      0
```
|        The nature of the logloss metric is to penalized predictions that had very high probabilities in the wrong direction. In other words, logloss penalizing overly confident models. For example, if the final model predicted that Team A had a 90% chance of beating Team B. Then Team A loss, the logloss would be greater than if the model said Team A only had a 60% of winning. Now the winning logloss values in the Kaggle competition in 2019 was around 0.45 or 0.46. However, this is partially due to one popular strategy of manually overriding the probabilies of certain games. This is typically done in the first or second round of the tournament in games that the model is clearly over confident in. Since the overall objective was to create a model that could predict the outcomes of the March Madness Tournament with the greatest accuracy, the logloss metric was not a priority when constructing and tuning the model. If the goal is to do as well as possible in the Kaggle competition, then the logloss should be minimized as much as possible.

# Conclusion

|        The objective of this report was to create a machine learning model that could predict the winning teams in the March Madness basketball tournament by only looking at the teams' statistics. Out of several machine learning models tested, the neural network model was found to be the model that performed the best. The most optimal parameters of the neural network were size = 1 and decay = 0.2. Overall, the created model was able to predict the outcomes 80.6% of the time with a logloss score of 0.492. This model will be adjusted and built upon for the 2021 March Madness Tournament since the 2020 March Madness Tournament was cancelled due to the Coronavirus outbreak. 
