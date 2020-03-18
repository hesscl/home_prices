library(tidyverse)
library(caret)
library(vroom)

#set working directory to repo
setwd("~/Desktop/home_prices")

#load in the kaggle data
train <- vroom("./input/train.csv")
test <- vroom("./input/test.csv")

#set seed for samping
set.seed(20200318)

#GOAL: predict SalePrice with regression model

#### A. Prepare data for modeling ---------------------------------------------

glimpse(train)
summary(train) #some NAs but not many

## First, let's deal with the NAs

#inclined to drop PoolQC, MiscFeature and Alley given low variation/high missing
#convert Fence to make NA into a valid value for no fences
#simplify FireplaceQu into indicators for masonry (Ex/Gd) and prefab fireplaces
#convert NA -> 0 for LotFrontage to remove NAs

#function to munge a given table, return clean table
munge <- function(tbl){
  tbl <- tbl %>%
    mutate(MasonFireplace = FireplaceQu %in% c("Ex", "Gd"),
           PrefabFireplace = FireplaceQu %in% c("TA", "Fa", "Po"),
           LotFrontage = if_else(is.na(LotFrontage), 0, LotFrontage)) %>%
    select(-PoolQC, -MiscFeature, -Alley, -FireplaceQu) %>%
    mutate_if(is.character, function(x){fct_lump_min(x, 20)})
  
  tbl
}

#run tables through the function
train <- munge(train)
test <- munge(test)

## Now, let's consider feature transformations




#### B. Setup training and validation data ------------------------------------

#impute missing values using knn
train.mis.model = preProcess(train %>% select(-SalePrice, -Id), 
                             "knnImpute")
train = predict(train.mis.model, train)
test = predict(train.mis.model, test)

#index the training data so 75% is used for model training, 25% for validation
training_data_index <- createDataPartition(train$Id, 
                                           p = 0.75, list = FALSE)

#split the training data into two parts for model fitting
training_data <- train[training_data_index,]
validation_data <- train[-training_data_index,]

#parameters for the model fitting
training_parameters <- trainControl(method = "repeatedcv", 
                                    number = 10, repeats = 10)

#### C. Train model -----------------------------------------------------------

lm_model <- train(SalePrice ~ .,
                  training_data %>% select(-Id),
                  method = "lm",
                  na.action = na.omit)

#### D. Evaluate model accuracy with validation data --------------------------

lm_model

validation_data$lm_pred <- predict(lm_model, validation_data)



