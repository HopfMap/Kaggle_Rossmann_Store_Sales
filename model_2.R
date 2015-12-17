# Model 2 (Neural Network)
# dataset: Rossmann Store Sales

# Libraries
library(caret)
library(ggplot2)

# Load datasets
rossmann_train <- read.csv("/home/andrea/lavori/Data_Science/Data/Kaggle/Rossmann_Store_Sales/train.csv")
rossmann_store <- read.csv("/home/andrea/lavori/Data_Science/Data/Kaggle/Rossmann_Store_Sales/store.csv")
rossmann_test <- read.csv("/home/andrea/lavori/Data_Science/Data/Kaggle/Rossmann_Store_Sales/test.csv")

# Evaluation function
RMSPE <- function(y,y_hat) {
  n <- length(y)
  s <- 0
  for (idx in 1:n) {
    if (y[idx] != 0) {
      s <- s+((y[idx]-y_hat[idx])/y[idx])^2}
  }
  
  return(sqrt(1/n*s))
}

# Model function
nn_model <- function(store,perc_train) {
  # Try to build a model for each single store
  
  store_dataset <- filter(rossmann_train,Store==store)
  
  store_dataset$BaselineDays <- unlist(lapply(store_dataset$Date,function(d) Sys.Date()-as.Date(d)))
  
  set.seed(1222)
  perc_train_set <- perc_train
  train_row_idx <- createDataPartition(store_dataset$Sales,
                                       p=perc_train_set,
                                       list = FALSE)
  
  train_set_store <- store_dataset[train_row_idx,]
  test_set_store <- store_dataset[-train_row_idx,]
  
  # Neural Network
  features <- c("DayOfWeek","Customers","Open","Promo","StateHoliday","SchoolHoliday","BaselineDays")
  f <- as.formula(paste("Sales ~", paste(features, collapse = " + ")))
  
  nn_model_store <- train(f,
                          data = train_set_store,
                          method = "neuralnet",
                          #preProc = c("center", "scale"),
                          tuneGrid = data.frame(layer1 = 2:8, layer2 = 0, layer3 = 0),
                          trControl=trainControl(method="cv",number=5,verboseIter = TRUE)
  )
  
  out_test_store <- predict(nn_model_store,test_set_store[features]) # y_hat
  
  print(RMSPE(test_set_store$Sales,out_test_store))
  
  return(nn_model_store)
}

nn_model_predict <- function(model,store,dataset) {
  dataset$BaselineDays <- unlist(lapply(dataset$Date,function(d) Sys.Date()-as.Date(d)))
  
  features <- c("DayOfWeek","Customers","Open","Promo","StateHoliday","SchoolHoliday","BaselineDays")
  
  out_store <- predict(model,dataset[features]) # y_hat
  
  print(RMSPE(dataset$Sales,out_store))
  
  return(list(RMSPE(dataset$Sales,out_store),out_store))
}

# A nice density plot
n_store <- 431
test_set_store_1 <- filter(rossmann_train,Store==n_store)
out_test_store_1 <- nn_model(n_store,0.8)

sales <- geom_density(fill = "blue", 
                      alpha = 0.5, 
                      aes(x=Sales), data=test_set_store_1)

out_test_store_1_df <- as.data.frame(out_test_store_1$finalModel$net.result)
colnames(out_test_store_1_df) <- c("Values")

pred_sales <- geom_density(fill = "red", 
                           alpha = 0.5,
                           aes(x=Values),data=out_test_store_1_df
)

den.plot <- ggplot() + sales + pred_sales

print(den.plot)