library(ggplot2)
library(dplyr)

rossmann_train <- read.csv("/home/andrea/lavori/Data_Science/Data/Kaggle/Rossmann_Store_Sales/train.csv")
rossmann_store <- read.csv("/home/andrea/lavori/Data_Science/Data/Kaggle/Rossmann_Store_Sales/store.csv")
rossmann_test <- read.csv("/home/andrea/lavori/Data_Science/Data/Kaggle/Rossmann_Store_Sales/test.csv")


# A nice density plot
school_yes_den <- geom_density(fill = "blue", 
                         alpha = 0.5, 
                         aes(x=Sales), data=rossmann_train[rossmann_train$SchoolHoliday==1,])
school_no_den <-  geom_density(fill = "red", 
                         alpha = 0.5, 
                         aes(x=Sales), data=rossmann_train[rossmann_train$SchoolHoliday==0,])

den.plot <- ggplot() + school_yes_den + school_no_den

print(den.plot)