

# Libraries ---------------------------------------------------------------

library(vroom)
library(dplyr)
library(corrplot)

# Reading In --------------------------------------------------------------

setwd("~/GitHub/Walmart-Recuiting")
Test <- vroom("test.csv")
Train <- vroom("train.csv")
Store_list <- vroom("stores.csv")
Features <- vroom("features.csv")


# 0 Method ----------------------------------------------------------------

Features[is.na(Features)] <- 0
Features$MarkdownTotal <- rowSums(Features[,c("MarkDown1", "MarkDown2",
                                              "MarkDown3","MarkDown4",
                                              "MarkDown5")])
Features$MarkdownFlag <- as.integer(!Features$MarkdownTotal == 0)
Features <- Features[, -c(5,6,7,8,9)]

joined_train <- left_join(
  Train,
  Features,
  by = c("Store", "Date", "IsHoliday")
)

joined_test <- left_join(
  Test,
  Features,
  by = c("Store", "Date", "IsHoliday")
)

head(joined_train) 
head(joined_test)

# Store Train -------------------------------------------------------------

colnames(Train)
colnames(Store_list)
colnames(Features)


Train$Size <- Store_list$Size[ match(Train$Store, Store_list$Store) ]
Train$Type <- Store_list$Type[ match(Train$Store, Store_list$Store) ]

unique(pull(Test,Store))
unique(pull(Train,Store))

table(pull(Train,Store))
table(pull(Test,Store))

head(Features)
sum(is.na(pull(Features,MarkDown1)))/length((pull(Features,MarkDown1)))

numeric_cols <- Features %>% select(where(is.numeric))
cor_matrix <- cor(numeric_cols, use = "complete.obs")
corrplot(cor_matrix)

