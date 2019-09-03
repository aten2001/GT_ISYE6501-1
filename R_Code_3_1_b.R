rm(list=ls())
library(data.table)
library(kknn)
set.seed(1)
ccdata <- read.table("C:/Users/ricar/Documents/GT_Data/credit_card_data-headers.txt", header = TRUE)
cols <- c("A1", "A9", "A10", "A12", "R1")
ccdata[cols] <- lapply(ccdata[cols], factor)
sample_size <- floor(0.75 * nrow(ccdata))
train_ind <- sample(seq_len(nrow(ccdata)), size = sample_size)
train <- ccdata[train_ind, ]
test <- ccdata[-train_ind, ]
knn <- train.kknn(R1 ~., data = train, kmax = 15, distance = 2, 
                  kernel = c("triangular", "rectangular", "epanechnikov", "optimal"),
                  scale = TRUE)
knn$best.parameters
cv = cv.kknn(knn, data = train, kcv = 10, scale = T)
cv = data.table(cv[[1]])
print('Cross Validation Accuracy')
print(table(cv$y == cv$yhat))
print(prop.table(table(cv$y == cv$yhat)))

#code found to split data by creating a vector and assigning to a variable
spec = c(train = .6, test = .2, validate = .2)

g = sample(cut(seq(nrow(ccdata)), nrow(ccdata)*cumsum(c(0,spec)), labels = names(spec)))

res = split(ccdata, g)
sapply(res, nrow)/nrow(ccdata)
addmargins(prop.table(table(g)))

train <- res$train
head(train)

validate <- res$validate
head(validate)

test <- res$test
head(test)

#ACCURACY of VALIDATION
#outer loop for printing k and validation accuracy score, inner loop for the model
for (k in 1:10){
  pred <- c()
  for (i in 1:nrow(validate)){
    knn <- kknn(R1 ~ .,train = train, test = validate[i, ], k = k, distance = 2, kernel = "rectangular", scale=TRUE)
    pred <- c(pred, knn$fitted.values)
    cat("\n pred = ", pred, "\n")   
  }
  #not sure what this command does other, perhaps remove the last row? 
  pred <- pred-1

  accuracy <- sum(pred == validate[,11]) / nrow(validate)
  cat("\n For K = ", k, "validation accuracy = ", accuracy, "\n")       
  
}
#clear pred
pred <- c()

#ACCURACY OF TEST
#loop to check test accuracy scores
for (i in 1:nrow(test)){
  knn <- kknn(R1 ~ .,train = train, test = test[i, ], k = 9, distance = 2, kernel = "rectangular", scale=TRUE)
  pred <- c(pred, knn$fitted.values)
}
#not sure what this command does other, perhaps remove the last row?  
pred <- pred-1
accuracy <- sum(pred == test[,11]) / nrow(test)
cat("\n For K = ", k, "test accuracy = ", accuracy, "\n")
  