rm(list=ls())
library(RCurl)
library(randomForest)
library(caret)
library(grDevices)

if (!file.exists("pml-training.csv")) {
  download.file("http://d396qusza40orc.cloudfront.net/predmachlearn/pml-training.csv", destfile = "pml-training.csv")
}
if (!file.exists("pml-testing.csv")) {
  download.file("http://d396qusza40orc.cloudfront.net/predmachlearn/pml-testing.csv", destfile = "pml-testing.csv")
}
training <- read.csv("pml-training.csv", na.strings=c("NA", ""))
testing <- read.csv("pml-testing.csv", na.strings=c("NA", ""))

# the size of data
#            rows  cols
# training: 19622   160
# testing:     20   160

# number of complete cases (no missing data): 406
# sum(complete.cases(training))

# filter out fields with NA values
t <- data.frame(colSums(!is.na(training)))
names(t) <- c('num')
t$field <- rownames(t)
fieldset <- subset(t, num == 19622, c(field))[,c('field')]
rm(t)

# removing unnecessary fields
# X is a counter, user_name, timestamps and windows seems to be only administrative data
# classe is the output variable but it is not available in test dataset
unnecessary <- c('X', 'classe', 'user_name', 'raw_timestamp_part_1', 
                 'raw_timestamp_part_2', 'cvtd_timestamp', 'new_window', 'num_window')
fieldset <- setdiff(fieldset, unnecessary)

filtered.testing <- testing[, fieldset]
# put back 'classe' as the uotput variable
fieldset <- c(fieldset, "classe")
filtered.training <- training[, fieldset]

set.seed(23716)

# built your model
# model <- randomForest(classe ~ ., data = filtered.training, ntree = 100)
model <- train(classe ~ ., data=filtered.training, method="rf", ntree=100)

# predict 20 different test cases 
predict(model, filtered.testing)

rm(list=ls())
