require(caret)
#require(pls)
require(gbm)
require(plyr)
require(parallel)
require(lattice)
require(ggplot2)
library(beepr)

source("loadFeatures.R")
data <- loadFeatures("../data/ComplaintData-ExtendedRaw2.csv", c("ComplaintId", "Severity", "VictimId", "SuspectId"
                 , "Credit.Purchases..Suspect." , "Credit.Purchases..Victim." # near zero variance                 
                 # Communicative Features
                 #, "Victim.To.Suspect.Chats", "Suspect.To.Victim.Chats", "Suspect.To.Victim.Invites", "Victim.To.Suspect.PM", "Suspect.To.Victim.PM"                 
                 # Victim Data
                 #      , "Games.Played..Victim.", "Wins..Victim.", "Incomplete.Games..Victim.", "Rating..Victim.", "Gender..Victim.", "Social.Rewards..Victim.", "Gifts.Purchased..Victim.", "Friends..Victim.", "Private.Messages..Victim.", "Daily.Logins..Victim.", "Friendship.Requests..Victim.", "Avatar.Items..Victim.", "Tables.Entered..Victim.", "Chat.Entries..Victim.", "Bad.Lang.Attempts..Victim.", "Silence.Before.Chat..Victim.", "Silence.After.Chat..Victim.", "Words.in.Chat..Victim."
                 # Communicative
                 #      , "Victim.To.Suspect.Chats" , "Suspect.To.Victim.Chats", "Suspect.To.Victim.Invites", "Victim.To.Suspect.PM", "Suspect.To.Victim.PM"
                                                     
), scale = F)

do_gbm <- function (training) {
    print("Stochastic Gradient Boosting (gbm)")
    set.seed(42423)
  
#     ctrl <- trainControl(method = "repeatedcv",
#                        allowParallel = T,
#                        repeats = 10,
#                        number = 10,                       
#                        classProbs = T,
#                        summaryFunction = twoClassSummary
#                        )

#   ctrl <- trainControl(
#     allowParallel = T,
#     classProbs = T,
#     summaryFunction = twoClassSummary,
#     method = "cv",
#     number = 10)

#     gbmGrid <-  expand.grid(interaction.depth = 3,
#                           n.trees = 400,
#                           shrinkage = 0.01)

#     modelFit <- train(training$Class ~ .,
#                    data = training,
#                    method = "gbm",
#                    trControl = ctrl,
#                    #preProcess=c("center","scale"),
#                    metric = "ROC",
#                    tuneGrid = gbmGrid,
#                    verbose = FALSE)
#     modelFit
    gbmFit <- gbm(training$Class ~ ., 
                  data = training,
            distribution="bernoulli",
            n.trees=10000, # number of trees
            shrinkage=0.001, 
            interaction.depth=3, 
            bag.fraction = 0.5,
            train.fraction = 1,
            n.minobsinnode = 10, 
            cv.folds = 5, 
            keep.data=FALSE, 
            verbose=FALSE
            )
    gbmFit
}

#data$training$Class <- as.factor(data$training$Class)
#data$testing$Class <- as.factor(data$testing$Class)

modelGBM <- do_gbm(data$training)
best.iter <- gbm.perf(modelGBM, method="cv")

resTraining <- confusionMatrix(data=predict(modelGBM, newdata=data$training, best.iter) <= 0, data$training$Class <= 0)
resHoldOut <- confusionMatrix(data=predict(modelGBM, newdata=data$testing, best.iter) <= 0, data$testing$Class <= 0)
#summary.gbm(modelGBM, n.trees=best.iter, cBars=10)

#require(ROCR)
#pred <- prediction(predict(modelGBM, newdata=data$training, best.iter), data$training$Class)
#perf <- performance(pred, measure = "tpr", x.measure = "fpr") 
#plot(perf, col=rainbow(10))

print.gbm(modelGBM)
beep()
