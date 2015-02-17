library(kernlab)
library(caret)
library(randomForest)
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
))

knn <- function (training) {
  print("kNN with Bootstrapped resampling, with center and scaling")
  set.seed(42423)
  
  bootControl <- trainControl(number = 5)
  knnGrid <- expand.grid(.k=c(2:10))
  
  modelFit <- train(training$Class ~., 
                    data = training, 
                    preProcess=c("center","scale"), 
                    method = "knn",
                    trControl = bootControl,
                    tuneGrid = knnGrid)
  modelFit
}

data$training$Class <- as.factor(data$training$Class)
data$testing$Class <- as.factor(data$testing$Class)

modelKNN <- knn(data$training)
KNNcm <- confusionMatrix(data=predict(modelKNN, newdata=data$testing), data$testing$Class, positive = "0")
beep()
