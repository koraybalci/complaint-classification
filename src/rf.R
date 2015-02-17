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

rf <- function (training) {
  print("Random Forrest with 10-fold cv training, no center and scaling")
  set.seed(42423)
  
  fitControl <- trainControl(
    method = "cv",
    number = 10)
  
  modelFit <- train(training$Class ~., 
                    data = training, 
                    #preProcess=c("center","scale"), 
                    method = "rf",
                    trControl = fitControl)
  modelFit
}

data$training$Class <- as.factor(data$training$Class)
data$testing$Class <- as.factor(data$testing$Class)

system.time(modelRF <- rf(data$training))
RFcm <- confusionMatrix(data=predict(modelRF, newdata=data$testing), data$testing$Class, positive = "0")
beep()

#predictions <- predict(modelFit, newdata=data$testing)
#confusionMatrix(data=predictions, data$testing$Class)