library(caret)
library(klaR)
library(beepr)

source("loadFeatures.R")
data <- loadFeatures("../data/ComplaintData-ExtendedRaw2.csv", c("ComplaintId", "Severity", "VictimId", "SuspectId"
                                                     #, "Credit.Purchases..Suspect.", "Credit.Purchases..Victim." # near zero variance
                                                     # Communicative Features
                                                     #, "Victim.To.Suspect.Chats", "Suspect.To.Victim.Chats", "Suspect.To.Victim.Invites", "Victim.To.Suspect.PM", "Suspect.To.Victim.PM"
                                                     # Victim Data
                                                     #      , "Games.Played..Victim.", "Wins..Victim.", "Incomplete.Games..Victim.", "Rating..Victim.", "Gender..Victim.", "Social.Rewards..Victim.", "Gifts.Purchased..Victim.", "Friends..Victim.", "Private.Messages..Victim.", "Daily.Logins..Victim.", "Friendship.Requests..Victim.", "Avatar.Items..Victim.", "Tables.Entered..Victim.", "Chat.Entries..Victim.", "Bad.Lang.Attempts..Victim.", "Silence.Before.Chat..Victim.", "Silence.After.Chat..Victim.", "Words.in.Chat..Victim."
                                                     # Communicative
                                                     #      , "Victim.To.Suspect.Chats" , "Suspect.To.Victim.Chats", "Suspect.To.Victim.Invites", "Victim.To.Suspect.PM", "Suspect.To.Victim.PM"                                                     
))


nb <- function (training) {
    print("Naive Bayes with 10-fold cv training, no center and scaling")
    set.seed(42423)
    fitControl <- trainControl(
        method = "cv",
        number = 10)
    
    modelFit <- train(training$Class ~., 
                      data = training, 
                      preProcess="pca",
                      #preProcess=c("center","scale"), 
                      method = "nb",
                      trControl = fitControl)
    modelFit
}

data$training$Class <- as.factor(data$training$Class)
data$testing$Class <- as.factor(data$testing$Class)

modelNB <- nb(data$training)
NBcm <- confusionMatrix(data=predict(modelNB, newdata=data$testing), data$testing$Class, positive = "1")
beep()