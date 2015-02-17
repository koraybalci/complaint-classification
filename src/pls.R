library(pls)
library(caret)
library(beepr)

source("loadFeatures.R")
data <- loadFeatures("../data/ComplaintData-ExtendedRaw.csv", c("ComplaintId", "Severity", "VictimId", "SuspectId"
                                                     #, "Credit.Purchases..Suspect.", "Credit.Purchases..Victim." # near zero variance
                                                     # Communicative Features
                                                     #, "Victim.To.Suspect.Chats", "Suspect.To.Victim.Chats", "Suspect.To.Victim.Invites", "Victim.To.Suspect.PM", "Suspect.To.Victim.PM"
                                                     # Victim Data
                                                     #      , "Games.Played..Victim.", "Wins..Victim.", "Incomplete.Games..Victim.", "Rating..Victim.", "Gender..Victim.", "Social.Rewards..Victim.", "Gifts.Purchased..Victim.", "Friends..Victim.", "Private.Messages..Victim.", "Daily.Logins..Victim.", "Friendship.Requests..Victim.", "Avatar.Items..Victim.", "Tables.Entered..Victim.", "Chat.Entries..Victim.", "Bad.Lang.Attempts..Victim.", "Silence.Before.Chat..Victim.", "Silence.After.Chat..Victim.", "Words.in.Chat..Victim."
                                                     # Communicative
                                                     #      , "Victim.To.Suspect.Chats" , "Suspect.To.Victim.Chats", "Suspect.To.Victim.Invites", "Victim.To.Suspect.PM", "Suspect.To.Victim.PM"                                                     
))

do_pls <- function (training) {
    print("partial least squares discriminant analysis (PLSDA) with 10-fold cv training, center and scaling")
    set.seed(42423)
    
    fitControl <- trainControl(
        method = "cv",
        number = 10)
        
    modelFit <- train(training$Class ~ ., 
                      data = training,
                      preProcess=c("center","scale"), 
                      trControl = fitControl,
                      method="pls")
    modelFit
}

data$training$Class <- as.factor(data$training$Class)
data$testing$Class <- as.factor(data$testing$Class)

modelPLS <- do_pls(data$training)
PLScm <- confusionMatrix(data=predict(modelPLS, newdata=data$testing), data$testing$Class, positive = "0")
beep()