loadFeatures <- function (filename, excludes, split = T, scale = F) {
    trainData = read.csv(filename, header = TRUE, sep=";")
    
    if("Gender..Suspect." %in% colnames(trainData)) {
        trainData$Gender..Suspect. <- as.factor(trainData$Gender..Suspect.)    
        levels(trainData$Gender..Suspect.) <- list(female="0", male="1") 
    }

    if("Gender..Victim." %in% colnames(trainData)) {
        trainData$Gender..Victim. <- as.factor(trainData$Gender..Victim.)
        levels(trainData$Gender..Victim.) <- list(female="0", male="1") 
    }
    
    exclude <- names(trainData) %in% excludes 
    data <- trainData[!exclude]
    data$Class <- as.numeric(data$Class)
#    levels(data$Class) <- list(innocent="0", guilty="1")
    if(split == F) {
         data
    } else {
        set.seed(42422)
        inTrain <- createDataPartition(y=data$Class, p=0.75, list = FALSE)
        training <- data[inTrain,]
        testing <- data[-inTrain,]
        
        if (scale == T) {
            for(i in 1:nrow(training)) {
                training[i, ] <- as.numeric(training[i,])
            }
                
            maxs <- apply(training, 2, max)    
            mins <- apply(training, 2, min)
            scaledTraining <- scale(training, center = mins, scale = maxs - mins)
            scaledTesting <- scale(testing, center = mins, scale = maxs - mins)
            
            result <- list("training" = as.data.frame(scaledTraining), "testing" = as.data.frame(scaledTesting))
        } else {        
            result <- list("training" = training, "testing" = testing)
        }
        result
    }
}