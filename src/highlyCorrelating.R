exclude <- names(data$training) %in% c("Gender", "VGender", "Class") # ignore factor vars
testData <- data$training[!exclude]
descrCor <- cor(testData)
highlyCorDescr <- findCorrelation(descCor, cutoff = 0.75)
colnames(testData)[highlyCorDescr]