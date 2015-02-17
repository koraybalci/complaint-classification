library(rjson)
library(stringr) # needed for counting words in sentence
library(beepr)

readGuidLookup <- function(filename) {
    lookup <- read.csv(filename, sep = ";", header = T)
}

processTableLog <- function (logname, victimId, suspectId, case) {
    if(file.exists(logname)) {
        conn=file(logname,open="r")
        lines=readLines(conn, warn = F)
        # traverse all lines in file
        for (i in 1:length(lines)){
            if(nchar(lines[i]) < 3) next()
            chatRec <- tryCatch({
                json <- fromJSON(lines[i], unexpected.escape = "skip" )
                json
            }, warning = function(w) {
                print(w)
                return(NULL)
            }, error = function(e) {
                print(e)
                return(NULL)
            }, finally = {
                
            })
            if(is.null(chatRec)) next()
            
            if (chatRec$act != "chat") next()
            
            # we have a chat line in chatRec
            if (chatRec$who == "server") next()
            else if (chatRec$who == "Çeker Atar") next()
            else if (is.na(chatRec$who)) next()
            else if (nchar(chatRec$who) < 5) next()
            
            #following line returns the list of locations of chatRec$who in guidLookup$Base64 list (but we know guidLookup is unique)
            foundUser <- seq_along(guidLookup$Base64)[sapply(guidLookup$Base64, FUN=function(X) chatRec$who %in% X)]
            
            # count words in chat sentence
            strm <- str_match_all( chatRec$obj, "\\S+" )  # Sequences of non-spaces
            wordCount <- length(strm[[1]])
            
            if(length(foundUser) == 0) {
                case$OtherChats <- case$OtherChats + 1
            } else {
                if(victimId == guidLookup$Id[foundUser[1]]) {
                    case$VictimChats <- case$VictimChats + 1
                    case$VictimWords <- case$VictimWords + wordCount
                }
                else if(suspectId == guidLookup$Id[foundUser[1]]) {
                    case$SuspectChats <- case$SuspectChats + 1
                    case$SuspectWords <- case$SuspectWords + wordCount
                }
            }
                
            #print(paste(guidLookup$Base64[foundUser[1]], " ", guidLookup$Id[foundUser[1]]))
        }
        close(conn)
        case
        
    } else {
        # print(paste(logname, " not found"))
        NULL
    }
}

processComplaint <- function(complaint) { 
    victimId <- complaint[3]
    suspectId <- complaint[4]
    case <- data.frame("ComplaintId" = complaint[1], "VictimChats" = 0, "SuspectChats" = 0, 
                       "OtherChats" = 0, "VictimWords" = 0, "SuspectWords" = 0,
                       row.names = "")
    
    tablelogs <- "D:\\Dropbox\\Tez\\R\\logs\\"
    # table 1
    logname <- paste(tablelogs, complaint[5], ".log", sep = "")
    table1 <- processTableLog(logname, victimId, suspectId, case)
    if(is.null(table1) == FALSE) case <- table1
    
    # table 2
    logname <- paste(tablelogs, complaint[6], ".log", sep = "")
    table2 <- processTableLog(logname, victimId, suspectId, case)
    if(is.null(table2) == FALSE) case <- table2
    
    # table 3
    logname <- paste(tablelogs, complaint[7], ".log", sep = "")
    table3 <- processTableLog(logname, victimId, suspectId, case)
    if(is.null(table3) == FALSE) case <- table3
    cases <<- rbind(cases, case)
}

guidLookup <- readGuidLookup("D:\\Dropbox\\Tez\\R\\ComplaintFeature-UserGuids.csv")
complaints <- read.csv("D:\\Dropbox\\Tez\\R\\Complaints2.csv", sep = ";" , header = T)

cases <- data.frame("ComplaintId" = character(), "VictimChats" = numeric(), "SuspectChats" = numeric(), "OtherChats" = numeric(),
                    "VictimWords" = numeric(), "SuspectWords" = numeric())

timing <- system.time(apply(complaints, 1, processComplaint))
print(timing)
write.csv2(cases, file = "D:\\Dropbox\\Tez\\R\\cases.csv")
beep()