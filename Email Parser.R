######Emails Parser#######

require(stringr)
require(stats)

#read TH doc you want to clean
emailsdoc <- read.csv("C:/Users/John Johnson/Documents/TitanHouse/TH Data/Titan Data 3-7-18.csv", header = TRUE, stringsAsFactors = FALSE)
#View(emailsdoc)          ##view dataframe you just read from csv
emailsdoc[emailsdoc==""] <- NA #convert blanks to NA

#View(emailsdoc)

##create df to catch results of upcoming loop
addlrws <- as.data.frame(matrix(ncol = ncol(emailsdoc)))
colnames(addlrws) <- c(colnames(emailsdoc))



for (i in 1:(nrow(emailsdoc))) {
  if (grepl("|||", emailsdoc[i,16], fixed = TRUE) == FALSE){
    next
  }
  if (grepl("|||", emailsdoc[i,16], fixed = TRUE) == TRUE) #emails are in column 16 of this doc
    multi <- unlist(strsplit(emailsdoc[i,16], "|||", fixed = TRUE)) #multi is list of parsed emails
    emailsdoc[i,16] <- multi[1]
    for (j in 2:length(multi)){ #for as many elements as exist in multi
      addlrws[nrow(addlrws) + 1,] <- rbind(c(emailsdoc[i,1:15], #copy data from row prior to email column to new row
                                             multi[j], #parsed email in next column
                                             emailsdoc[i,17:ncol(emailsdoc)])) #copy data from row after email column
    
  }
}
View(emailsdoc)
View(addlrws)

#combine emailsdoc and addlrws df's
emailsdoc <- rbind(emailsdoc, addlrws)

###clean emails column by
###dropping email type tags, delineators, and whitespace from email column (where they exist)
###if NA, move to next
for (i in 1:nrow(emailsdoc)) {
  if (is.na(emailsdoc[i,16])) {
    next
  }
  
  if (grepl("(Work)", emailsdoc[i,16], fixed = TRUE) == TRUE) 
    emailsdoc[i,16] <- gsub("(Work)", "", emailsdoc[i,16], fixed = TRUE) #drop (Work) type tag
  else if (grepl("(Home)", emailsdoc[i,16], fixed = TRUE) == TRUE) 
    emailsdoc[i,16] <- gsub("(Home)", "", emailsdoc[i,16], fixed = TRUE) #drop (Home) type tag
  else if (grepl(" ", emailsdoc[i,16], fixed = TRUE) == TRUE) 
    emailsdoc[i,16] <- gsub(" ", "", emailsdoc[i,16], fixed = TRUE) #drop whitespace
}
#View(emailsdoc)

###write results to csv
###overwrite original file
###writing NA as blank
write.csv(emailsdoc, file = "C:/Users/John Johnson/Documents/TitanHouse/TH Data/Titan Data 3-7-18 for bounce testing.csv", na ="")

