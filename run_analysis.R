run_analysis <- function() {
  xtrain <- read.table("train/X_train.txt");
  subjectTrain <- readLines("train/subject_train.txt")
  trainActivities <- read.table("train/Y_train.txt")
  xtrain <- cbind(trainActivities, xtrain);
  xtrain <- cbind(subjectTrain, xtrain);
  
  xtest <- read.table("test/X_test.txt")
  subjectTest <- readLines("test/subject_test.txt")
  testActivities <- read.table("test/Y_test.txt");
  xtest <- cbind(testActivities, xtest);
  xtest <- cbind(subjectTest, xtest);
  
  
  #read the column names from features.txt
  colNames <- readLines("features.txt",561);
  colNames <- c("subject", "activity", colNames);   #because we added the subject column a few lines ago
  
  names(xtrain) <- colNames;
  names(xtest) <- colNames;
  
  allx <- rbind(xtrain, xtest)
  
  names(allx) <- gsub("[0-9]", "", names(allx))
  names(allx) <- gsub("[()]", "", names(allx))
  names(allx) <- gsub(" ", "", names(allx))
  names(allx) <- gsub("-", "_", names(allx))
  names(allx) <- gsub(",", "_", names(allx))
  
  #extract the ones with "mean" or "std" in the name
  colsToUse <- c(grep("mean", colNames), grep("std", colNames));
  colsToUse <- c(1, 2, colsToUse);
  
  tidyx <- allx[,colsToUse];
  write.csv(tidyx, "data_set_1.csv")
  
  tidy2 <- data.frame();
  for(i in 3:15) {
    print(paste(i, names(allx)[i], sep=""));
    current <- sqldf(paste('select subject, activity, avg(', names(allx)[i], ') as mean from allx group by subject, activity order by subject, activity'));
    tidy2 <- rbind(tidy2, current);
  }
  write.csv(tidy2, "data_set_2.csv");
}

removedCode <- function() {
  ytest <- read.table("test/Y_test.txt")
  ytrain <- read.table("train/Y_train.txt")
  ally <- rbind(ytrain, ytest)
  
  xmeans <- colMeans(allx);
  xsds <- apply(allx, 2, sd);
  ymeans <- mean(ally[,])
  yds <- sd(ally[,])
}