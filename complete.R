
# Creating function that reads a directory full of files and reports the number of completely observed cases in each data file. 
# The function will return a data frame where the first column is the name of the file and the second column is the number of complete cases.
# Directory is user defined path for the specdata folder, default value of id is (1:332) that is all the files

complete <- function(directory,id=1:332){
  
  y <- formatC(id,width=3,flag=0)
  l <- length(y)
  v <- integer(2)
  df <- data.frame(matrix(nrow=0,ncol=2))
  
  for(i in 1:l){
    data <- paste0(directory,"/",as.character(y[i]),".csv")
    v <- c(id[i],sum(complete.cases(read.csv(data))))
    df <- rbind(df,v)
  }
  colnames(df) <- c("Id","nobs")
  
  return(df)
  
}

complete("specdata",c(2,4,8,10,12))
set.seed(42)
cc <- complete("specdata", 332:1)
use <- sample(332, 10)
print(cc[use, "nobs"])

