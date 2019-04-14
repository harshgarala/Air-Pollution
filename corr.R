# Creating a function that takes a directory of data files and a threshold for complete cases 
# and calculates the correlation between sulfate and nitrate for monitor locations 
# where the number of completely observed cases (on all variables) is greater than the threshold. 
# 
# The function should return a vector of correlations for the monitors that meet the threshold requirement. 
# If no monitors meet the threshold requirement, then the function should return a numeric vector of length 0.



corr <- function(directory,threshold=0){
  
  y <- formatC(1:332,width=3,flag=0)
  l <- length(y)
  v <- numeric()
  
  for(i in 1:l){
    
    data <- paste0(directory,"/",y[i],".csv")
    # print(sum(complete.cases(read.csv(data))))
    # print(class(threshold))
    
    if(sum(complete.cases(read.csv(data)))>threshold){
      v <- c(v,cor(read.csv(data)$sulfate,read.csv(data)$nitrate,use = "complete.obs"))
    }
    
  }
  return(v)
}

cr <- corr("specdata", 2000)                
n <- length(cr)                
cr <- corr("specdata", 1000)                
cr <- sort(cr)
print(c(n, round(cr, 4)))

