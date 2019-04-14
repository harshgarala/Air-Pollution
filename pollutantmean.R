getwd()

# Creating function to calculate the mean of the user defined pollutant considering user defined id
# Pollutant can be either "sulfate" or "nitrate"
# The default value of id is set to all files(i.e 1:332)


pollutantmean <- function(directory,pollutant,id=1:332){
  
  y <- formatC(id,width=3,flag=0) # As the file names are in 3 digits format
  l <- length(y)
  total <- 0
  rows <- 0
  for(i in 1:l){
    data <- paste0(directory,"/",as.character(y[i]),".csv")
    
    total <- total + sum(read.csv(data)[,pollutant],na.rm = T)
    rows <- rows + nrow(read.csv(data)) - sum(is.na(read.csv(data)[,pollutant]))
    
  }
  total/rows
}
