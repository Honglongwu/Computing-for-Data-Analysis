corr <- function(directory, threshold = 0) {
  ## 'directory' is a character vector of length 1 indicating
  ## the location of the CSV files
  
  ## 'threshold' is a numeric vector of length 1 indicating the
  ## number of completely observed observations (on all
  ## variables) required to compute the correlation between
  ## nitrate and sulfate; the default is 0
  
  ## Return a numeric vector of correlations
  files <- list.files(directory)
  cors <- vector('numeric')
  for(file in files){
    filepath <- paste(c(strsplit(directory,'/'),file),collapse='/')
    data <- read.csv(filepath)
    nob <- sum(complete.cases(data))
    if(nob <= threshold) next
    cors <- c(cors, cor(data$sulfate,data$nitrate,use="complete.obs"))
  }
  return(cors)
}