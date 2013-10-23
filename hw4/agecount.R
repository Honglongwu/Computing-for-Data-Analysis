agecount <- function(age = NULL) {
  ## Check that "age" is non-NULL; else throw error
  if (is.null(age)) {stop('age is NULL')}
  age <- as.integer(age)
  ## Read "homicides.txt" data file
  homicides <- readLines('homicides.txt')
  
  ## Extract ages of victims; ignore records where no age is given
  # Filter out lines without age information
  homicides <- homicides[grep("<dd>.*[0-9]* *years old</dd>", homicides)]
  
  #extract age
  r <- regexpr("<dd>.*[0-9]* *years old</dd>",homicides)
  homicides.ages <- regmatches(homicides, r)
  homicides.ages <- gsub("<dd>[^0-9]*| *years old</dd>", "", homicides.ages)
  homicides.ages <- as.integer(homicides.ages)
  
  ## Return integer containing count of homicides for that age
  sum(homicides.ages == age)
}