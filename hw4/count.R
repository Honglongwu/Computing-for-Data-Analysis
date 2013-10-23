count <- function(cause = NULL) {
  ## Check that "cause" is non-NULL; else throw error
  if (is.null(cause)) {stop('cause is NULL')}
  
  ## Check that specific "cause" is allowed; else throw error
  causes <- c('asphyxiation', 'blunt force', 'other',  'shooting', 'stabbing','unknown')
  if (!tolower(cause)%in%causes) {stop(paste('cause should be',paste(causes,collapse=', ')))}

  ## Read "homicides.txt" data file
  homicides <- readLines('homicides.txt')
  
  ## Extract causes of death
  # Filter out lines without cause
  homicides <- homicides[grep("<dd>[C|c]ause:", homicides)]
  
  # extract causes
  r <- regexpr("<dd>[C|c]ause:(.*?)</dd>", homicides)
  homicides.causes <- regmatches(homicides, r)
  homicides.causes <- gsub("<dd>[C|c]ause: *|</dd>", "", homicides.causes)
  homicides.causes <- tolower(homicides.causes)
  
  ## Return integer containing count of homicides for that cause
  length(homicides.causes[homicides.causes == tolower(cause)])
}