library(tm)
library(tm.corpus.Reuters21578)
library(SnowballC)
library(slam)

init_data2000 <- function() 
{
  
  data(Reuters21578)
  # We can use arbitrary character processing functions as transformations as long as
  # the function returns a text
  # document.  In this case we use content_transformer()
  # which provides a convenience wrapper to access and
  # set the content of a document.  Consequently most text manipulation functions from base
  # R can directly be used with this wrapper. 
  reut2000 <- tm_map(Reuters21578[1:2000], content_transformer(tolower))
  reut2000 <- tm_map(reut2000, removeWords, stopwords("english"))
  reut2000 <- tm_map(reut2000, stemDocument)
  reut2000 <- tm_map(reut2000, removePunctuation)
  reut2000 <- tm_map(reut2000, removeNumbers)
  reut2000 <<- tm_map(reut2000, stripWhitespace)
  
  tdm2000 <<- TermDocumentMatrix(reut2000)
  N <- length(colnames(tdm2000))
  NW <- slam::row_sums(tdm2000, na.rm=TRUE)
  # Inverse document frequency
  IDF2000 <<- log(N/NW) 
}

init_data <- function()
{
  
  data(Reuters21578)
  # We can use arbitrary character processing functions as transformations as long as
  # the function returns a text
  # document.  In this case we use content_transformer()
  # which provides a convenience wrapper to access and
  # set the content of a document.  Consequently most text manipulation functions from base
  # R can directly be used with this wrapper. 
  reuters <- tm_map(Reuters21578, content_transformer(tolower))
  reuters <- tm_map(reuters, removeWords, stopwords("english"))
  reuters <- tm_map(reuters, stemDocument)
  reuters <- tm_map(reuters, removePunctuation)
  reuters <- tm_map(reuters, removeNumbers)
  reuters <<- tm_map(reuters, stripWhitespace)
  
  tdm <<- TermDocumentMatrix(reuters)
  N <- length(colnames(tdm))
  NW <- slam::row_sums(tdm, na.rm=TRUE)
  # Inverse document frequency
  IDF <<- log(N/NW) 
}

query <- function(words, k, idf, TDM, normalize=FALSE, apply.idf=FALSE)
{
  # Create bag-of-words vector for query q
  q <- idf
  q[1:length(q)] <- 0
  q[words] <- 1
  q <- as.matrix(q, ncol=1)
  
  if(normalize) {
    q <- normalize_vector(q)
    idf <- apply(idf, 2, normalize_vector)
  }
  
  # Calculate similarity
  nr <- norm(q, "f")
  if(apply.idf) {
    ej <- as.matrix(idf,ncol=1)
    sim <- apply(TDM, 2, function(x) { return(t(q)%*%(x*ej)/(nr*norm(as.matrix(x)*ej, "f"))) })
  }
  else {
    sim <- apply(TDM, 2, function(x) { return(t(q)%*%x/(nr*norm(as.matrix(x), "f"))) })
  }
  #Find indexes of articles with highest similarity
  best <- c()
  l <- k
  while(l > 0 && length(best) < k) {
    i <- which.max(sim)
    best <- append(best, i)
    sim <- sim[-i]
    
    l <- l-1
  }
  # Returns indexes of matched articles
  return(best)
}

normalize_vector <- function(x)
{
  sm <- sum(x, na.rm=TRUE)
  x <- x/sm
  return(x)
}

apply_svd <- function(TDM, k)
{
  svd <- svd(TDM, nu=k, nv=k)
  TDM <- svd$u %*% diag(svd$d[1:k]) %*% t(svd$v)
  N <- length(colnames(TDM))
  NW <- slam::row_sums(TDM, na.rm=TRUE)
  # Inverse document frequency
  IDF <<- log(N/NW) 
  
}

# writeLines(as.character(Reuters21578[[1]])) Print 1st article