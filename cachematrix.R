## These are two functions that cache the inverse of a matrix


## This first function creates a special matrix object that can cache it's inverse

makeCacheMatrix<-function(m=matrix()){
  ## This initialize the inverse property
  i<-NULL
  ## Method to set the matrix
  set <- function(matrix){
    m<<-matrix
    i<<-NULL
  }
  ## Method to get the matrix
  get <- function() {
    m
  }
  ## Method to set the inverse of the matrix
  setInverse <- function(inverse){
    i <<-inverse
  }
  ## Method to get the inverse of the matrix
  getInverse <- function() {
    i
  }
  ## Return a list of the methods
  list(set =set, get = get,
       setInverse = setInverse,
       getInverse = getInverse)
}



## Write a short comment describing this function
## This one computes the inverse of the special matrix returned by the first function
## If the inverse has already been calculated and has not changed, then this function
## should retrieve the inverse from the cache

cacheSolve <- function(x,...){
  ##Returns a matrix that is the inverse of 'x'
  m <- x$getInverse()
  ## Just returns the inverse if it is already set
  if(!is.null(m)){
    message("getting cached data")
    return(m)
  }
  ## Gets the matrix from our object
  data <- x$get()
  ## Computes the inverse matrix
  m <- solve(data)
  ## Set the inverse to the object
  x$setInverse(m)
  ## Returns the matrix
  m
}