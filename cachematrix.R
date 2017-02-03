## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
  
  invmtr <- NULL #initialze an object 
  
  setmtr <- function(y){
    x <<- y #assign input y to be the x object in parent environment
    invmtr <<- NULL #assign NULL to the invmtr object in parent environment
  }
  getmtr <- function() x #retrive x from parent environment
  setinvr <- function(solve) invmtr <<- solve #assign inverse matrics to invmtr in parent environment
  getinvr <- function() invmtr #retrive object invmtr
  list(setmtr = setmtr, getmtr = getmtr, setinvr=setinvr, getinvr=getinvr)
  # setmtr: name of the setmtr() function
  # getmtr: name of the getmtr() function
  # setinvr: name of the setinvr() function
  # getinvr: name of the getinvr() function
}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  invmtr <- x$getinvr() #assign input matrix to invmtr
  if(!is.null(invmtr)){
    message("Getting cached data") # if inverse matrix has been calculated
    return(invmtr)
  }
  data <- x$getmtr() #get matrix 
  invmtr <- solve(data) # invert matrix
  x$setinvr(invmtr)
  invmtr
}