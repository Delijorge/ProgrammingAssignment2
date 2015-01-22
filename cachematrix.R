## This is the Programming Assignment 2 of course "R Programming"


## First function allows to set and get the input matrix and set and 
## get its inverse

makeCacheMatrix <- function(x = matrix()) {
  size<-dim(x)
  if(size[1]!=size[2]){
    message("Inverse is only defined for square matrices!")
  } else{
    inv <- NULL
    set <- function(y) {
      x <<- y
      inv <<- NULL
    } 
    get <- function() x
    setinv <- function(inverse) inv <<- inverse
    getinv <- function() inv
    list(set = set, get = get,
         setinv = setinv,
         getinv = getinv)
  }
}


## This second function computes the inverse of the special 
## "matrix" returned by makeCacheMatrix above. If the inverse has 
##already been calculated (and the matrix has not changed), then the
##cachesolve should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
  
  inv<-x$getinv()
  if(!is.null(inv)){
    message("Getting cached data:")
    return(inv)
  }
  data<-x$get()
  inv<-solve(data,...)
  x$setinv(inv)
  inv
        ## Return a matrix that is the inverse of 'x'
}
