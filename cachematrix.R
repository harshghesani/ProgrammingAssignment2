## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

##Caching function to cache matrix and inverse

makeCacheMatrix <- function(x = matrix()) {

  #Initialize inverse
    inv <- NULL
  #set original matrix value
    set <- function(y){
      x <<- y
      inv <<- NULL
    }
    #cache original matrix
    get <- function() x
    
    #set inverse to be later used to set the inverse that is computed
    setinverse <- function(inverse) inv <<- inverse
    #caching inverse of the matrix
    getinverse <- function() inv
    
    list(set = set, get = get,
         setinverse = setinverse,
         getinverse = getinverse)

}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
    #getting the inverse to verify if the value needs to be recalculated
    inv <- x$getinverse()
    #only check now will be to see if its null because for this problem matrices passed are assumed to be invertible
    if(!is.null(inv)){
      message("getting cached inverse")
      return(inv)
    }
    
    #get the matrix to be inverted
    data <- x$get()
    #invert the matrix
    inv <- solve(data,...)
    #set the inverse to be cached later
    x$setinverse(inv)
    inv
}
