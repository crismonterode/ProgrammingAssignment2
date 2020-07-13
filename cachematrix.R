##Asigment 2 coursera

##makeCacheMatrix will create a matrix, then its inverse, and it will cache its inverse value.

makeCacheMatrix <- function(x = matrix()) {
  #set the value of the matrix
    i <- NULL
    set <- function(y) {
      x <<- y
      i <<- NULL
    }
    #get the value of the matrix
    get <- function() x
    #set the value of the inverse
    setinverse <- function(inverse) i <<- inverse
    #get the value of the inverse
    getinverse <- function() i
    list(set = set,
         get = get,
         setinverse = setinverse,
         getinverse = getinverse)
}


##cacheSolve computes the inverse of the matrix obtained by the previous function (makeCacheMatrix). 

cacheSolve <- function(x, ...) {
  i <- x$getinverse()
  if (!is.null(i)) {
    message("getting cached data")
    return(i)
  }
  data <- x$get()
  i <- solve(data, ...)
  x$setinverse(i)
  i
  
}
