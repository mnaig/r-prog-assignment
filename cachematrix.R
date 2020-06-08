#function - makeCacheMatrix creates a special "matrix" object that can cache its inverse.
makeCacheMatrix <- function(x = matrix()) {
  i <- NULL
  set <- function(y) {
    x <<- y
    i <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) i <<- inverse
  getinverse <- function() i
  list(set = set,
       get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}

#function - cacheSolve computes the inverse of the special "matrix" returned by makeCacheMatrix above
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

######################
#Testing the function

x <- matrix(c(1:4),2) #creating a 2x2 matrix
print(x)

##     [,1] [,2]
##[1,]    1    3
##[2,]    2    4


#calling the function
a <- makeCacheMatrix(x)
cacheSolve(a)

##     [,1] [,2]
##[1,]   -2  1.5
##[2,]    1 -0.5
