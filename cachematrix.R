## The two functions are very similar to the ones in the example. The first one creates a list containing functions to perform with the matrix. The second one executes the functions depending on whether or not it has already been executed and stored in the cache.

# The function makeCacheMatrix creates a list containing functions to
# set the value of the vector
# get the value of the vector
# set the value of the mean
# get the value of the mean

makeCacheMatrix <- function(x = matrix()) {
  i <- NULL
  set <- function(y) {
    x <<- y
    i <<- NULL
  }
  get <- function() x
  setinverse <- function(solve) i <<- solve
  getinverse <- function() i
  list(set = set, 
       get = get,
       setinverse = setinverse,
       getinverse = getinverse)

}


# The function cacheSolve calculates the inverse of the matrix in the list created with makeCacheMatrix. However, it first checks to see if the inverse has already been calculated. If so, it gets the inverse from the cache and skips the computation. Otherwise, it calculates the inverse of the data and sets the value of the inverse in the cache via the setinverse function.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        i <- x$getinverse()
        if(!is.null(i)) {
          message("getting cached data")
          return(i)
        }
        data <- x$get()
        i <- solve(data,...)
        x$setinverse(i)
        i

}
