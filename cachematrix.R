##################### 
## This program will create a function list to help with calculating, 
## caching and retrieving the cached value of inverse of the matrix 
## if one exists instead of recalculating it, every time same matrix is passed
#####################

## Create a list of functions to help with setting and retrieving values
## in cache
makeCacheMatrix <- function(x = matrix()) {
  
      m <- NULL
      set <- function(y) {
          x <<- y
          m <<- NULL
      }
      get <- function() x
      setinverse <- function(inverse) m <<- inverse
      getinverse <- function() m
      list(set=set, 
           get=get, 
           setinverse = setinverse, 
           getinverse = getinverse
           )
}

## This function checks if the inverse of passed matrix has already be calculated
## and stored in the cache.
## If it is stored in the cache, it retrieves the cached 'inverse' of the matrix
## If it is not stored in the cache, it calculates the inverse of the matrix using 'solve'
## and stored the calculated inverse in the cache
cacheSolve <- function(x, ...) {
      ## Get the inverse of 'x' from cache
      m <- x$getinverse()
      if (!is.null(m)){
          message("getting cached data")
          return(m)
      }
      ## The inverse is not in the cache, calculate and store the inverse in the cache
      data <- x$get()
      m <- solve(data, ...)
      x$setinverse(m)
      m
}
