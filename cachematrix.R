## Put comments here that give an overall description of what your
## functions do
## 
## The makeCacheMatrix function will make a Matrix and stored cached it
## The makeCacheMatrix function contains a list of four function
## to help the user get access to the cached matrix
## 1. makeCactheMatrix$get(): get the value of the matrix
## 2. makeCactheMatrix$set(): set the value of the matrix
## 3. makeCactheMatrix$getinverse(): get the inverse of the matrix
## 4. makeCactheMatrix$setinverse(): set the inverse of the matrix

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
    m <- NULL
    set <- function(y) {
        x <<- y
        m <<- NULL
    }
    get <- function() x
    setinverse <- function(solve) m <<- solve
    getinverse <- function() m
    list(set = set, get = get,
         setinverse = setinverse,
         getinverse = getinverse)

}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
    m <- x$getinvers()
    if(!is.null(m)) {
        message("getting cached data")
        return(m)
    }
    data <- x$get()
    m <- solve(data, ...)
    x$setinverse(m)
    m
}

