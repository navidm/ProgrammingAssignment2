## The purpose of writing the following functions is to cache costly
## calculations to save time. In these functions we try to define
## a matrix and cache its inverse for further use.

## The following function is the main function for this purpose
# it gets the matrix object and provide subset functions to set, get
# both matrix and its inverse. The variable "inverse" in this function
# is a global variable which keeps the inverse of the matrix.

makeCacheMatrix <- function(x = matrix()) {
    # each time this function is called, an environment will be
    # assigned to the matrix x (the output)
    inverse <- NULL
    print(environment())
    env <- environment()
    parent.env(env)
    set <- function(y) {
        x <<- y
        inverse <<- NULL
    }
    get <- function() x
    setinverse <- function(i) inverse <<- i
    print(environment())
    env <- environment()
    parent.env(env)
    getinverse <- function() inverse
    list(set = set, get = get,
         setinverse = setinverse,
         getinverse = getinverse)
}



## This function is a wraper for "makeCacheMatrix".
 # It checks and returns the inverse matrix if existed or
 # calculates it if it is not.
cacheSolve <- function(x, ...) {
        ## Since each time the makeCacheMatrix function is being
    # called the environment assigned to its output matrix changes,
    # the inverse matrix will be calculated for fresh matrices. In 
    # other words, since for defining a new matrix another call to
    # makeCacheMatrix function has to be made, we can be sure that 
    # inverse matrix will be calculated by cacheSolve function 
    # each time a new matrix is passed to it.
    print(environment())
    env <- environment()
    parent.env(env)
    inverse <- x$getinverse()
    if (!is.null(inverse)){
        print("getting cached data")
        return(inverse)}
        data <- x$get()
        inverse<- solve(data)
    print(environment())
    env <- environment()
    parent.env(env)
        x$setinverse(inverse)
        inverse
    
}
