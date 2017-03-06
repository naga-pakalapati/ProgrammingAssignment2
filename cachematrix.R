## The functionality of the first half of the below 2 funcitons is to take a square matirx as input 
## and calculate it's inverse and catch it. Second half checks if the inverse is already calculated  
## and catches it from first, else calculate the inverse and display it.


## makeCacheMatrix is a function that takes a square matrix as an input. We can set the inverse of this
## matirx using a sub function "setinv" and the values can be catched using "getinv". If we want to 
## provide a different input matrix, that can be set using "set" sub function and it value is catched
## using "get" sub function.

makeCacheMatrix <- function(x = matrix()) {
        m <- NULL
        set <- function(y){
                x <<- y
                m <<- NULL
        }
        get <- function() x
        
        setinv <- function(x) {
                m <<- solve(x)
        }
        getinv <- function() m
        
        list(set = set, get = get, setinv = setinv, getinv = getinv)
}


## cacheSolve takes function(x) as input, checks its subfuction "getinv" is not null and display the 
## result if so, along withth a message. If its null, it will take the output from another subfunction
## of x "get" and calculate its inverse.

cacheSolve <- function(x, ...) {
        m <- x$getinv()
        
        if(!is.null(m)){
                message("getting cached data")
                return(m)
        }
        
        data <- x$get()
        m <- solve(data)
        m
}