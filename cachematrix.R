## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

## sets up a list of functions that can have pull the cached matrix
## and inverse matrix
makeCacheMatrix <- function(x = matrix()) {
        ##sets up the cached invered matrix
        inversematrix <- NULL
        set <- function(y) {
                x <<- y
                inversematrix <<- NULL
        }
        ## allows you to retrieve the original matrix 
        get <- function() x
        ## pass in the solved matrix and then put that matix into the cache
        setsolve <- function(solve) inversematrix <<- solve
        ## allows you to retrieve the solved invese matrix
        getsolve <- function() inversematrix
        ## returns the list with all the functions in it 
        list(set = set, get = get,
             setsolve = setsolve,
             getsolve = getsolve)
}


## Checks to see if there is a solved matrix caches in the new list
## if that is not the case we solve the matrix and put into the cashed
## value m. if the matrix is already chached it lets you know that

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        inversematrix <- x$getsolve()
        ## If the inverse martix is cached pull it up
        
        if(!is.null(inversematrix)) {
                message("getting cached data")
                return(inversematrix)
        }
        ## otherwise pull the original matrix
        data <- x$get()
        ## solve it
        inversematrix <- solve(data, ...)
        ## put it into cache
        x$setsolve(inversematrix)
        ## spit it out
        inversematrix
}

