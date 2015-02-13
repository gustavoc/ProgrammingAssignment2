#########################################################################################
# R Programming - Week 3 - assignment 2
# Student: Gustavo Cavalcanti
# 2015-02-12
#________________________________________________________________________________________
#
# This file contains 2 functions: makeCacheMatrix and cacheSolve. 
#    >> makeCacheMatrix: Creates special matrix object that can cache its inverse.
#    >> cacheSolve: Matrix inverse through calling functions in the object created 
#                   from makeCacheMatrix
#########################################################################################


# Creates special matrix  object that can cache its inverse
makeCacheMatrix <- function(mat = matrix)
{
    m <- NULL
    set <- function(y) 
    {
        x <<- y
        m <<- NULL
    }

    get <- function() mat
    inverse <- function(solve) m <<- solve
    getInverse <- function() m
    list(set = set, get = get, inverse = inverse,getInverse = getInverse)
}

# Inverts a "cached matrix" by calling getInverse()
cacheSolve <- function(x, ...) 
{
    m <- x$getInverse()
    if(!is.null(m)) 
    {
        message("getting cached data")
        return(m)
    }
    data <- x$get()
    m <- solve(data, ...)
    x$inverse(m)
    m
}
