## Put comments here that give an overall description of what your
## functions do

# I preferred to put the calculation of the inverse matrix directly in the makeCacheMatrix
# so this could be an independent object that can calculate its own inverse without cacheSolve.
# 
# makeCacheMatrix is a object that contain a matrix, and can calculate and store its inverse
# 
# cacheSolve simply calls the functions defined in makeCacheMatrix to help you use this object
# You can make it calculate the inverse (and obviously put it in the cache), or you can give it
# an inverse matrix you already know so it will just put this matrix in the cache
# Finally, you can recalculate an inverse matrix that is already in the cache if you think
# that it was wrong

##############################################################################################################
##############################################################################################################
##############################################################################################################

## Write a short comment describing this function
# 
# to build the object you can call it as M <- makeCacheMatrix(x) where x is a matrix.
# If called without argument, by default it will be x = matrix()
#
# methods:
#
# M$set(x)             change the matrix stored in the object (it will reset the inverse to NULL)
#                      x must be a squared invertible matrix
# M$get()              return the matrix
# M$calculateInverse() calculate the inverse matrix and stores it in the cache
# M$setcache(inverse)  put the inverse in the cache.
#                      inverse must be a squared matrix inverse of x, that is
#                      inverse %*% x = x %*% inverse = I (I identity matrix)
# M$getcache()         return the matrix stored in the cache, that is supposed to be the inverse of x

makeCacheMatrix <- function(x = matrix()) {
        cache <- NULL
        set <- function(y) {
                x <<- y
                cache <<- NULL
        }
        get <- function() x
        calculateInverse <- function() cache <<- solve(x)
        setcache <- function(inverse) cache <<- inverse
        getcache <- function() cache
        list(set = set, get = get,
             calculateInverse = calculateInverse,
             setcache = setcache,
             getcache = getcache)
}


## Write a short comment describing this function
#
# This function will help you to use the cache of makeCacheMatrix
# With the default call cacheSolve(x) where x is a makeCacheMatrix object,
# it will give you the inverse of the matrix in x, calculating it or getting it from the cache
#
# arguments:
#
# x           (mandatory)  a makeCacheMatrix object
# inverse     (= NULL)     you can give an inverse, if you already know it, to store in the cache
#                          this parameter will overwrite the inverse in the cache if there is one
# recalculate (= FALSE)    recalculate the inverse and overwrite the cache
#                          useful if you think that the inverse stored in the cache is incorrect

cacheSolve <- function(x, inverse = NULL, recalculate = FALSE, ...) {
        ## Return a matrix that is the inverse of 'x'
        cache <- x$getcache()
        if(!is.null(cache) && is.null(inverse) && !recalculate) {
                message("getting cached data")
                return(cache)
        }
        if(!is.null(inverse)){
                message("setting your inverse in the cache")
                x$setcache(inverse)
                return(inverse)
        }
        if(is.null(inverse)){
                message("calculating inverse")
                x$calculateInverse()
                return(x$getcache())
        }
}
