## Inverse matrix
## Matrix inversion saves you from compute it repeatedly

## Below are a pair of functions that are used to store a matrix and caches its inverse

makeCacheMatrix <- function(x = matrix()) {
        inv<-NULL
        set<-function(y){
                x<<-y
                inv<<-NULL
        }
        get<-function() x
        setInverse<- function(inverse) inv<<-inverse
        getInverse<- function() inv
        list(set=set,
             get=get,
             setInverse=setInverse,
             getInverse=getInverse)
}


## This function perform the inverse of the matrix created by the first function. If the inverse was already calculated then 
## it returns the inverse from the cache

cacheSolve <- function(x, ...) {
        inv<-x$getInverse()
        if (!is.null(inv)) {
                message(" getting cached data")
                return(inv)
        }
        mat<-x$get()
        inv<-solve(mat,...)
        x$setInverse(inv)
        inv
}
