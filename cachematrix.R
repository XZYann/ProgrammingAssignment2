## Put comments here that give an overall description of what your
## functions do

## makeCacheMatrix creates a special "matrix", which is really a list containing a function 
## to
## set the value of the matrix
## get the value of the matrix
## set the value of the inverse
## get the value of the inverse

makeCacheMatrix <- function(x = matrix()) {
    i<-NULL
    set<-function(y){
        x<<-y
        i<<-NULL
    }
    get<-function()x
    setmatrix<-function(inverse)i<<-inverse
    getmatrix<-function()i
    list(set=set,get=get,
         setmatrix=setmatrix,
         getmatrix=getmatrix)
}


## The following function calculates the inverse of the special "matrix" created with the above
## function. However, it first checks to see if the inverse has already been calculated. If so,
## it gets the inverse from the cache and skips the computation. Otherwise, it calculates the 
## inverse of the matrix and sets the inverse in the cache via the setinverse function.

cacheSolve <- function(x, ...) {
    i<-x$getmatrix()
    if(!is.null(i)){
        message("getting the cache data")
        return(i)
    }
    data<-x$get()
    i<-solve(data,...)
    x$setmatrix(i)
    i
}
