## Computing the inverse of a square matrix

## This function creates a special "matrix" object 
## that caches its inverse

makeCacheMatrix <- function(x = matrix()) {
      inv=NULL
      set <- function(y){
            x<<-y
            inv<<-NULL
      }
      get <- function(){x}
      setInverse <- function(inverse){inv <<- inverse}
      getInverse <- function(){inv}
      list(set=set,get=get,setInverse=setInverse, getInverse=getInverse)
}


## Return a matrix that is the inverse of 'x'

cacheSolve <- function(x, ...) {
      inv <- x$getInverse()
      if(!is.null(inv)){
            message("Getting cache data")
            return(inv)
      }
      mat <- x$get()
      inv <- solve(mat, ...)
      x$setInverse(inv)
      inv
}


# examples
adat <- makeCacheMatrix(matrix(3:6,nrow =2,ncol = 2))
adat$get()
adat$getInverse()
cacheSolve(adat)