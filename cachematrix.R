## First, I create makeCacheMatrix and cacheSolve functions. After this, I tested my programming with an easy example.

## If you copy this code into R, it runs perfectly

## The following function creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) m <<- inverse
  getinverse <- function() m
  list(set=set,get=get,setinverse=setinverse,getinverse=getinverse)
}

## cacheSolve computes the inverse of the special "matrix" returned by makeCacheMatrix above. 

cacheSolve <- function(x, ...) {
m <- x$getinverse()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  matrix_inv <- x$get()
  m <- solve(matrix_inv, ...)
  x$setinverse(m)
  m
}

## Testing

a<-matrix(c(4, 2, 7, 6),nrow=2,ncol=2)
b<-makeCacheMatrix(a)
cacheSolve(b)
