## This code has two functions "makeCacheMatrix" and "cacheSolve". 
## These are used together to read a matrix, compute its inverse and store it, 
## when run for the first time. 
## To get back values for the inverse of the matrix,repeated computation of 
## the inverse is not required,instead the stored inver value can be simply 
## called using these functions. This reduces computation time especially 
## when dealing with large matrices
## The first function takes a matrix as an argument. This function is 
## used for caching the inverse of a matrix. 

## This function takes in a matrix as an argument and returns a list of functions
## This function returns a list having four new functions: get(), set(),setin(), 
## getin(). The set function sets the value of the argument
## The get function is used to return the value of the original matrix (argument)
## The setin function sets the value of the inverse matrix. This is not used 
## independently as it uses a "superassignment" operator to set the 
## value of the inverse this assignment instead of actually calculating it.
## The get in function is used to return the inverse of a matrix. 

makeCacheMatrix <- function(x = matrix()) {
  i <- NULL
  set <- function(y) {
    x <<- y
    i <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) i <<- inverse
  getinverse <- function() i
  list(set=set, get=get,
       setin=setinverse,
       getin=getinverse)
}

## This function takes in a list of functions as an argument and computes the 
## inverse of the matrix under consideration. If the cacheSolve is operated on 
## the same matrix for the second time,
## it returns a value stored in the cache function using the getin() function. 
## If it is run for the first time on a given matrix, it reads the matrix using 
## the get() function, calculates the inverse, stores it makeCacheMatrix function 
## using setin and then returns the inverse value. By storing it in the cache function, 
## the inverse can be directly called from the cache memory instead of being 
## computed repeatedly.

cacheSolve <- function(x, ...) {
  i <- x$getin()
  if(!is.null(i)) {
    message("getting cached data")
    return(i) ## Return a matrix that is the inverse of 'x'
  }
  data <- x$get()
  i <- solve(data, ...)
  x$setin(i)
  i      ## Return a matrix that is the inverse of 'x'
}

x<-matrix(c(1,2,3,4),ncol=2,nrow=2)
y<-matrix(c(5,6,7,8),ncol=2,nrow=2)
a<-makeCacheMatrix(x)
b<-makeCacheMatrix(y)

cacheSolve(a)
cacheSolve(a)
cacheSolve(b)
cacheSolve(b)
cacheSolve(a)
cacheSolve(b)

## Solution ##
# > cacheSolve(a)
# [,1] [,2]
# [1,]   -2  1.5
# [2,]    1 -0.5
# > cacheSolve(a)
# getting cached data
# [,1] [,2]
# [1,]   -2  1.5
# [2,]    1 -0.5
# > cacheSolve(b)
# [,1] [,2]
# [1,]   -4  3.5
# [2,]    3 -2.5
# > cacheSolve(b)
# getting cached data
# [,1] [,2]
# [1,]   -4  3.5
# [2,]    3 -2.5
# > cacheSolve(a)
# getting cached data
# [,1] [,2]
# [1,]   -2  1.5
# [2,]    1 -0.5
# > cacheSolve(b)
# getting cached data
# [,1] [,2]
# [1,]   -4  3.5
# [2,]    3 -2.5