## Put comments here that give an overall description of what your
## functions do  create and solve the inverse if a matrix,

## Write a short comment describing this function
##  this function create a matrix what is not inverted

makecachematrix <- function(x = matrix()){
  inv <- NULL
  set <- function(y){
    x <<- y
    inv <<- NULL
  }
  get <- function(){x}
  inverse_set <- function(inverse) {inv <<- inverse}
  get_inverse <- function(){inv}
  list(set = set, get = get, inverse_set = inverse_set, get_inverse = get_inverse)
}

## Write a short comment describing this function
## function cache solve gets the matrix create with the previus function and trow his inverse solution 

cachesolve <- function(x,...){
  inv <- x$get_inverse()
  if(!is.null(inv)){
    message("getting cached data")
    return(inv)
  }
  mat <- x$get()
  inv <- solve(mat,...)
  x$inverse_set(inv)
  inv
}
