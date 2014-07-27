## Put comments here that give an overall description of what your
## makeCacheMatrix
## - get/set a matrix, get/set the inverse
makeCacheMatrix <- function(x = matrix()) {
  i <- NULL
  set <- function(y){
    x <<- y
    i <<- NULL
  }
  get <- function() x
  set_inverse <- function(inv) i <<- inv
  get_inverse <- function() i
  list(set = set, get = get, set_inverse = set_inverse, get_inverse = get_inverse)
}


## Write a short comment describing this function
## cacheSolve
## - return an inverse of the matrix
## - if it is previously computed a cached version is returned
cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  inv <- x$get_inverse()
  if(!is.null(inv)){
    message("getting cached inverse")
    return(inv)
  }
  data <- x$get()
  inv <- solve(data, ...)
  
  x$set_inverse(inv)
  
  inv
}

#testCacheMatrix <- function(){
#  #test the init
#  original_raw <- matrix(rnorm(16), nrow = 4) 
#  original_cachable <- makeCacheMatrix(original_raw)
#  
#  #test get (should be true)
#  if(all.equal(original_raw, original_cachable$get())){
#    message("the original and returned from get are the same")
#  }
#
#  #generate another one, assert it's not the same
#  generated_again <- matrix(rnorm(16), nrow = 4) 
#  original_cachable$set(generated_again)
#  j2 <- all.equal(original_raw, original_cachable$get())
#  message(j2)
#  
#  #test solving
#  solved <- cacheSolve(original_cachable)
#  solved_from_cache <- cacheSolve(original_cachable) #should print the message 'getting cached inverse'
#  
#}