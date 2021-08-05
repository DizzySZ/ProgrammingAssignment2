## These two functions will allow you to cache the inverse of a matrix


## makeCacheMatrix creates a list with all the functions and variables you need
## to save the inverse of the matrix

makeCacheMatrix <- function(x = matrix()) {
  #create empty variable i to store the inverse matrix
  i <- NULL
  #creates function that sets x to the first function argument (y) and i as null
  set <- function(y) {
    x <<-y
    i <<-NULL
  }
  #creates function that takes value of x
  get <- function() x
  #creates function that assigns i to the function argument "solve"
  setinverse <- function(solve) i <<- solve
  #creates function that gets value of i
  getinverse <- function() i
  #creates list of functions (set, get, setinverse, getinverse)
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


## cacheSolve checks if the inverse has already been calculated
## and if not, then it will calculate the inverse matrix

cacheSolve <- function(x, ...) {
  ## Start by checking if matrix inverse already exists in list and
  ## if so, then fetches value
  i <- x$getinverse()
  if(!is.null(i)){
    message("getting cached data")
    return(i)
  }
  #if no inverse, then calculates inverse by assigning data as matrix,
  #solving for its inverse and assigning it to i
  data <- x$get()
  i <- solve(data)
  x$setinverse(i)
  i
  ## Return a matrix that is the inverse of 'x'

}