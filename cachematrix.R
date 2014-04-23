## Write a short comment describing this function
#`makeCacheMatrix` creates a special "matrix", that is 
#used as the required argument type for the cacheSolve function

makeCacheMatrix <- function(x = matrix()) {
  #initialize the variable m as NULL
  m <- NULL
  #creates a inner-function inside the parent function 'makeCacheMatrix'
  #in the local environment
  set <- function(y) {
    #assigning the value x to the object y
    x <<- y
    #assigning the value NULL to the object m 
    m <<- NULL
  }
  
  #assigns an anonymous function in the global environment to the matrix x,
  #convenient to pass as an argument to a higher-order function,
  #in this case 'makeCacheMatrix' which takes one or more functions 
  #as an input or outputs a function
  get <- function() x
  #setinverse store the given value as inverse for future use and
  #function(solve) m, assigns an anonymous function to the variable setinverse,
  #which takes a variable solve as its argument and sets the variable m in 
  #the global environment to the value contained in solve
  
  setinverse <- function(solve) m <<- solve
  #getinverse function to extract the inverse
  getinverse <- function() m
  #output the special "matrix" list, that is going to be used as the required 
  #argument type for the cacheSolve function
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}

## Write a short comment describing this function

#cacheSolve function takes as an argument the list generated for 
#makeCacheMatrix and returns the inverse of the items in the list 
#and an out of band message saying that it is using 
#the cached value if it has seen the special list before.

cacheSolve <- function(x, ...) {
  ##Return a matrix that is the inverse of 'x'
  ##the value of the inverse of x (x$getinverse()) is assign to m
  m <- x$getinverse()
  #self contain
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  #assign the x value to a variable called data
  data <- x$get()
  #assign the inverse of data to m
  m <- solve(data, ...)
  #x$setinverse() is simply a function that writes the previously-calculated 
  #inverse of a matrix into the function. 
  x$setinverse(m)
  #outputs m
  m
}

cacheSolve(makeCacheMatrix(matrix(1:4,2,2)))
cacheSolve(x<-makeCacheMatrix(matrix(1:4,2,2)))
cacheSolve(x)
#getting cached data
x <- makeCacheMatrix(matrix(1:4,2,2))
cacheSolve(x)
cacheSolve(x)
#getting cached data
x$getinverse()
x$get()
x$set(matrix(2:5,2,2)) #I can access x$set() directly to change the underlying vector
#this resets m to NULL and forces cacheSolve() to calculate a new mean
cacheSolve(x)
x$get()
