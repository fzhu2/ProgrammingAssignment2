## Write a short comment describing this function
#Hello my peers!
#In the code below, I followed the pattern of the example - to cache mean - given by our instructor, with some adjustment
#of the code in the example. While overwriting the original codes in the example, these adjustments are meant to implement 
#the function of getting the inverse matrix.

# A new symbol introduced here is:
# inv - stores cache value of the calculated inverse matrix

#I hope you guys find my codes readable rather than annoying!
#Good luck to me and good luck to you, my friend!


makeCacheMatrix <- function(x = matrix()) {
  #initialize the variable - inv - that stores cache value
  inv <- NULL
  
  #set function sets x to the argument y - a matrix -  and set cache value - inv -  to null
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  
  #get returns the value of x (argument of makeCacheMatrix)
  get <- function() x
  
  #sets inv in makeCacheMatrix to inverse 
  setInv <- function(inverse) inv <<- inverse
  
  # getInv returns the value of inv 
  getInv <- function() inv
  
  #returns a labeled vector of functions set, get, setInv and getInv

  list(set = set, get = get,
       setInv = setInv,
       getInv = getInv)
}




cacheSolve <- function(x, ...) {
## Return a matrix that is the inverse of 'x'

  #attempts to get the inverse matrix from x (if it was calculated previously)
  inv <- x$getInv()
  
  #if not null, a valued was cached, so return inv 

  if(!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  
  #since its null, set data to x from makeCacheMatrix
  data <- x$get()
  
  #calculate the inverse matrix of the input
  inv <- solve(data)
  
  #set inv in x to calculated inverse matrix

  x$setInv(inv)
  
  #return the inverse matrix - inv.
  inv
}
