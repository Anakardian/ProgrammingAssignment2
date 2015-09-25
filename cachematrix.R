## Overall description of functions
## makeCacheMatrix clears teh stored inversed matrix whenever it is called.
## In addition it provides teh functions used by cachesolve to display the inversed matrix.
## When called upon to do so it will calculate an inversed matrix and store for future use.

## cachesolve checks to see if the inversed matrix has already been calculated.
## If so, it returns the inversed matrix. Otherwise it calls makecacheMatrix to calculate a 
## new inversed matrix and displays that


## use the following to make the function run correctly:
##    Matrix<-makeCacheMatrix(Matrix goes here)
##    cacheSolve(Matrix) to run the function


## makeCacheMatrix will clear teh stored inversed matrix whenever it is called directly.
## In addition it makes a functions available that can be called upon to calculate a new
## inversed matrix or return an already stored inversed matrix.

makeCacheMatrix <- function(x = matrix()) {
  
        ## Clear the stored inverse matrix from the last dataset. This happens every time the
        ## function is called directly.
    inversematrix <- NULL 
    
        ## When a new matrix is supplied, replace the
        ## earlier matrix and clear the stored inversed matrix
    set <- function(y) 
      {   
        x <<- y              
        inversematrix <<- NULL
      }
    
        ## return the original matrix
    get <- function() x 
    
        ## Create the inversed matrix
    makeinversematrix <- function(solve) inversematrix <<- solve 
    
        ## Return teh inversed matrix when this is called
    getinversematrix <- function() inversematrix  
    
        ## Make a list of the functions above.
        ## These functions can be called without deleting the stored inversed matrix.
    list(set = set, get = get,     
         makeinversematrix = makeinversematrix,
         getinversematrix = getinversematrix)
    
  }
 
  



## cachesolve checks to see if the inversed matrix has already been calculated.
## If so, it returns the inversed matrix. Otherwise it calls makecacheMatrix to calculate a 
## new inversed matrix and displays that.

cacheSolve <- function(x, ...) 
  {
  
        ## Return a matrix that is the inverse of 'x'
  inversematrix <- x$getinversematrix()
  
        ## Check if the inversed matrix has already been calculated
        ## If yes, provide the stored inversed matrix.
        ## If not, calculate the new inversed matrix and display it while also storing it for the future.
  if (!is.null(inversematrix))
    {
    message("The same matrix was provided, returning cached result")
    return(inversematrix)
    }
  else
    {
      message("New matrix provided, calculating new inversed matrix")
      data <- x$get()
      inversematrix <- solve(data, ...)
      x$makeinversematrix(inversematrix)
      inversematrix
    }
  }
