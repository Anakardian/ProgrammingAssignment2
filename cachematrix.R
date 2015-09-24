## Put comments here that give an overall description of what your
## functions do

## use cacheSolve(makeCacheMatrix(Matrix goes here)) to run the function

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
  
  if (is.matrix(x)) { ## Test if teh data being supplied is a matrix. If not, exit the function gracefully with a message
  
    ## Clear the stored inverse matrix from the last dataset and store a copy of the new dataset
    inversematrix <- NULL 
    originalmatrix <-NULL

    
    
    ## When a new matrix is supplied, replace the
    ## earlier matrix and clear the stored inversed matrix
    set <- function(y) {   
      x <<- y              
      inversematrix <<- NULL
      originalmatrix <<- x
      print(originalmatrix)
    }
    
    ## return the original matrix
    get <- function() x 
    
    ## Create the inversed matrix
    makeinversematrix <- function(solve) inversematrix <<- solve 
    
    ## Return teh inversed matrix when this is called
    getinversematrix <- function() inversematrix  
    
    ## Make a list of the functions above
    list(set = set, get = get,     
         makeinversematrix = makeinversematrix,
         getinversematrix = getinversematrix)
    
  } 
  ## If the supplied data is not a matrix, print the message and end teh function
  else {print("The supplied data is not a matrix. To use the function please supply it with a matrix.")}
}



## Write a short comment describing this function

cacheSolve <- function(x, ...) {
  
        ## Return a matrix that is the inverse of 'x'

  ## Go to the makecachematrix function and get the inversed matrix
  ## If the inversed matrix is not null, print a message and show the stored inversed matrix
  ## If the stored inversed matrix is still valid, print a message and 
  ##    show the stored inversed matrix
  inversematrix <- makeCacheMatrix$getinversematrix()
  if(!is.null(inversematrix)) {
    message("getting cached data")
    return(inversematrix)
  } ##else if (inversematrix=x)
    ##{print("identical")}
  
  else
    
    {
  ## If the stored inversed matrix is null, calculate teh inversed matrix and show it
  data <- x$get()
  inversematrix <- solve(data, ...)
  x$makeinversematrix(inversematrix)
  inversematrix
}}
