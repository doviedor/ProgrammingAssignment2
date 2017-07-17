## The time.elapsed() function helps us to invert a Matrix and  calculate the
## elapsed time required to do that.

## makeCacheMatrix(): This function is the base to generate all the functions required to
##                    invert the matrix

makeCacheMatrix <- function(x = numeric()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  set.inv <- function(inverse) m <<- inverse
  get.inv <- function() m
  list(set = set, get = get,
       set.inv = set.inv,
       get.inv = get.inv)
}


## cacheSolve(): This function is the responsable to invert the matrix

cacheSolve <- function(x, ...) {
  m <- x$get.inv()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  inv.data <- x$get()
  m <- solve(inv.data, ...)
  x$set.inv(m)
  return(m)
}


##time.elapsed(): This is the funcion that calculate the time to invert a matrix
time.elapsed <- function(datos){
  
  ## @datos: Matrix to invert
  temp.data <- makeCacheMatrix(datos)
  
  ## Start.time: Variable to store the start time
  start.time = Sys.time()
  
  ## Inverting the matrix
  cacheSolve(temp.data)
  
  ## Time elapsed to invert the Matrix
  time.total = Sys.time() - start.time
  
  print(time.total)
  
  
}