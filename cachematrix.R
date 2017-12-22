#The main purpose of these function is highly the usage
#of lexical scoping in R

#i)  Function makeCacheMatrix allows us to set an retrieve
#a x matrix and its inverse. 


makeCacheMatrix <- function(x = matrix()) {
  #1) First, set the function for the matrix x
  set=function(y){
    x<<-    y
    inv<<-  NULL
  }
  
  #2) Second, function that retrieves the matrix x
  get=function() x
  
  #3) Third, function that sets the inverse of the matrix x
  setinv=function(inverse) inv <<- inverse
  
  #4) Finally, this function retrieves the inverse of the matrix x
  getinv=function() inv
  
  #We store all the values in a list
  list(set=set,get=get,setinv=setinv,getinv=getinv)

}


#ii) Function cacheSolve allows us to retrieve and calculate
#an inverse matrix from the object created with the previous 
#function.

cacheSolve <- function(x, ...) {
## Return a matrix that is the inverse of 'x'
  #i) If the object x has already an inverse matrix
  #it returns it.
     
      inv=x$getinv()
      if(!is.null(inv)){
        message("Getting cached inverse matrix")
        return(inv)
      }
  #ii) If the object x has not an inverse matrix
  #it calculates it using the available matrix in the object i.e x.
  # then returns it.
      data=x$get()
      inv=solve(data)
      x$setinv(inv)
      inv
}
