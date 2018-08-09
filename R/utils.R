# Converts object to certain type, preserving the NULL value.
# @param x Object to cast.
# @param as.type A casting function for x
# @return x casted, or NULL.
# @export
as.type_or_null <- function(x,as.type){
  if(is.null(x)){
    return(NULL)
  }
  else{
    return(as.type(x))
  }
}

# Converts object to integer, preserving the NULL value.
# @param x Object to cast.
# @return x casted, or NULL.
# @export
as.integer_or_null <- function(x){
  return(as.type_or_null(x,as.integer))
}

# Converts object to numeric, preserving the NULL value.
# @param x Object to cast.
# @return x casted, or NULL.
# @export
as.numeric_or_null <- function(x){
  return(as.type_or_null(x,as.numeric))
}

# Converts every numeric item in the list to integer.
# @param x List.
# @return A list with every numeric item as integer.
# @export
numeric.as.integer <- function(x){
  return(lapply(l, function(x){if(is.numeric(x)){as.integer(x)} else{x}}))
}

# Console log
# @param fmt Text to log.
# @export
console.log <- function(fmt,...) {
  print(paste(format(Sys.time(), "%H:%M:%S"), sprintf(fmt, ...)))
}
