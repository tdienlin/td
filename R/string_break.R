string_break <- fun(object = NULL, length = 40) {
  # function to break long strings into short ones
  # function by "Deer Hunter", https://stackoverflow.com/a/29847221/8118356
  
  tmp <- paste(strwrap(object, length), collapse="\n")
  return(tmp)
  }