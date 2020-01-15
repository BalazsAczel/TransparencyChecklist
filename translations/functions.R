# converting translations csv <--> json
#' @param x a list of length 2 with x[[1]] has the vector of languages and x[[2]] has the translations
#' @returns a data.frame with the translations in rows and languages in columns
list2df <- function(x) {
  tab <- sapply(x$translation, function(x) {unlist(x)})
  df    <- as.data.frame(t(tab), stringsAsFactors=FALSE)
  colnames(df) <- x$languages
  
  return(df)
}

#' @param x a data frame with translations in rows and languages in columns
#' @returns a list of length 2 with x[[1]] has the vector of languages and x[[2]] has the translations
df2list <- function(x){
  out <- list()
  out[['languages']] <- as.list(colnames(x))
  out[['translation']] <- list()
  
  for(i in 1:nrow(x)){
    out[['translation']][[i]] <- as.list(transdf[i,])
  }
  
  return(out)
}
