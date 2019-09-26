countries <- as.data.frame(stringsAsFactors=FALSE, matrix(c(
  1,"USA",
  2,"UK",
  3,"Russia"
  ), ncol=2, byrow=TRUE,
  dimnames=list(NULL, c("id","country"))
))
