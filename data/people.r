people <- as.data.frame(stringsAsFactors=FALSE, matrix(c(
  "Trump",1,
  "Johnson",2,
  "Blair",2,
  "Putin",3
  ), ncol=2, byrow=TRUE,
  dimnames=list(NULL, c("name","id_country"))
))
