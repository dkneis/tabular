temperature <- as.data.frame(stringsAsFactors=FALSE, matrix(c(
  1,"Oslo",20,
  2,"Oslo",21,
  1,"Vienna",22,
  2,"Vienna",24,
  1,"Rome",25,
  2,"Rome",30
  ), ncol=3, byrow=TRUE,
  dimnames=list(NULL, c("day","city","temperature"))
))
