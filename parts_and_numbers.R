##  Subsets sheet into user definced dates
organize <- function(x) {
  data.frame(x[,6], 
             as.factor(as.numeric(format(x[,6], format = "%W"))+1), 
             format(x[,6], format = "%B"), x[,13], x[,27], x[,22], x[,8], x[,29], x[,33], x[,35], x[,2])
}

#  Subset by date
weekly <- function(x, d1) {
  subset(x, Week == d1) 
}

#  List of makercare and non-makercare
makercare <- function(x) {
  list(x[grep("Y", x$Makercare, ignore.case=TRUE),],
       x[grep("N", x$Makercare, ignore.case=TRUE),])
}

##  Get parts
partsList <- function(x) {
  v <- regexpr("[0-9]+ [Xx] (.*?)[0-9 |a-z|)|\n{1,8}]$", x)
  parts <- regmatches(x, v)
  parts <- strsplit(parts, "\\n")
  parts <- unlist(parts)
  parts
}

## Get number of parts
numOfparts <- function(x) {
  v <- regexpr("[0-9]+ [Xx] (.*?)[0-9 |a-z|)|\n{1,2}]$", x)
  parts <- regmatches(x, v)
  parts <- strsplit(parts, "\\n")
  parts <- unlist(parts)
  g <- gregexpr("[0-9]+ [Xx]", parts)
  y <- unlist(regmatches(parts, g))
  sum(as.numeric(unlist(strsplit(y, " [Xx]"))))
}
