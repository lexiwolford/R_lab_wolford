#Econ 294A Assignment- 2
LexiWolfordAssignment2<-list(
  FirstName = "Lexi",
  LastName = "Wolford",
  Email = "awolford@ucsc.edu",
  studentID = 1445763
)

#1
diamonds<-get(
  load(
    file = url('https://github.com/EconomiCurtis/econ294_2015/raw/master/data/diamonds.RData')
  )
)

LexiWolfordAssignment2$s1a <- nrow(diamonds)
LexiWolfordAssignment2$s1b <- ncol(diamonds)
LexiWolfordAssignment2$s1c <- names(diamonds)
LexiWolfordAssignment2$s1d <- summary(diamonds$price)


#2
NHIS_2007 <- read.table(file = 'https://github.com/EconomiCurtis/econ294_2015/raw/master/data/NHIS_2007_TSV.txt', header = TRUE)
LexiWolfordAssignment2$s2a <- nrow(NHIS_2007)
LexiWolfordAssignment2$s2b <- ncol(NHIS_2007)
LexiWolfordAssignment2$s2c <- names(NHIS_2007)
LexiWolfordAssignment2$s2d <- mean(NHIS_2007$weight)
LexiWolfordAssignment2$s2e <- median(NHIS_2007$weight)
LexiWolfordAssignment2 <- column<-ifelse (test = NHIS_2007$weight >= 996, yes = NA, no = NHIS_2007$weight) 
LexiWolfordAssignment2 <- hist(column, na.rm = TRUE)
LexiWolfordAssignment2$s2f <- mean(column, na.rm = TRUE)
LexiWolfordAssignment2$s2g <- median(column, na.rm = TRUE)

subweight <-subset(NHIS_2007, weight<990 & SEX==1)
LexiWolfordAssignment2$s2h <- summary(subweight$weight)

subweight2 <-subset(NHIS_2007, weight<990 & SEX==2)
LexiWolfordAssignment2$s2i <- summary(subweight2$weight)


#3 
vec<-c(letters, LETTERS)
as.factor(c(letters, LETTERS))
vec_even <- vec[seq(2, length(vec), 2)]
LexiWolfordAssignment2$s3a <-print(vec_even)
LexiWolfordAssignment2$s3b <- paste(vec[c(38,5,24,9)], collapse="")
arr <- array( c(letters,LETTERS), dim = c(3,3,3))
LexiWolfordAssignment2$s3c <- arr[,c(1),2]
LexiWolfordAssignment2$s3d <- arr[2,2,]
LexiWolfordAssignment2$s3e <- paste(arr[3,1,2], arr[2,2,1], arr[3,2,3])

print(LexiWolfordAssignment2)

