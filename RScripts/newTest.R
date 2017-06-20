require(rio)
a <- import("C:\\Users\\alok\\Desktop\\projectData.xlsx")
View(a)
a$REQ_GRADE[1]
length(a$REQ_GRADE[1])
a$REQ_GRADE[a$REQ_GRADE[1] == configGradeData$GRADE[1]] <- configGradeData$GRADE_LEVEL[1]


a$REQ_GRADE[as.matrix(strsplit(as.character(a$REQ_GRADE[1]),','))[1] == configGradeData$GRADE[1]] <- configGradeData$GRADE_LEVEL[1]




b <- read.delim(file = a$GROUP_CUSTOMER_NAME, sep = ",")
s <- as.matrix(strsplit(as.character(a$GROUP_CUSTOMER_NAME), ','))
s[1,2]
s$c..java.....net....maiframe..[1]
length(s)
View(a)
as.data.frame(a)
s[1]


as.matrix(s)
d <- data.frame(director = unlist(s))
a <- dput(as.character(a$GROUP_CUSTOMER_NAME))
a
d[[1]]

h <- data.frame()
a <- 100
b <- 1000
for (i in 1:10) {
  
  a <- a + i
  b <- b + i
  c <- paste("C1","C2",sep = ",")
  f <- data.frame(id = a , won = b ,grade = c)
  h <- rbind(h,f)
  
}
require(rio)
export(h,"pData.csv")
s <- "ad,adad,hgb"
s
a <-  paste("abc",sample(1:100,NROW(h),replace = F),"cde",sep = "" )
a
export(as.data.frame(a),"adsf.csv")
"E1" > "E2"
length(a)
b <- data.frame(name = c(a))
b
v <- 1
won<- paste("c1","c2",sep = ",")
h

seq(103:606)


seq(1, 9, by = pi)    # stays below 'end'
seq(1, 6, by = 3)
seq(1.575, 5.125, by = 0.05)
",J,"
g <- paste(",won,")
g
won



