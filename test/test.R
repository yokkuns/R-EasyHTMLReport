library(EasyHTMLReport)

mail.from <- "yokkuns@locahost"
mail.to <- "yohei0511@gmail.com"
subject <- "sendmailEx Test!"
body <- "This mail is sendmailEx test."

sendmailEx(mail.from,mail.to,subject,body)



A <- data.frame(id=c("2013-08-01","2013-08-02","2013-08-03",
                  "2013-08-04","2013-08-05"),
                variable="A",
                value=c(100,123,130,125,140))
B <- data.frame(id=c("2013-08-01","2013-08-02","2013-08-03",
                  "2013-08-04","2013-08-05"),
                variable="B",
                value=c(90,100,120,140,110))
C <- data.frame(id=c("2013-08-01","2013-08-02","2013-08-03",
                  "2013-08-04","2013-08-05"),
                variable="C",
                value=c(150,130,150,160,150))

D <- data.frame(id=c(1,2,3,4,5),
                variable="D",
                value=c(150,130,150,160,150))

G <- data.frame(id=c("a","b","c","d","e"),
                variable="G",
                value=c(150,130,150,160,150))

data1 <- rbind(A,B,C)

set.seed(2)
data2 <- data1
data2$value <- data2$value*100 + rnorm(nrow(data2),0,100)

set.seed(4)
data3 <- data1
data3$value <- data2$value*100 + rnorm(nrow(data2),0,50)

simpleHtmlReport("Simple HTML Report",
                 "yokkuns@localhost",
                 "yohei0511@gmail.com",
                 "simpleHtmlReport Test",
                 report.data=list(
                     "Report1" = list(data=data1),
                     "Report2" = list(data=data2),
                     "Report3" = list(data=data3),
                     "Report4" = list(data=D),
                     "Report5" = list(data=G)
                 ))
