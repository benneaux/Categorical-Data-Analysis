
# Data comes from Agresti's website.

table.6.9 <- data.frame(scan(file="Data/Agresti_6_9.txt",
                             what=list(Center="",
                                       Treatment="",
                                       Response="",
                                       Freq=0))) 
levels(table.6.9$Treatment) <- c("Drug","Control") 
levels(table.6.9$Response) <- c("Success","Failure")

array.6.9 <- xtabs(Freq~Treatment+Response+Center, data=table.6.9)

mantelhaen.test(array.6.9, correct = TRUE)
