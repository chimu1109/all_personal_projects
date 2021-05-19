# Loading required libraries
library(plyr)
library(dplyr)

# Inputting the data and filtering relevant data
trial <- read.delim("~/trial.gff", header=FALSE, comment.char="#")
useful <- select(trial, V1:V5)
useful1 <- filter(useful, V3 =="CDS")

#Creating a blank matrix 
m <- matrix(, nrow = 4310, ncol = 1)

#Running a for loop for the distances and saving the output to the matrix
for(i in 1:4310){
        m1 <- c()
        m2 <- c()
        m1 <- useful1$V4[i]
        m2 <- useful1$V5[1:4310]
        m1 <- c(m1, m2[-i])
        b <- m1 - m1[1] 
        #        b <- dist(m1, method = "manhattan")
        
        if(i==1){
                c <- b
        } else if(i == 4310){
                c <- c(b[2:4310], 0)
        } else{
                c <- b[2:i]
                c <- c(c, 0)
                c <- c(c, b[i+1:4310])
        }
        m <- cbind(m, c)          

}

#Removing the first column of the matrix as it has NAs
m <- m[, -1]

#Adding 1 to the matrix 
final <- m+1

#Writing to a .tsv file
#write.table(as.data.frame(final), file = "final1_genes.tsv", sep = "\t")
