library(gridExtra)
library(grid)

#MEAN HYPER
my_table = matrix(0,nrow = 6, ncol = 6)
my_table[1,] = CRE21[100*c(1:6),1]
my_table[2,] = CRE22[100*c(1:6),1]
my_table[3,] = CRE31[100*c(1:6),1]
my_table[4,] = CRE32[100*c(1:6),1]
my_table[5,] = CRE51[100*c(1:6),1]
my_table[6,2:6] = MAZDA[21*c(1:5),1]

labelsCol = c("No CHT", "Static B=1", "Static B=100", "SelfAdaptative", "Dynamic C=0.05 A=2", "Dynamic C=0.02 A=2")
labelsRow = c("CRE21", "CRE22", "CRE31", "CRE32", "CRE51", "MAZDA")
colnames(my_table) = labelsCol
rownames(my_table) = labelsRow
#my_table = round(my_table,5)
my_table = formatC(my_table, format = "e", digits = 5)

d <- head(my_table)

#SD HYPER
my_table2 = matrix(0,nrow = 6, ncol = 6)
my_table2[1,] = CRE21[100*c(1:6),2]
my_table2[2,] = CRE22[100*c(1:6),2]
my_table2[3,] = CRE31[100*c(1:6),2]
my_table2[4,] = CRE32[100*c(1:6),2]
my_table2[5,] = CRE51[100*c(1:6),2]
my_table2[6,2:6] = MAZDA[21*c(1:5),2]

colnames(my_table2) = labelsCol
rownames(my_table2) = labelsRow
#my_table2 = round(my_table2,5)
my_table2 = formatC(my_table2, format = "e", digits = 5)

e <- head(my_table2)

#d <- list(fontface=matrix(c(1,2,3), ncol=ncol(d),nrow=nrow(d),byrow=TRUE))
grid.table(e)
