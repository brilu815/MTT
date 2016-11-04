#' MethTable_to_ggplot
#' 
#' This function takes the table output from Meth_to_table and plots it.
#' 
#' 

library(dplyr)
library(ggplot2)

input.file <- output.csv

MethTable_to_ggplot <- function(input.file){
  meth.table <- read.table(input.file, header = T, sep = ",", dec = ".")
  meth.table.rownames <- data.frame(meth.table[,-1], row.names=meth.table[,1])
  
  ggplot(meth.table, aes(X, fill = drv)) + geom_bar(position="fill")
         
}
barplot(as.matrix(meth.table))

E14_A_0hr_Kcnv2EO_transpose_CpG_Mean3$value<-round(E14_A_0hr_Kcnv2EO_transpose_CpG_Mean3$value , digits=2)
E14_A_0hr_Kcnv2EO_transpose_CpG_Mean3= ddply(E14_A_0hr_Kcnv2EO_transpose_CpG_Mean3, "Rowname", mutate, label_y=cumsum(value) - .5*value)
ggplot(E14_A_0hr_Kcnv2EO_transpose_CpG_Mean3, aes(x=Rowname, y=value, fill=variable)) +
  geom_bar(stat='identity') +
  geom_text(aes(y=label_y, label=value), color='white')->P111
P111 <- P111 + labs(x="Hairpin CpG based methylation", y="% Methylation") + 
  ggtitle("RASSF1A-S11") 
P111
P111 <- P111+ theme(legend.position="bottom", legend.direction="horizontal", 
                    legend.title = element_blank())
P111
P111<-P111+ theme(plot.title = element_text(size=10,face="bold"))
P111
P111<-P111+theme(axis.text.x=element_text(angle=70, size=07,face="bold",vjust=0.5))
P111
P111<-P111+theme(axis.text.y=element_text(size=8,face="bold",vjust=0.5))
P111