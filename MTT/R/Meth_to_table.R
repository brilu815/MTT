#' Converts methylation.tsv files to porportion tables
#' 
#' This function takes binary methylation.tsv files, and sums together cpmplmentary CpG sites. It then calculates the proportion of each row of CpGs that is unmethylated, hemimethylated or methylated, and then averages the amount of sites that are unmeth, hemimeth or meth. As these final mean columns are created, they are added to the results table, which is then output using the original file names as the row names.
#' @param filenames The input file, with a list of methylation.tsv files.


Meth_to_table <- function(filenames){
  results <- data.frame()
  for(i in 1:length(filenames)){
    dat <- read.table(filenames[i], quote="\"", comment.char="", header = FALSE)
    dat <- data.frame(lapply(dat, as.character), stringsAsFactors=FALSE)
    dat <- data.frame(lapply(dat, as.numeric), stringsAsFactors=FALSE)
    dat_T <- dat[, 1:(ncol(dat)/2)] + dat[, ncol(dat):(ncol(dat)/2+1)]
    dat_Strand <- transform(dat_T, Methylated = rowMeans(dat_T[,1:(ncol(dat_T))] == 2, na.rm=T), Hemimethylated = rowMeans(dat_T[,1:(ncol(dat_T))] == 1, na.rm=T), Unmethylated = rowMeans(dat_T[,1:(ncol(dat_T))] == 0, na.rm=T))
    
    dat_Meth <- data.frame(Methylated = mean(dat_Strand$Methylated), Hemimethylated = mean(dat_Strand$Hemimethylated), Unmethylated = mean(dat_Strand$Unmethylated))
    results <- rbind(results, dat_Meth)
  }
  write.table(results, file = "output.csv", row.names = filenames, 
              append = FALSE, col.names = NA, sep = ", ")
}