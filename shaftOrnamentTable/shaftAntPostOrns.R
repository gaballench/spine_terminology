## use the pipe operator
library(magrittr)

## read the dataset
shaft <- read.delim("shaft.tab", stringsAsFactors = FALSE)

aggregated <- aggregate(Reference ~ Name + Spine, FUN = c, data = shaft)

## aggregate and generate column with LaTeX citation code
latexCol <- paste0("\\citep{", aggregated$Reference, sep = "") %>%
    gsub("c\\(", "", .) %>% 
    gsub("\\\"", "", .) %>% 
    gsub(")", "", .) %>% paste0("}") %>%
    gsub(" ", "", ., fixed = TRUE)

## Replace the old references column with the latex-formatted one
aggregated <- data.frame(aggregated[, c(1, 2)], Reference = latexCol, stringsAsFactors = FALSE)

## write back to a file
write.table(aggregated, file = "aggregatedSHaftTable.tab", sep = "\t", quote = FALSE, row.names = FALSE)
