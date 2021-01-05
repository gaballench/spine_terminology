### Load packages
library(magrittr)
library(dplyr)

### Read the dataset
dataset <- read.delim(file = "nonShaft.tab", stringsAsFactors = FALSE) 

### Summarize data in tables
tables <- apply(X = dataset, MARGIN = 2, FUN = table)

### Construct the different combinations corresponding to different terminology systems. tic-toc indicates ca. 0.05 secs running
termSystems <- vector(length = length(tables$Spine) * length(tables$Structure) * length(tables$Term))

tic <- Sys.time()
counter <- 1
for (i in names(tables$Spine)) {
    for (j in names(tables$Structure)) {
        for (k in names(tables$Term)) {
            termSystems[counter] <- paste(i, j, k, sep = ", ")
            counter <- counter + 1
        }
    }
}
toc <- Sys.time()
toc - tic

### Calculate cost for each alternative term
freqs <- vector()
totals <- vector()
structs <- vector()
termins <- vector()

counter <- 1
for (i in names(tables$Structure)) {
    terms <- table(dataset[dataset$Structure == i, "Term"])
    for (j in seq_along(names(terms))) {
        freqs <- c(freqs, unlist(terms)[j] %>% unname)
        totals <- c(totals, sum(unlist(terms)))
        structs <- c(structs, i)
        termins <- c(termins, names(terms)[j])
        cat("Structure ", i, ", term ", names(terms)[j], ", frequency = ", freqs[counter], ", against ", totals[counter], "\n", sep = "")
        counter <- counter + 1
    }
}

output <- data.frame(Structure = structs, Terms = termins, Frequency = freqs, Total = totals, stringsAsFactors = FALSE)

output <- mutate(output, indCost = Total - Frequency)

### Determine argmin{C(t)}
argmins <- aggregate(x = output$indCost, by = list(output$Structure, output$Terms), FUN = min)

aggregate(indCost ~ Structure, data = output, min)

### identify optimal terms. This still needs to be done visually because there could be more than one optimal term
output[order(output$Structure, output$indCost),] %>% View

### Write this table for decision making
write.table(x = output, file = "optimalTerms.tab", quote = FALSE, sep = "\t", row.names = FALSE)
