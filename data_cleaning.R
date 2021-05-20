#####################
### Data Cleaning ###
#####################

### Setting Working Directory
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

### Loading Packages
library(tidyverse)
library(vroom)

### Reading in data
responses <- vroom("../../data/responses.csv")

#####################
### Data Cleaning ###
#####################

### Reducing to only responses
res <- responses[,c(1:50,107)]

### filtering by IPC == 1
res <- res %>% filter(IPC == 1) %>% select(-IPC)

### Getting rid of responses with 0's and null responses
res$nullCount <- apply(res,1,function(x) sum(x == "NULL")) #1,141 have all NULL responses, rest are complete
res$zeroCount <- apply(res,1,function(x) sum(x == "0"))    #92,382 are missing at least one response. Of those most are missing 1,2,3,4, and 50 responses

res <- res %>% filter(nullCount == 0, zeroCount == 0) %>% select(-c(nullCount,zeroCount)) #reduced to 603,322 observations

### Changing to numeric format
res <- res %>% apply(2, as.numeric) %>% as.data.frame()

### Replacing Values for negative questions
neg.quest <- c('EXT2','EXT4','EXT6','EXT8','EXT10', # 5
               'EST2','EST4',                       # 2
               'AGR1','AGR3','AGR5','AGR7',         # 4
               'CSN2','CSN4','CSN6','CSN8',         # 4
               'OPN2','OPN4','OPN6')                # 3

pos.quest <- c('EXT1','EXT3','EXT5','EXT7','EXT9',                       # 5
               'EST1','EST3','EST5','EST6','EST7','EST8','EST9','EST10', # 8
               'AGR2','AGR4','AGR6','AGR8','AGR9','AGR10',               # 6
               'CSN1','CSN3','CSN5','CSN7','CSN9','CSN10',               # 6
               'OPN1','OPN3','OPN5','OPN7','OPN8','OPN9','OPN10')        # 7

# res[,"EXT2"] %>% table #1 115440, 2 140936, 3 146777, 4 120122, 5 80047; checking values
res[,neg.quest] <- apply(res[,neg.quest],2, function(x) (-1 * x) + 6)

### Summing values across each group
sumRes <- res %>% mutate(EXT = rowSums(.[1:10]),
                         EST = rowSums(.[11:20]),
                         AGR = rowSums(.[21:30]),
                         CSN = rowSums(.[31:40]),
                         OPN = rowSums(.[41:50])) %>% select(EXT:OPN)

### Normalizing 
sumNorm <- as.data.frame(apply(sumRes,2,function(x) (x - mean(x)) / sd(x)))

### Saving Data set
write.csv(file = "normalized.csv",sumNorm, row.names = FALSE)