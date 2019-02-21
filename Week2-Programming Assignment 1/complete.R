complete <- function(directory, id = 1:332) {
    ## 'directory' is a character vector of length 1 indicating
    ## the location of the CSV files
    
    ## 'id' is an integer vector indicating the monitor ID numbers
    ## to be used
    
    ## Return a data frame of the form:
    ## id nobs
    ## 1  117
    ## 2  1041
    ## ...
    ## where 'id' is the monitor ID number and 'nobs' is the
    ## number of complete cases
    
    # set working directory
    setwd("~/文档/Week2")
    # get a list of files in specdata
    file <- list.files(directory,full.name = TRUE)
    # initiate a data frame
    dat <- data.frame()
    for (i in id){
        current_file <- read.csv(file[i])
        nobs <- sum(complete.cases(current_file))
        dat <- rbind(dat,c(i,nobs))
    }
    colnames(dat) <- c("id","nobs")
    dat
}

# quiz answers
cc <- complete("specdata", 54)
print(cc$nobs)

set.seed(42)
cc <- complete("specdata", 332:1)
use <- sample(332, 10)
print(cc[use, "nobs"])