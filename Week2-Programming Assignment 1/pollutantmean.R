pollutantmean <- function(directory,pollutant,id = 1:332){
    # 'directory' is a character vector of length 1 indicating the location of the CSV files
    # 'pollutant' is a character vector of length 1 indicating the name of the pollutant for which we will calculate the mean; either "sulfate" or "nitrate".
    # 'id' is an integer vector indicating the monitor ID numbers to be used
    # Return the mean of the pollutant across all monitors list in the 'id' vector (ignoring NA values)
    # NOTE: Do not round the result!
    
    # set working directory
    setwd("~/文档/Week2")
    # get a list of files in specdata directory
    file <- list.files(directory,full.names = TRUE)
    # initiate a data frame
    dat <- data.frame()
    for(i in id){
        dat <- rbind(dat,read.csv(file[i]))
    }

    mean(dat[,pollutant],na.rm = TRUE)
}

# Quiz answers
pollutantmean("specdata", "sulfate", 1:10)
pollutantmean("specdata", "nitrate", 70:72)
pollutantmean("specdata", "sulfate", 34)
pollutantmean("specdata", "nitrate")
