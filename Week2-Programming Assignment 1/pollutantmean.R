pollutantmean <- function(directory,pollutant,id = 1:332){
    # 'directory' is a character vector of length 1 indicating the location of the CSV files
    # 'pollutant' is a character vector of length 1 indicating the name of the pollutant for which we will calculate the mean; either "sulfate" or "nitrate".
    # 'id' is an integer vector indicating the monitor ID numbers to be used
    # Return the mean of the pollutant across all monitors list in the 'id' vector (ignoring NA values)
    # NOTE: Do not round the result!
    file <- list.files(directory,full.names = True)
    dat <- data.frame()
    for(i in id){
        dat <- rbind(dat, read.csv(file[i])
    }
    mean(dat[,pollutant],na.rm = TRUE)

}
source("pollutantmean.R")
pollutantmean("specdata", "sulfate", 1:10)
