
###### SETUP WORKING DIRECTORY ######

workDir <- getwd()

output.folders <- c(
  "1.Raw.Data","2.Clean.Data", "3.Analysis",
  "4.Graphs", "5.Tables"
)

for(i in 1:length(output.folders)){
  if(!file.exists(output.folders[i])){
    print(paste(i, "does not exist"))
    dir.create(output.folders[i])
  }
  else  {
    print(paste(i,"does exist"))
  }
}

path.rd <- paste(workDir,"/",output.folders[1], "/", sep="") #rawdata
path.cd <- paste(workDir,"/",output.folders[2], "/", sep="") #cleandata
path.a <- paste(workDir,"/",output.folders[3], "/", sep="") #analysis
path.g <- paste(workDir,"/",output.folders[4], "/", sep="") #graphs
path.t <- paste(workDir,"/",output.folders[5], "/", sep="") #tables

### INSTALL AND LOAD REQUIRED LIBS. WRNING: FIRST TIME ONLY ####

load.lib <- c(
  "ggpubr", "ggplot2","gridExtra", "jtools", "TSA", "tidyr", 
  "lubridate", "dplyr", "plyr", "tseries", "xts", "leaflet",
  "cowplot", "network", "sna", "igraph", "qgraph", "readxl"
)

install.lib <-load.lib[!load.lib %in% installed.packages()]

for(lib in install.lib) 
  install.packages(lib,dependencies=TRUE)
sapply(load.lib,require,character=TRUE)

############ 
## BEGIN CLEANING DATA ##
############

setwd(path.rd)



