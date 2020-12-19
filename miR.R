install_packages<-function(list.of.packages,lib=TRUE){
    if (!is.vector(list.of.packages)){
        list.of.packages<-c(list.of.packages)
    }
    
    new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
    
    if(length(new.packages)) {
        install.packages(new.packages)
    }

    if (lib){
        for(package in list.of.packages){
            library(package,character.only = TRUE)
        }
    }
}
