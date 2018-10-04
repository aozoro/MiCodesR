rm(list = ls())
list.of.packages <- c("readr", "dplyr","openxlsx")
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages)
rm("list.of.packages","new.packages")

myfile <- file.choose()
myfolder <- dirname(myfile)

library(readr)
library(dplyr)
library(openxlsx)
cartera <- read_delim(myfile,"|", escape_double = FALSE, 
                      col_types = cols(CUENTA_SAT = col_double(), NUM_DOCUMENTO = col_character(), 
                                       NOMBRES = col_character(), APELLIDO_MATERNO= col_character(), 
                                       APELLIDO_PATERNO = col_character(), FECHA_FACTURACION=col_integer()), 
                      trim_ws = TRUE, locale=locale(encoding = "latin1"))

cartera <- data.frame(cartera[,"CUENTA_SAT"],cartera[,"NUM_DOCUMENTO"], cartera[,"NOMBRES"],
                      cartera[,"APELLIDO_PATERNO"],cartera[,"APELLIDO_MATERNO"], 
                      cartera[,"FECHA_FACTURACION"])

colnames(cartera)[1:6] <- c("CUENTA","DNI","NOMBRES", "APELLIDO_P","APELLIDO_M","DIAFACTURACION")
cartera$CUENTA <- ifelse(cartera$CUENTA > 10^11, trunc(cartera$CUENTA/100) , cartera$CUENTA )
cartera <-  arrange(cartera , CUENTA)

wb <- createWorkbook()
stp=5*10^5
N=nrow(cartera)
t=trunc(N/stp)
nh=ceiling(N/stp)
Lim <- c()

for (i in 1:t){
    first=1+stp*(i-1)
    last = stp*i
    hoja<-data.frame(cartera[first:last,])
    Lim[i]<-hoja[1,1]
    wsName <-paste0("Hoja",i)
    addWorksheet(wb,wsName)
    writeData(wb,wsName,hoja)   
} 

if(t!=nh){
    i=i+1
    first=1+stp*(i-1)
    last=N
    hoja<-data.frame(cartera[first:last,])
    Lim[i]<-hoja[1,1]
    wsName <-paste0("Hoja",i)
    addWorksheet(wb,wsName)
    writeData(wb,wsName,hoja)
}

i=i+1
wsName <-paste0("Hoja",i)
addWorksheet(wb,wsName)
t=length(Lim)
hoja<- data.frame(Lim,c(1:t))

writeData(wb,wsName,hoja,colNames = FALSE)

saveWorkbook(wb,paste0(myfolder,"/BuscarCuenta.xlsx"), overwrite = TRUE)
