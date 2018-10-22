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
                      cartera[,"FECHA_FACTURACION"],cartera[,"FECBAJA"])

colnames(cartera)[1:7] <- c("CUENTA","DNI","NOMBRES", "APELLIDO_P","APELLIDO_M","DIAFACTURACION","FECHACIERRE")

N=nrow(cartera)


cartera<-data.frame(cartera,vector("character",N),vector("character",N))
colnames(cartera)[8:9]<-c("ESTADOCUENTA","NOMBRE_COMPLETO")

cartera$ESTADOCUENTA<-ifelse(cartera$FECHACIERRE == "0001-01-01", "A", "C")

cartera$NOMBRE_COMPLETO<-ifelse(cartera$APELLIDO_M=="X",paste(cartera$NOMBRES,cartera$APELLIDO_P),paste(cartera$NOMBRES,cartera$APELLIDO_P,cartera$APELLIDO_M))
cartera$CUENTA <- ifelse(cartera$CUENTA > 10^11, trunc(cartera$CUENTA/100) , cartera$CUENTA)


cartera <- select(cartera,CUENTA,DNI,NOMBRE_COMPLETO,ESTADOCUENTA,FECHACIERRE)
colnames(cartera)[1:5] <- c("CUENTA","DNI","NOMBRE_COMPLETO","ESTADOCUENTA","FECHACIERRE")

cartera <- subset(cartera,ESTADOCUENTA=="C")
cartera <- arrange(cartera , CUENTA)
cartera <- select(cartera,-ESTADOCUENTA)

wb <- createWorkbook()
stp=10^6
N=nrow(cartera)
t=trunc(N/stp)
nh=ceiling(N/stp)

if (t>=1){
	for (i in 1:t){
		 first=1+stp*(i-1)
		 last = stp*i
		 hoja<-data.frame(cartera[first:last,])
		 wsName <-paste0("Hoja",i)
		 addWorksheet(wb,wsName)
		 writeData(wb,wsName,hoja)   
	} 
}
	
 if(t!=nh){
	i=i+1
	first=1+stp*(i-1)
	last=N
	hoja<-data.frame(cartera[first:last,])
	wsName <-paste0("Hoja",i)
	addWorksheet(wb,wsName)
	writeData(wb,wsName,hoja)
}

saveWorkbook(wb,paste0(myfolder,"/CuentasCerradas.xlsx"), overwrite = TRUE)
 
