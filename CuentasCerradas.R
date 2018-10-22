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

cartera <- select(cartera,CUENTA_SAT,NUM_DOCUMENTO,NOMBRES,APELLIDO_PATERNO,APELLIDO_MATERNO,FECBAJA)

colnames(cartera)[1:6] <- c("CUENTA","DNI","NOMBRES", "APELLIDO_P","APELLIDO_M","FECHACIERRE")
cartera <- subset(cartera, FECHACIERRE != "0001-01-01")

Nombre_Completo <- data.frame(ifelse(cartera$APELLIDO_M=="X",paste(cartera$NOMBRES,cartera$APELLIDO_P),paste(cartera$NOMBRES,cartera$APELLIDO_P,cartera$APELLIDO_M)))
colnames(Nombre_Completo) <- "NOMBRE_COMPLETO"

cartera<- select(cartera, -NOMBRES, -APELLIDO_P, -APELLIDO_M)
cartera<- data.frame(cartera[,1:2],Nombre_Completo, cartera[,3:ncol(cartera)])
cartera<-arrange(cartera,desc(FECHACIERRE))

wb <- createWorkbook()
stp=10^6
N=nrow(cartera)
t=trunc(N/stp)
nh=ceiling(N/stp)

i<-0
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
