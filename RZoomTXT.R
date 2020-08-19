################################################
############# ELEGIR CARPETA ##################

#carpeta <-  "C:\\Users\\pawn\\Desktop\\RZoomTXT" #fijar carpeta 
carpeta <- choose.dir() #seleccionar carpeta

################################################

if (! "tidyverse" %in% installed.packages()){
    install.packages("tidyverse")
}

library(tidyverse)

formatoHora<-function(tiempo){
  if(str_count(tiempo,":")==1){
    tiempo = paste("00:",tiempo,sep="") 
  }
  return(tiempo)
}

quitarExtension<-function(nombreArchivo){
  ultimo = nchar(nombreArchivo)
  aux= TRUE
  while (aux & ultimo > 0){ 
    if (substring(nombreArchivo,ultimo,ultimo)=="."){
      aux=FALSE
    }
    ultimo = ultimo -1
  }
  return(substring(nombreArchivo,1,ultimo))
}

descargarTXT <- function(carpeta){
  archivos <- dir(carpeta,"html")
  carpeta <- paste(carpeta ,"\\",sep="")
  keyword = "window.__data__.chatList.push"
  
  for (archivo in archivos) {
    my_txt <- readLines(paste(carpeta,archivo,sep=""),encoding="UTF-8")
    aux=T
    ultimo = length(my_txt)
    jj=0
    
    while (jj <= ultimo & aux) {
      jj=jj+1
      if (str_count(my_txt[jj],keyword) >= 1){
        aux=F
      }
    }
    
    my_txt <- my_txt[jj:ultimo]
    ultimo <- length(my_txt)
    aux=T
    
    while (ultimo > 0 & aux) {
      if (str_count(my_txt[ultimo],keyword) >= 1){
        aux=F
      }
      ultimo = ultimo-1
    }
    
    ultimo <- ultimo + 5 #ultimo chat completo
    
    my_txt <- my_txt[1:ultimo] 
    
    N= length(my_txt)/5 #numero de chats
    
    linea <- c()
    
    inicio = 0
    fin = 0
    jj=-1
    
    for (ii in 1:N){
      jj =jj +3
      nombre<-substring(my_txt[jj],12,nchar(my_txt[jj])-2)
      
      jj = jj +1
      tiempo <- formatoHora(substring(my_txt[jj],8,nchar(my_txt[jj])-2))
      
      jj = jj + 1
      mensaje <- substring(my_txt[jj],11,nchar(my_txt[jj])-1)
      
      linea[ii] <- paste(tiempo,"\t", nombre,":","\t",mensaje ,sep="")
    }
    
    archivoNombre <- quitarExtension(archivo)
    
    archivo <- file(paste(carpeta,archivoNombre,".txt",sep=""))
    writeLines(linea, archivo,useBytes=T)
    close(archivo)
  }
}
  
descargarTXT(carpeta)
