#Import data
#library(readxl)
#data <- read_excel("Ing. Sistemas/Clases/DatosProyecto.xlsx", col_names=FALSE)


cartasX_R <- function(data) {
  
  A2 = NULL
  D3 = NULL
  D4 = NULL
  d2 = NULL
  enControlPatron1 = TRUE
  enControlPatron2 = TRUE
  enControlPatron3 = TRUE
  enControlPatron4 = TRUE
  enControlPatron5 = TRUE
  patronFueraControl = ''
  
  ##Patrón 5
  patron5 <- function(valores, LCS_ZC, LCI_ZC){
    colores = c()
    enControlPatron5 <<- TRUE
    longitud = length(valores)
    
    #Quince puntos consecutivos en la zona C, arriba o abajo de la línea central
    saltos = longitud - 14
    for(i in 1:saltos) {
      front = i + 14
      
      if(all(valores[i:front] < LCS_ZC & valores[i:front] > LCI_ZC)){
        colores = rep("cornflowerblue", times = longitud)
        colores[i:front] = rep("red", times = 15)
        enControlPatron5 <<- FALSE
        patronFueraControl <<- "Patrón 5: Falta de variabilidad"
        break
      }
    }
    
    return(colores)
  }
  
  ##Patrón 4
  patron4 <- function(valores, LCS_ZC, LCI_ZC) {
    colores = c()
    enControlPatron4 <<- TRUE
    longitud = length(valores)
    
    #Ocho puntos consecutivos en ambos lados de la línea central con ninguno en la zona C
    saltos = longitud - 7
    for(i in 1:saltos) {
      front = i + 7
      
      if(all(valores[i:front] > LCS_ZC | valores[i:front] < LCI_ZC)){
        colores = rep("cornflowerblue", times = longitud)
        colores[i:front] = rep("red", times = 8)
        enControlPatron4 <<- FALSE
        patronFueraControl <<- "Patrón 4: Mucha variabilidad"
        break
      }
    }
    
    return(colores)
  }
  
  ##Patrón 3
  patron3 <- function() {
    enControlPatron3 <<-TRUE
  }
  
  
  ##Patrón 2
  patron2 <- function(valores) {
    colores = c()
    enControlPatron2 <<- TRUE
    longitud = length(valores)
    
    #Seis o más puntos consecutivos ascendentes o decendentes
    saltos = longitud - 5
    
    for(i in 1:saltos) {
      front = i + 5
      if(all(diff(valores[i:front]) > 0) | all(diff(valores[i:front]) < 0)){
        colores = rep("cornflowerblue", times = longitud)
        colores[i:front] = rep("red", times = 6)
        enControlPatron2 <<- FALSE
        patronFueraControl <<- "Patrón 2: Regla 1 \nTendencias en el nivel del proceso"
        break
      }
    }
    
    if(enControlPatron2){
      library(boot)
      
      r = corr(cbind(valores, 1:longitud))
      
      if(r > 0.5 | r < -0.5){
        colores = rep("red", times = longitud)
        enControlPatron2 <<- FALSE
        patronFueraControl <<- "Patrón 2: Regla 2 \nTendencias en el nivel del proceso"
        
      }
    }
    
    #print(valores[c(TRUE, FALSE)])
      
    return(colores)
  }
  
  #Patrón 1
  patron1 <- function(valores,LCS,LCC,LCI) {
    colores = c()
    enControlPatron1 <<- TRUE
    longitud = length(valores)
    
    #Un punto fuera de los límites
    for(i in 1:length(valores)){
      if(valores[i] > LCS | valores[i] < LCI) {
        colores = c(colores, "red")
        enControlPatron1<<- FALSE
        patronFueraControl <<- "Patrón 1: Regla 1 \nDesplazamientos o cambios en el nivel del proceso"
      }else{
        colores = c(colores, "cornflowerblue")
      }
    }
    #Ocho o más puntos consecutivos de un sólo lado de la línea central.
    saltos = longitud - 7
    
    if(enControlPatron1){
      for (i in 1:saltos){
        front = i + 7
        
        if(
            all(c(valores[i:front] > LCC) == TRUE) |
            all(c(valores[i:front] < LCC) == TRUE)
          ) {
          
          colores = rep("cornflowerblue", times = longitud)
          colores[i:front] = rep("red", times = 8)
          enControlPatron1 <<- FALSE
          patronFueraControl <<- "Patrón 1: Regla 2 \nDesplazamientos o cambios en el nivel del proceso"
          break
        }
      }
    }
    
    #Al menos 10 de 11 puntos consecutivos caen de un mismo lado de la línea central.
    saltos = longitud - 10
    if(enControlPatron1){
      for(i in 1:saltos) {
        front = i + 10
        
        sumaPuntos = sum(c(valores[i:front] > LCC), na.rm = TRUE)
        
        if(sumaPuntos == 1 | sumaPuntos == 10){
          
          colores = rep("cornflowerblue", times = longitud)
          colores[i:front] = rep("red", times = 11)
          enControlPatron1 <<- FALSE
          patronFueraControl <<- "Patrón 1: Regla 3 \nDesplazamientos o cambios en el nivel del proceso"
          break
        }
      }
    }
    
    #Por lo menos 12 de 14 puntos consecutivos ocurren por un mismo lado de la línea central.
    saltos = longitud - 13
    if(enControlPatron1){
      for(i in 1:saltos) {
        front = i + 13
        
        sumaPuntos = sum(c(valores[i:front] > LCC), na.rm = TRUE)
        
        if(sumaPuntos == 2 | sumaPuntos == 12){
          
          colores = rep("cornflowerblue", times = longitud)
          colores[i:front] = rep("red", times = 14)
          enControlPatron1 <<- FALSE
          patronFueraControl <<- "Patrón 1: Regla 4 \nDesplazamientos o cambios en el nivel del proceso"
          break
        }
      }
    }
    
    return(colores)
  }
  
  ##Carta de control X
  carta_X <- function(rowLength, mediaDeRangos) {
    mediasX = rowMeans(data)
    
    mediaDeMediasX = mean(mediasX)
    
    LCS_CX = mediaDeMediasX + A2 * mediaDeRangos
    LCC_CX = mediaDeMediasX
    LCI_CX = mediaDeMediasX - A2 * mediaDeRangos
    
    diferenciaZonas = (LCS_CX - LCC_CX)/3
    LCS_ZB = LCC_CX + diferenciaZonas*2
    LCS_ZC = LCC_CX + diferenciaZonas
    LCI_ZB = LCC_CX - diferenciaZonas*2
    LCI_ZC = LCC_CX - diferenciaZonas
    msg = NULL
    
    CX_colores = c()
    colores = patron1(mediasX, LCS_CX,LCC_CX,LCI_CX)
    
    if(!enControlPatron1){
      CX_colores = colores
      msg = paste("Fuera de control por:",patronFueraControl,sep = "\n")
      
    }else{
      colores = patron2(mediasX)
    }
    
    if(!enControlPatron2){
      CX_colores = colores
      msg = paste("Fuera de control por:",patronFueraControl,sep = "\n")
    }else if(enControlPatron1){
      print("aqui 2")
      colores = patron3()
    }
    
    if(!enControlPatron3){
      CX_colores = colores
      msg = paste("Fuera de control por:",patronFueraControl,sep = "\n")
    }else if(enControlPatron2 && enControlPatron1){
      print("aqui 3")
      colores = patron4(mediasX, LCS_ZC, LCI_ZC)
    }
    
    if(!enControlPatron4){
      CX_colores = colores
      msg = paste("Fuera de control por:",patronFueraControl,sep = "\n")
    }else if(enControlPatron1 & enControlPatron2 & enControlPatron3){
      print("aqui 4")
      colores = patron5(mediasX, LCS_ZC, LCI_ZC)
    }
    
    if(!enControlPatron5){
      CX_colores = colores
      msg = paste("Fuera de control por:",patronFueraControl,sep = "\n")
    }
    
    if(enControlPatron1 & enControlPatron2 & enControlPatron3 &
       enControlPatron4 & enControlPatron5) {
      CX_colores = rep("cornflowerblue", times = rowLength)
      msg = "Bajo control estadístico"
    }
    
    plot(
      c(1:rowLength), 
      mediasX,
      main = "Carta de Control X",
      pch=19,
      xlab = "Muestras",
      ylab = "Medias",
      col = CX_colores,
      type = "o",
      xaxp = c(1,rowLength,rowLength-1),
      ylim = c(
        ifelse(LCI_CX > min(mediasX), min(mediasX),LCI_CX),
        ifelse(LCS_CX < max(mediasX), max(mediasX),LCS_CX)
      )
    )
    
    abline(h=c(LCI_CX, LCI_ZB, LCI_ZC, LCC_CX, LCS_ZC, LCS_ZB,LCS_CX),
           col = c("red3", "yellow", "lightblue","blue", "lightblue", "yellow","red3"),
           lty = c("solid", "dashed", "dotted","solid", "dotted", "dashed","solid"),
           lwd =2)
    
    legend(1,ifelse(LCS_CX < max(mediasX), max(mediasX),LCS_CX),
           c("LCS","LC", "LCI"),
           lty = c("dashed", "dotdash", "dashed"),
           pch = c(NA,NA,NA,18),
           col = c("Green","Gray","Yellow"),
           cex = 0.7)
    
    return (msg)
    
  }
  
  ##Carta de control R
  carta_R <- function(rowLength, mediaDeRangos, rangos) {
    
    LCS_CR = D4 * mediaDeRangos
    LCC_CR = mediaDeRangos
    LCI_CR = D3 * mediaDeRangos
    
    diferenciaZonas = (LCS_CR - LCC_CR)/3
    LCS_ZB = LCC_CR + diferenciaZonas*2
    LCS_ZC = LCC_CR + diferenciaZonas
    LCI_ZB = LCC_CR - diferenciaZonas*2
    LCI_ZC = LCC_CR - diferenciaZonas
    msg = NULL
    
    CR_colores = c()
    colores = patron1(rangos, LCS_CR,LCC_CR,LCI_CR)
    
    if(!enControlPatron1){
      CR_colores = colores
      msg = paste("Fuera de control por:",patronFueraControl,sep = "\n")
      
    }else{
      colores = patron2(rangos)
    }
    
    if(!enControlPatron2){
      CR_colores = colores
      msg = paste("Fuera de control por:",patronFueraControl,sep = "\n")
    }else if(enControlPatron1){
      colores = patron3()
    }
    
    if(!enControlPatron3){
      CR_colores = colores
      msg = paste("Fuera de control por:",patronFueraControl,sep = "\n")
    }else if(enControlPatron1 & enControlPatron2){
      colores = patron4(rangos, LCS_ZC, LCI_ZC)
    }
    
    if(!enControlPatron4){
      CR_colores = colores
      msg = paste("Fuera de control por:",patronFueraControl,sep = "\n")
    }else if(enControlPatron1 & enControlPatron2 & enControlPatron3){
      colores = patron5(rangos, LCS_ZC, LCI_ZC)
    }
    
    if(!enControlPatron5){
      CR_colores = colores
      msg = paste("Fuera de control por:",patronFueraControl,sep = "\n")
    }
    
    if(enControlPatron1 & enControlPatron2 & enControlPatron3 &
       enControlPatron4 & enControlPatron5) {
      CR_colores = rep("cornflowerblue", times = rowLength)
      msg = "Bajo control estadístico"
    }
    
    plot(
      c(1:rowLength), 
      rangos,
      main = "Carta de Control R",
      pch=19,
      xlab = "Muestras",
      ylab = "Rangos",
      col = CR_colores,
      type = "o",
      xaxp = c(1,rowLength,rowLength-1),
      ylim = c(
        ifelse(LCI_CR > min(rangos), min(rangos),LCI_CR),
        ifelse(LCS_CR < max(rangos), max(rangos),LCS_CR)
      )
    )
    
    abline(h=c(LCI_CR, LCI_ZB, LCI_ZC, LCC_CR, LCS_ZC, LCS_ZB,LCS_CR),
           col = c("red3", "yellow", "lightblue","blue", "lightblue", "yellow","red3"),
           lty = c("solid", "dashed", "dotted","solid", "dotted", "dashed","solid"),
           lwd =2)
    
    legend(1,ifelse(LCS_CR < max(rangos), max(rangos),LCS_CR),
           c("LCS","LC", "LCI"),
           lty = c("dashed", "dotdash", "dashed"),
           pch = c(NA,NA,NA,18),
           col = c("Green","Gray","Yellow"),
           cex = 0.7)
    
    return (msg)
  }
  
  ##Obtener factores
  obtenerFactores <- function(n) {
  if(!require("gsheet")) install.packages("gsheet")
  library(gsheet)
  factores <- gsheet2tbl("docs.google.com/spreadsheets/d/1_OqgFh6zKBnswubGYO0uaZ7eFl59l8i1")
  for(i in 1:nrow(factores)){
    if(factores[i,1] == n){
      A2 <<- as.numeric(factores[i, 2])
      D3 <<- as.numeric(factores[i, 4])
      D4 <<- as.numeric(factores[i, 5])
      d2 <<- as.numeric(factores[i, 7])
      break
    }
  }
}
  
  main <- function(){
    if(!require("gplots")) install.packages("gplots")
    library(gplots)
    n = ncol(data)
    rowLength = nrow(data)
    
    obtenerFactores(n)
    
    rangos = c()
    for(i in 1:rowLength) {
      rangos = c(rangos, abs(max(data[i,]) - min(data[i,])))
    }
    
    mediaDeRangos = mean(rangos)
    
    par(mar = c(1, 1, 1, 1))
    
    par(fig = c(0.05, 0.5, 0.25, 0.95))
    msg1 = carta_X(rowLength,mediaDeRangos)
    
    par(fig = c(0.5, 0.95, 0.25, 0.95), new = TRUE)
    msg2 = carta_R(rowLength,mediaDeRangos,rangos)
    
    par(fig = c(0.05, 0.5, 0, 0.3), new = TRUE)
    textplot(msg1, halign = "center", cex = 0.6)
    
    par(fig = c(0.5, 0.95, 0, 0.3), new = TRUE)
    textplot(msg2, halign = "center", cex = 0.6)
  }
  
  main()
}



