library(pdftools)
library(stringr)
library(tidyverse)
library(tabulizer)
library(dplyr)
library(reshape2)
#FOLIARES----
url<- "C:/Users/jrobledo/Desktop/Resultado muestras la Guajira/Foliares/"
files <- list.files(pattern = "pdf$", path= url)
files<- paste(url, files, sep="")
opinions <- lapply(files, pdf_text)
identificacion<- c()
nitrogeno<- c()
faltantes<- data.frame()
boro<- c()
for (i in 1:length(opinions)) {
  identificaciontemp<- gsub('^.*Identificación:\\s*|\\s*Matriz.*$', '', opinions[[i]])
  identificacion<- c(identificacion,identificaciontemp)
  nitrogenotemp<- gsub('^.*Kjeldahl/Vo lumetría\\s*|\\s*\r\n.*$', '', opinions[[i]])
  nitrogeno<- c(nitrogeno,nitrogenotemp)
  otros<- str_match_all(opinions[[i]], "Espectro metría de emisió n de\\s*(.*?)\\s*\r\n")
  otros <- data.frame(matrix(unlist(otros), nrow=length(otros), byrow=TRUE))
  otros<- otros[,((ncol(otros)/2)+1):ncol(otros)]
  if(ncol(otros)==10){
    colnames(otros)<- c("P", "K", "Ca", "Mg", "Na", "S", "Fe", "Cu","Mn", "Zn")
    faltantes<- rbind(faltantes,otros)
    borotemp<- gsub('^.*VIS\\s*|\\s*\r\n.*$', '', opinions[[i]])
    boro<- c(boro,borotemp)
  }else{
    colnames(otros)<- c("P", "K", "Ca", "Mg", "Na", "S", "Fe", "Cu","Mn", "Zn", "B")
    borotemp<- otros$B
    otros<- otros[,1:(ncol(otros)-1)]
    faltantes<- rbind(faltantes,otros)
    boro<- c(boro,borotemp)
  }
}
def<- data.frame(ID= identificacion, N= nitrogeno, faltantes,  B= boro)








#DENSIDAD APARENTE----
url<- "C:/Users/jrobledo/Desktop/Resultado muestras la Guajira/Densidad aparente/"
files <- list.files(pattern = "pdf$", path= url)
files<- paste(url, files, sep="")
opinions <- lapply(files, pdf_text)
identificacion<- c()
d_aparente<- c()
c_hidraulica<- c()
for (i in 1:length(opinions)) {
  identificaciontemp<- gsub('^.*Identificación\\s*|\\s*\r\n.*$', '', opinions[[i]])
  identificacion<- c(identificacion,identificaciontemp)
  d_aparentetemp<- gsub('^.*Picnómetro\\s*|\\s*\r\n.*$', '', opinions[[i]])
  d_aparente<- c(d_aparente,d_aparentetemp)
  c_hidraulicatemp<- gsub('^.*constante\\s*|\\s*\r\n.*$', '', opinions[[i]])
  c_hidraulica<- c(c_hidraulica,c_hidraulicatemp)
}
def<- data.frame(ID= identificacion, `Densidad Aparente`= d_aparente, `Conductividad Eléctrica`= c_hidraulica)
def<- def %>% separate(Conductividad.Eléctrica, c("Conductividad eléctrica", "Categoría"), sep = "\\s+")
#SUELOS----
url<- "C:/Users/jrobledo/Desktop/Resultado muestras la Guajira/Suelos/"
files <- list.files(pattern = "pdf$", path= url)
files<- paste(url, files, sep="")
opinions <- lapply(files, extract_tables, encoding = "UTF-8")
info<- data.frame()
chemical_analysis<- data.frame()
rep<- 1 
for (i in 1:length(opinions)) {
  part_one_index<- seq(from=1, to = (length(opinions[[i]])-2), by=3)
  info_index<- seq(from=2, to = (length(opinions[[i]])-1), by=3)
  part_two_index<- seq(from=3, to = (length(opinions[[i]])), by=3)
  infotemp<- do.call(rbind, args = opinions[[i]][info_index])
  info<- rbind(info,infotemp)
  for (j in 1:length(part_one_index)) {
    id<- opinions[[i]][[info_index[j]]][1,2]
    temp<- rbind(opinions[[i]][[part_one_index[j]]],opinions[[i]][[part_two_index[j]]])
    if(id=="F7LP36-1-S24"){
      #F7LP36-1-S24 replicated sample
      id<- paste("F7LP36-1-S24", rep, sep = "_")
      rep<-rep+1
    }
    id<- rep(id, nrow(temp))
    temp<- as.data.frame(temp)
    temp$ID<- id
    chemical_analysis<-rbind(chemical_analysis,temp) 
  }
}
chemical_analysis<- chemical_analysis[chemical_analysis!="" & chemical_analysis!="DETERMINACIÓN ANALÍTICA",]
chemical_analysis<- na.omit(chemical_analysis)
chemical_analysis$V4<- as.numeric(chemical_analysis$V4)
chemical_wide<- dcast(chemical_analysis, formula = ID~V1, value.var = "V4", fun.aggregate = mean)
#F7LP36-1-S24 replicated sample: first in the documents =F7LP36-1-S24_1 second= F7LP36-1-S24_2
#F5LE7-1-S17 duplicated organic carbon
#F5LE7-2-S17 duplicated organic carbon & total nitrogen
#F2LE52-1-S8 duplicated total nitrogen
#F2LP45-1-S7 duplicated total nitrogen

#TEXTURA----
url<- "C:/Users/jrobledo/Desktop/Resultado muestras la Guajira/Textura/"
files <- list.files(pattern = "pdf$", path= url)
files<- paste(url, files, sep="")
opinions <- lapply(files, pdf_text)
identificacion<- c()
caracterisiticas<- data.frame()
i<-1
for (i in 1:length(opinions)) {
  identificaciontemp<- gsub('^.*IDENTIFICACIÓN:\\s*|\\s*ALTURA.*$', '', opinions[[i]])
  identificacion<- c(identificacion,identificaciontemp)
  caracteristicasTemp<- str_match_all(opinions[[i]], "Bouyoucos\\s*(.*?)\\s*\r\n")
  caracteristicasTemp <- data.frame(matrix(unlist(caracteristicasTemp), nrow=length(caracteristicasTemp), byrow=TRUE))
  caracteristicasTemp<- caracteristicasTemp[,((ncol(caracteristicasTemp)/2)+1):ncol(caracteristicasTemp)]
  caracterisiticas<- rbind(caracterisiticas, caracteristicasTemp)
}
colnames(caracterisiticas)<- c("Porcentaje de arena (% A)", "Porcentaje de arcilla (% Ar)", "Porcentaje de limo (% L)", "Clase textural" )
def<- data.frame(ID= identificacion, caracterisiticas)
#F7LP36-1-S24 two replicates






write.csv2(def, "c:/Users/jrobledo/Desktop/textura.csv", row.names = FALSE)

