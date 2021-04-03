library(pdftools)
library(stringr)

files <- list.files(pattern = "pdf$")
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
  colnames(otros)<- c("P", "K", "Ca", "Mg", "Na", "S", "Fe", "Cu","Mn", "Zn")
  faltantes<- rbind(faltantes,otros)
  borotemp<- gsub('^.*VIS\\s*|\\s*\r\n.*$', '', opinions[[1]])
  boro<- c(boro,borotemp)
}

# identificacion<- gsub('^.*Identificación:\\s*|\\s*Matriz.*$', '', opinions[[1]])
# nitrogeno<- gsub('^.*Kjeldahl/Vo lumetría\\s*|\\s*\r\n.*$', '', opinions[[1]])
# otros<- str_match_all(opinions[[1]], "Espectro metría de emisió n de\\s*(.*?)\\s*\r\n")
# otros <- data.frame(matrix(unlist(otros), nrow=length(otros), byrow=TRUE))
# otros<- otros[,((ncol(otros)/2)+1):ncol(otros)]
# colnames(otros)<- c("P", "K", "Ca", "Mg", "Na", "S", "Fe", "Cu","Mn", "Zn")
# boro<- gsub('^.*VIS\\s*|\\s*\r\n.*$', '', opinions[[1]])

def<- data.frame(ID= identificacion, N= nitrogeno, otros,  B= boro)







