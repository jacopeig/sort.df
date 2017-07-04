# data frame resultado de combinar dos data frames
# este ultimo estara ordenado por elementos, es decir:
# coje un elemento de una fila pivote y le asigna a la columna sus valores
# de los respectivos data frames

createdf.epivot<-function(df1, df2, c1 = NULL, c2 = NULL){
  if (!(is.data.frame(df1))) stop("No data frame as argument")
  if (!(is.data.frame(df2))) stop("No data frame as argument")
  
  # por defecto las columnas a cojer pivote seran la 1.
  
  if (is.null(c1)) c1<-1
  if (is.null(c2)) c2<-1
  
  #
  cc1<-class(df1[,c1])
  cc2<-class(df1[,c2])
  cn1<-colnames(df1)
  cn2<-colnames(df2)
  
  DF1<-as.matrix(df1)
  DF2<-as.matrix(df2)
  
  # Para despues, habra que mirar si los que tienen mas de un elemento
  # son los elementos iguales, con lo cual se podria arreglar
  
  if (length(levels(as.factor(DF1[,c1]))) != length(DF1[,c1])) stop ("More than one element")
  if (length(levels(as.factor(DF2[,c2]))) != length(DF2[,c2])) stop ("More than one element")
  
  v<-as.character(c(DF1[,c1],DF2[,c2]))
  
  #crear el vector con el numero de columnas, excepto las columnas c1 y c2
  #parece que va
  #se utiliza "seq" en vez se "c" porque al sumar, con c suma 1 unidad entonces los limites varian
  
  if (c1==1){
    w1<-seq(c1+1:ncol(df1))
  }else if (ncol(df1)){
    w1<-seq(1:ncol(df1)-1)
  }else if (c1>1 & c1<ncol(df1)){
    w1<-c(seq(1,c1-1),seq(c1+1:ncol(df1)))  
  }else{
    stop("Out of range")
  }
  
  
  if (c2==1){
    w2<-seq(c2+1:ncol(df2))
  }else if (ncol(df2)){
    w2<-seq(1:ncol(df2)-1)
  }else if (c1>1 & c1<ncol(df2)){
    w2<-c(seq(1,c2-1),seq(c2+1:ncol(df2)))  
  }else{
    stop("Out of range")
  }
  
  return(c(c1,c2,length(DF1[,c1]),length(DF2[,c2]), v,w1,w2))
}
