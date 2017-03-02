###########################################
# Create a new data frame sorted by a word
###########################################
#' @author Jacobo Perez Iglesias (\email{jacopeig@gmail.com}).
sort.df<-function(df,word,columns){
	if(!is.data.frame(df)) stop("No data frame as argument")
	if(!is.character(word)) stop("No character as argument")
	if(!is.numeric(columns)) stop("Not valid number of columns")
	if(max(columns)>ncol(df) | min(columns)<0) stop("out of range")	


	A<-matrix(0,nrow=nrow(df),ncol=length(columns))
	
	for (j in 1:length(columns)){
		for(i in 1:nrow(df)){
			if(is.na(pmatch(word,df[i,columns[j]]))){
				A[i,j]<-0
			}else{
				A[i,j]<-1
			}
		}
	}

	dff<-as.data.frame(A)
	
	e<-0
	for(i in 1:nrow(dff)){
		e[i]<-sum(dff[i,])
		if (e[i]>1) stop("Two repeated words in several columns")
	}
	
	for (i in 1:ncol(dff)){
		df[,columns[i]]<-as.character(df[,columns[i]])
	}
	a<-0
	for (j in 1:length(columns)){
		for (i in 1:nrow(df)){
			if(dff[i,j]==1){
				a<-df[i,columns[1]]
				df[i,columns[1]]<-df[i,columns[j]]
				df[i,columns[j]]<-a
			}
		}
	}
	return(df)
}
