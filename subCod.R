subCod<-function(x,y,cols=1,file="testeSubCod.txt"){
	for(i in 1:length(cols)){
		c<-0
		s<-match(y[,cols[i]],x[[1]][,1+c]) #x[[1]]=mapa de recodificação
		y[,cols[i]]<-x[[1]][s,c+2]
		c<-c+2
	}
	y<-y[!is.na(y[,cols[1]])] #Removendo do arquivo de dados indivíduos que não estão listados no arquivo de pedigree
	
	write.table(y,file=file,row.names=FALSE,col.names=FALSE)
}




