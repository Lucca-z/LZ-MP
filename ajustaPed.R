ajustaPed <-
function(ped,arquivo.saida="pedigree3cols.txt"){
	
	if(length(ped)==3){cat("N<U+00E3>o h<U+00E1> nada a fazer! O pedigree possui 3 colunas\n")}
	else{if(length(ped)<3){cat("N<U+00E3>o h<U+00E1> colunas suficientes no pedigree! Verifique novamente o arquivo.\n")}
		else{	
			#Calculando o n<U+00FA>mero de gera<U+00E7><U+00F5>es (de acordo com Daniel Watanabe)
			k<-((length(ped)-1)/2)+1
			t<-log2(k)
			
			rm(k)
			
			ped1<-ped[,-1]
			ped<-ped[,1:3]
			names(ped)<-c("id","pai","m<U+00E3>e")
			
			#Calculando o total de ancestrais na pen<U+00FA>ltima gera<U+00E7><U+00E3>o
			ancp<-(length(ped1))-2^t
			
			for(i in 1:ancp){
			
				pedtemp<-data.frame(id=ped1[,1],pai=ped1[,i+2],mae=ped1[,i+3],stringsAsFactors=FALSE)
				
				ped<-rbind(ped,pedtemp)
				
				#Removendo duplica<U+00E7><U+00F5>es
				ped<-unique(ped)
				#Removendo indiv<U+00ED>duos n<U+00E3>o identificados
				ped<-ped[!is.na(ped[,1]),]
				
				ped1<-ped1[,-1]
			}
			
			rm(ancp,pedtemp)
			gc()
			
			c1<-NULL
			for (i in 1:(2^t-1)){
				c1<-c(c1,ped1[,1])
				
				#Removendo duplica<U+00E7><U+00F5>es
				c1<-unique(c1)
				#Removendo indiv<U+00ED>duos n<U+00E3>o identificados
				c1<-c1[!is.na(c1)]
				
				ped1<-ped1[,-1]
			}
			
			c1<-c(c1,ped1)
			
			rm(ped1)
			gc()
			
			#Removendo duplica<U+00E7><U+00F5>es
			c1<-unique(c1)
			#Removendo indiv<U+00ED>duos n<U+00E3>o identificados
			c1<-c1[!is.na(c1)]
			
			c2<-rep(NA,length(c1))
			c3<-rep(NA,length(c1))
			
			pedtemp<-data.frame(id=c1,pai=c2,mae=c3,stringsAsFactors=FALSE)
			
			rm(c1,c2,c3)
			gc()
			
			ped<-rbind(ped,pedtemp)
			
			rm(pedtemp)
			gc()
			
			#Removendo duplica<U+00E7><U+00F5>es
			ped<-unique(ped)
			
			write.table(ped,arquivo.saida,row.names=FALSE,col.names=FALSE,sep=" ")
		
			ped
		}
	}
}

