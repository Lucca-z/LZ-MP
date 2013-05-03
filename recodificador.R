
recod <-function(ped,print.file=TRUE,out="pedigree.txt",print.list=FALSE){	
#===============================================================================================
#REFORMATEO E TESTES DO PEDIGREE ORIGINAL
  
  #Renomeando colunas em ped
	names(ped)<-c("id","pai","mae")
  
  #Definar as colunas como caracter
  ped$id<-as.character(ped$id)
  ped$pai<-as.character(ped$pai)
  ped$mae<-as.character(ped$mae)
	
  #Eliminar IDs que sejam NA
  ped<-ped[!is.na(ped$id),]

  #Eliminar linhas e  individuos repetidos.
  #linhas
  ped<-unique(ped)
  #individuos  
  ped1<-ped[!duplicated(ped$id),]
  rep<-ped[duplicated(ped$id),] 
  rep<-sort(unique(rep$id))
  
      #teste1 = Avalia se existem IDs repetidos com diferentes pais
	              # Os animais que apresetam este problema podem ser consultado no arquivo "id_pais_diferentes"
                # A solução padrão para este problema é definir os pais destes animais como desconhecidos.
      inf1<-NULL
      if(length(rep)>0){inf1<-paste(length(rep), "IDs repetidos com diferentes pais, os pais destes animais foram definidos como desconhecidos \n \t \t Para mais informações consulte o arquivo 'IDs_pais_dif'")}
      #colocar pais NA para animaris repetidos com diferentes ids de pais
      x<-ped1$id%in%rep
      ped1[x,2]<-NA
      ped1[x,3]<-NA
      ped<-ped1
  
	    #test2 = Avalia se ha animais que aparecem como pai e mae
	             #A slução padrão para este problema é definir o sexo do animal baseado na frequencia de aparição como pai ou mãe
      test<-na.exclude(unique(ped$pai[ped$pai%in%ped$mae]))
      inf2<-NULL
      if(length(test)>0) { inf2<-paste(length(test),"Individuos aparecen no pedigree como pai e mae, o sexo de animal sera definido pela maior frequencia de aparição como pai ou mae")
          #definir pai==mae como NA
          test<-as.character(test)
          p<-table(ped$pai)
          m<-table(ped$mae)
          gc()
          for(i in 1: length(test)) {
          fp<- p[names(p)==test[i]]
          fm<- m[names(m)==test[i]]
          if(fp >= fm) { ped$mae[ped$mae==test[i]]<-NA } else { 
          ped$pai[ped$pai==test[i]]<-NA} 
            }
          }	
      
      #test3 = Avalia se existem animais filhos de si mesmos
              # A solução padrao para este problema é definir o pai ou a mae segum seja o caso com NA
      id_mae<-na.exclude(ifelse(ped$id==ped$mae,1,0))
      id_pai<-na.exclude(ifelse(ped$id==ped$pai,1,0))
      inf3<-NULL 
      if(sum(id_mae)!=0 | sum(id_mae)!=0) {inf3<-paste( sum(c(id_mae, id_pai)), "Animais aparecem como pai ou mae de si mesmo")}
  	  ped$pai<-ifelse(ped$id==ped$pai, NA, ped$pai)
	    ped$mae<-ifelse(ped$id==ped$mae, NA, ped$mae)
      
      ped<-unique(ped)
  
#===========================================================================================================
#ORDENANDO IDs POR GERAÇÃO
  #Separação do pedigre em: População base, Nucleo e População sem descendencia

  #--------------------------------------------------------------------------------------------------------- 
  #POPULAÇÃO BASE
	#Indivíduos sem paes conhecidos pedigree original
	ped0<-ped$id[is.na(ped$pai)&is.na(ped$mae)]
	
	#Individuos sem paes conhecidos presentes nas colunas de pai e mãe
	paiBase<-ped$pai%in%ped$id ; 	paiBase<-ped$pai[!paiBase]
	  #removendo NAs
	  paiBase<-paiBase[!is.na(paiBase)]
  	#removendo registros duplicados
	  paiBase<-unique(paiBase)
	  
	maeBase<-ped$mae%in%ped$id ;	maeBase<-ped$mae[!maeBase]
	  #removendo NAs
	  maeBase<-maeBase[!is.na(maeBase)]
	  #removendo registros duplicados
	  maeBase<-unique(maeBase)
		
  #Formando população base
	ped0<-c(ped0,paiBase,maeBase)  # arquivo final populaçao base
	tped0<-length(ped0)  
	
	rm(maeBase,paiBase) ;  gc()
	
  #------------------------------------------------------------------------------------------------------
  # NUCLEO PEDIGREE

	#Separando individuos que são pais com ancestros conhecidos
	pedp<-ped$pai[ped$pai%in%ped$id]
	pedm<-ped$mae[ped$mae%in%ped$id]
	idpais<-c(pedp,pedm)
	
	#removendo registros duplicados
	idpais<-unique(idpais)
	#removendo NAs
	idpais<-idpais[!is.na(idpais)]
	
	#removendo duplicações com a população-base
	idpais<-idpais[!idpais%in%ped0]                                          
	
	rm(pedp,pedm); 	gc()
	
	#Ordenando os indivíduos que são pais - por geração
  gera<-2  # contador das gerações
	if(identical(length(idpais),as.integer(0))){
		idfilhos<-ped$id} else{
		pospais<-1:2 #Force to enter the while loop
		idfilhos<-NULL
		while(length(pospais)>1){
			# construindo as gerações
      gera<-gera+1
			subped<-match(idpais,ped$id)
          #Separando os pais dos pais
          ptemp<-match(ped$pai[subped],idpais)
			    ptemp<-ptemp[!is.na(ptemp)]
			    ptemp<-unique(ptemp)
			
			    mtemp<-match(ped$mae[subped],idpais)
			    mtemp<-mtemp[!is.na(mtemp)]
			    mtemp<-unique(mtemp)
			
			    pospais<-c(ptemp,mtemp) 
          if(length(pospais)==0) {idfilhos<-c(idpais,idfilhos)} else {
                                  idfilhos<-c(idpais[-pospais],idfilhos)}
			idpais<-idpais[pospais]
      
      if(gera>60) { cat("\n Problema de parentesco entre os animais \n" ); print(ped[subped,]); stop("\n ERRO PEDIGREE")}
		}
		rm(subped,ptemp,mtemp,pospais)
	}
	ped1<-c(idpais,idfilhos) # arquivo final nucleo pedigree
  
  rm(idpais,idfilhos) ; gc()

  #------------------------------------------------------------------------------------------------------
  # POPULAÇÃO SEM DESCENDENCIA	

	filhosp<-ped$id%in%ped$pai
	filhosm<-ped$id%in%ped$mae
	
	ped2<-ped$id[!filhosp & !filhosm]
  i<-ped2%in%ped0 # tirando animais que já estabam presentes na população base
  ped2<-ped2[!i] # arquivo final população sem descendencia

  tped2<-length(ped2)
	rm(filhosm,filhosp)
	
#===========================================================================================================
#RENUMERAR OS INDIVIDUOS COM BASE NA ORDEM DE GERAÇÃO E CRIANDO PEDIGREE RECODIFICADO
	vrecod<-c(ped0,ped1,ped2)
	vrecod_t<-length(vrecod)
  
  #mapa de recodificação
  mapa<-data.frame(id=vrecod, id_n=1:vrecod_t)
  
  #Montar a totalidade do pedigree original 
  pedp<-ped[, c(1,2)]
  pedm<-ped[, c(1,3)]
  pedt<-merge(mapa,pedp,by="id", all.x=T)
  pedt<-merge(pedt, pedm, by="id", all.x=T)
  pedt<-pedt[order(pedt$id_n),]
  
  #Construindo o pedigree recodificado
  id_n<-match(pedt$id,vrecod)
  pai_n<-match(pedt$pai,vrecod)
  mae_n<-match(pedt$mae,vrecod)
  pedrecod<-data.frame(id_n,pai_n,mae_n)

  rm(pedp, pedm, id_n, pai_n, mae_n)

#======================================================================================================	
#TESTES DE CONSISTENCIA DO PEDIGREE RECODIFICADO.
  #Estes erros se existenes param a execução do script.

	#Se os filhos tem recodificação maior que dos pais
	if(!all(pedrecod[,1]>pedrecod[,2]|pedrecod[,1]>pedrecod[,3],na.rm=TRUE)) stop("\n FALHA!\n Algum indiv<U+00ED>duo recebeu um c<U+00F3>digo maior que o c<U+00F3>digo de um de seus pais!\n")
		
	#Se tem pai na coluna de mae
	pai_mae_0<-pedrecod[!is.na(pedrecod$pai_n&pedrecod$mae_n),]	 
	if(all(pai_mae_0$pai_n%in%pai_mae_0$mae_n)) stop("\n FALHA! \n Algum pai foi tamb<U+00E9>m codificado como m<U+00E3>e ou vice-versa! \n")
	
	rm(pai_mae_0) ; 	gc()

#=====================================================================================================
#GERANDO INFORMAÇÕES PARA O RESUMO
  
  #Animais sem descendencia
  pais<-na.exclude(unique(c(ped$pai, ped$mae)))
  i<-ped0%in%pais
  sd_pb<-length(i[i==F]) # animais da população base que não tem descendencia
  tsd<-sd_pb+tped2   # Total animais sem descendencia
  
  #Total matrzes
  tmatriz<-length(na.exclude(unique(ped$mae)))
  #Total Reprodutores
  ttouro<-length(na.exclude(unique(ped$pai)))
  #Animais  com os dois  progenitores conhecidos
  ta2pc<-sum(ifelse(!is.na(ped$pai) & !is.na(ped$mae), 1,0))
  #Animais só com a mae conhecida
  tamc<-sum(ifelse(!is.na(ped$mae) & is.na(ped$pai), 1,0))
  #Animais só com pai conhecida
  tapc<-sum(ifelse(is.na(ped$mae) & !is.na(ped$pai), 1,0))

  #Tabela Resumo 
  
  item<-c("No. Total de individuos no pedigree","----------------", "No. Total de Matrizes", "No. Total de Reprodutores", "Animais Sem Descendencia", "----------------------",  "Animais Sem Pais Conhecidos", "Animais Só com o Pai Conhecido", "Animais Só com a Mãe Conhecida", "Animais com os Dois Pais Conhecidos", "--------------------", "No. de Gerações no Pedigree")
  valor<-c(vrecod_t,0, tmatriz, ttouro, tsd, 0, tped0, tapc, tamc, ta2pc, 0,  gera)
  resumo<-data.frame(INFORMACAO=item, VALOR=valor )

#==================================================================================================
#=ALERTA DE MENSAGENS IMPORTANTES=============
  #Estas mensagens não param a execução do script, porem tem que ser revisados

	cat("==============Mensagens=========== \n \n")
	cat(inf1, sep="\n")
	cat(inf2, sep="\n")
  cat(inf3, sep="\n")

#===============================================================================================
#VALOR RETORNO DA FUNÇÃO

	#Escreve arquivo txt com Formato MTDFREML e WOMBAT
  pedrecod[is.na(pedrecod[,2]),2]<-0
  pedrecod[is.na(pedrecod[,3]),3]<-0
	if(print.file){write.table(pedrecod,out,col.names=FALSE,row.names=FALSE)}
  
  #Retornar lista
	if(print.list){
	  list(Mapa=mapa,Pedigree.reco=pedrecod, IDs_pais_dif=rep, Resumo=resumo)
  }
}



