#####
# Criar uma função de dredge #
#####

f.dredge=function(dados){
  resposta=dados[,1]
  explanatoria=dados[,-1]
  n=ncol(explanatoria)
  
  nobs.mod=numeric(1)
  
  guardar=numeric()
  combinacoes=numeric(1)
  
  for(z in 1:n){ #for de combinações - quantos modelos possiveis
    guardar[z]=choose(n,z)
  }
  combinacoes=sum(guardar)
  
  logLik.mod=matrix(NA, nrow=combinacoes,ncol=2) #Qtd. de modelos determina linhas de matrizes de saida
  resultados=matrix(NA, nrow=combinacoes, ncol=3)
  
  contador=1
  
  for(i in 1:n){ #for do tamanho da quantidade de variaveis
    expla.vars=combn(names(explanatoria), i) #todas combinações possiveis, diferentes colunas
    modelos=matrix(NA, nrow=1, ncol=ncol(expla.vars))
    
    for(j in 1:nrow(modelos)){
      for(l in 1:ncol(modelos)){ #2 fors para salvar os modelos acima
        
        modelos[j,l]=paste(expla.vars[,l], collapse="+") #salvando os nomes das variaveis dos modelos
        regression=paste0("resposta", "~", modelos[j,l]) #formula da regressão
        logLik.mod[contador,1]=as.numeric(logLik(lm(as.formula(regression), data=dados))) #LogLik dos modelos
        logLik.mod[contador,2]=i+1 #salvando k
        nobs.mod=nobs(lm(as.formula(regression), data=dados)) #salvando n 
        
        contador=contador+1
        
      }
    }
    
    LL=logLik.mod[,1]
    k=logLik.mod[,2]
    
    AIC.t= -(2*LL)+(2*k)
    resultados[ ,1] = AIC.t+(2*k*(k+1))/(nobs.mod-k-1) # AICc salvar na coluna 1
    resultados[ ,2]=(resultados[,1]-(min(resultados[,1]))) # Delta Aic salvar na coluna 2
    resultados[ ,3]=exp(-1/2*resultados[,2])/(sum(exp(-1/2*resultados[,2]))) # W.aic salvar na coluna 3
    
    matriz.final=cbind(logLik.mod, resultados)
    colnames(matriz.final)=c("LL", "k", "AICc", "∆AIc", "WAic")
    matriz.final=matriz.final[order(matriz.final[,4], matriz.final[,5],decreasing=F),] #ordenando DeltaAIc primeiro e depois WAIc
  }
  
  return(matriz.final)
}


#####
# Usando a função
#####

dados=read.table("/path/dados.txt", h=T)
f.dredge(dados)

