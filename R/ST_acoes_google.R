##################### Analise de Série Temporal - GOOGLE #############################
### Banco de dados
 dados=read.table("table_mensal.csv",head=T)
 google = ts(dados,start=c(2004,8),frequency=12)
 google
 library(tseries)

############################ Descritiva ################################################
 plot(google, type="l",xlab="Ano",ylab="Fechamento (U$)")
 summary(google);sd(google)
 CV=(sd(google)/mean(google))*100;CV;sd(google)

# Decompondo a serie
 plot(decompose(google),xlab="Ano",sub="(a)")

# Testes para tendência e sazonalidade
 cox.stuart.test(google)   # Ultilizou implementação de Gilenio
 grupo=c(rep(seq(1,12),7),c(1,2))
 kruskal.test(google,grupo)   # Teste de Sazonalidade

# Primeira Diferenciação
 cox.stuart.test(diff(google,difference=1))
 grupo=c(rep(seq(1,12),7),1)
 kruskal.test(diff(google,difference=1),grupo)  # Teste de Sazonalidade 1o diferenciação

# Correlograma
 par(mfrow=c(1,2))
 acf(google,main="Série Real",sub="(b)")
 acf(diff(google,difference=1),main="1ª Diferenciação",sub="(c)")

#### Retirando os ultimos 12 meses
 base=google[-c(75:86)]
 base_comp=google[75:86]
 google2=ts(base,start=c(2004,8),frequency=12); google2
 google_comp=ts(base_comp,start=c(2010,10),frequency=12)

# Descritiva
 plot(google2, type="l",xlab="Ano",ylab="Fechamento (U$)",
      main="Serie Temporal do fechamento das ações da Google(2004-2010).")
 summary(google2);sd(google2)
 CV=(sd(google2)/mean(google2))*100;CV;sd(google2)

# Decompondo a serie
 plot(decompose(google2),xlab="Ano",ylab=c("obe","sas","sss","sss"))

# Testes para tendência e sazonalidade
 cox.stuart.test(google2)   # Ultilizou implementação fornecida em sala
 grupo=c(rep(seq(1,12),6),c(1,2))
 kruskal.test(google2,grupo) # Teste de Sazonalidade
 #13

# Primeira Diferenciação
 cox.stuart.test(diff(google2,difference=1))
 grupo=c(rep(seq(1,12),6),1)
 kruskal.test(diff(google2,difference=1),grupo)  # Teste de Sazonalidade 1ª diferenciação
 plot(diff(google2,difference=1))

########################### Ajuste por HW ##################################
 ajuste1=HoltWinters(google2) #previs~ao considerando sazonalidade aditiva
 VF1=predict(ajuste1,12)
 ajuste2=HoltWinters(google2, seasonal="multiplicative") # previsão considerando

# sazonalidade multiplicativa
 VF2=predict(ajuste2,12)

# Gráfico do ajuste pelo HW - comparar os ajustamentos
 par(mfrow=c(1,2))
 plot(ajuste1,VF1,xlab="Ano",ylab="Fechamento (U$)",
      main="",sub="(a)");lines(google,type="l",lwd=1,col="black")
      leg.txt=c("Valor real", "HW-Aditivo"); cores=c("black","red")
      legend(list(x=2007.4,y=150), legend = leg.txt, col=cores, lwd=2, merge=FALSE)

plot(ajuste2,VF2,xlab="Ano",ylab="Fechamento (U$)",
     main="",sub="(b)");lines(google,type="l",lwd=1,col="black")
     leg.txt=c("Valor real", "HW-Multiplicativo"); cores=c("black","red")
     legend(list(x=2007,y=150), legend = leg.txt, col =cores, lwd=2, merge=TRUE)

# Matriz com os valores predito e real
 predA= ts(VF1,start=c(2010,10),frequency=12)
 predB= ts(VF2,start=c(2010,10),frequency=12)
 testHW=data.frame(cbind(google_comp,predA,predB))

#################### Ajuste do Modelo SARIMA #####################
### Ajuste do modelo ARIMA(p,d,q)x(P,D,Q)
 arima.bic=arima
 fix(arima)
 y=google2 # Série utilizada
 lag.entrada=c(1,2) # Variavéis de Entrada
 prev=12
 l=max(lag.entrada) # Numero máximo de defasagens
 n=length(y) # Numero de observações

#### Contruindo a matriz de entrada Y
# Constroi a matriz de entrada para os lags especificos antes
 Y=matrix(nrow=length(y),ncol=length(lag.entrada))
 for(m in 1:length(lag.entrada))
   {
    e=lag.entrada[m]
     for(i in 1:length(y))
       {
        if(i<=e) for(a in 1:e){Y[a,m]=0}
        else Y[i,m]=y[i-e]
   }
 }


### Ajuste do modelo ARIMA
 y.teste=y[(n-prev+1):n] # Dados de teste
 X.teste=Y[(n-prev+1):n,]
 y=y[(l+1):(n-prev)]
 X=Y[(l+1):(n-prev),] ## Matriz do modelo apos eliminac~ao dos zeros

###### Selec~ao do modelo ARIMA(p,1,q)x(0,1,1)-original ##########
#d=1, Com a primeira diferenciacc~ao obteve a sserie estacionaria
 selecao.de.modelos <- function(serie=y, p.max=2,q.max=2,d=1,P=0,D=0,Q=0){
   M<-matrix(0,p.max+1,q.max+1) ## Matriz para armazenar os resultados
   if(P==0 && Q==0){
    for(i in 0:p.max){
      for(j in 0:q.max){
        if(i==0 && j==0) M[1,1]<-NA
        else M[i+1,j+1]<-arima(serie, order=c(i,d,j), seasonal=list(order=c(P,D,Q)))$aic #=BIC
      }
    }
  } else {
    for(i in 0:p.max) {
      for(j in 0:q.max){
        M[i+1,j+1]<-arima(serie,order=c(i,d,j),seasonal=list(order=c(P,D,Q)))$aic
       }
     }
   }
    return(M)
  }
 M=selecao.de.modelos(google2)
 p.selec=which(M==min(M),arr.ind=TRUE)[1];p.selec
 q.selec=which(M==min(M),arr.ind=TRUE)[2];q.selec
# Modelos Selecionados
# a obtenção do modelos se deu da seguinte forma:
# atribuiu valores 0,1 para os parametros d,P,Q,D
# e vez todas as possiveis compinações para os
# os valores de p,q variando entre 1 e 3.
# e seleciondos os modelos com menor BIC
 mod1=arima(google2,order=c(2,0,1),seasonal=list(order=c(1,1,0)))
 mod4=arima(google2,order=c(2,1,1),seasonal=list(order=c(1,1,0)));mod4
 mod2=arima(google2,order=c(1,1,1),seasonal=list(order=c(1,1,0)));mod2
 mod3=arima(google2,order=c(1,1,1),seasonal=list(order=c(1,0,1)));mod3
 mod5=arima(google2,order=c(2,1,1),seasonal=list(order=c(1,0,1)));mod5
 names(mod2)

## Previs~ao
 y.prev1=predict(mod2,se.fit=FALSE,n.ahead=prev)
 y.prev2=predict(mod4,se.fit=FALSE,n.ahead=prev)

###Gráfico da série e das previsões
 par(mfrow=c(1,2))
 # SARIMA 2
 a1=mod2[["residuals"]]+google2
 predA1=c(a1,y.prev1s)
 predA1s=ts(predA1,start = c(2004,8), frequency = 12)
 plot(predA1s,col="2",xlim=c(2004,2012),ylab="Fechamento (U$)",xlab="Ano",sub="(a)")
      lines(google); abline(v=2010.9,lty=2)
      leg.txt=c("Valor real", "SARIMA2"); cores=c("black","red")
      legend(list(x=2004,y=500), legend = leg.txt, col =cores, lwd=2, merge=FALSE)

# SARIMA 4
  a2=mod4[["residuals"]]+google2
  predA2=c(a2,y.prev2s)
  predA2s=ts(predA2,start = c(2004,8), frequency = 12)
  plot(predA2s,col="2",xlim=c(2004,2012),ylab="Fechamento (U$)",xlab="Ano",sub="(b)")
  lines(google); abline(v=2010.9,lty=2)
  leg.txt=c("Valor real ", "SARIMA4 "); cores=c("black","red")
  legend(list(x=2004,y=500), legend = leg.txt, col =cores, lwd=2, merge=FALSE)

# Previsão - 12 passos
  plot(y.test1,xlab="Ano",lwd=1,ylab="Fechamento (U$)",ylim=c(250,500))
      lines(VF1,lwd=1, col="blue")
      lines(VF2,lwd=1,col="purple")
      lines(y.prev1s,lwd=1,col="red")
      lines(y.prev2s,lwd=1,col="orange")
      leg.txt=c("Valor real", "Valor predito HW Aditivo",
                "Valor predito HW Mutiplicativo",
                "Valor predito SARIMA 2",
                "Valor predito SARIMA 4" )
      cores=c("black", "blue", "purple","red","orange")
      legend(list(x=2011,y=320), legend = leg.txt, col=cores, lwd=1, merge=TRUE)

## Diagnóstico
 tsdiag(mod2)
 tsdiag(mod4)

# SARIMA
 sse1=sum((google_comp-y.prev1s)^2)
 mse1=mean((google_comp-y.prev1s)^2)
 sse2=sum((google_comp-y.prev2s)^2)
 mse2=mean((google_comp-y.prev2s)^2)

#Holt-Winteres
 sse.h=sum((google_comp-VF1)^2)
 mse.h=mean((google_comp-VF1)^2)
 sse.h2=sum((google_comp-VF2)^2)
 mse.h2=mean((google_comp-VF2)^2)
 aba=matrix(c(sse1,mse1,sse2,mse2,sse.h,
              mse.h,sse.h2,mse.h2),ncol=2,byrow=T)
######################### FIM ###################################
