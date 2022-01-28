
### Modelo credHermes
# Base - sem criação das dummy
credhermes <- read.table("~/cred_hermes.csv", sep=";", quote="\"")
names(credhermes) <- c("nocliente","cpf", "sexo","est_civil","profissao",
                       "uf","cep","cep3","telefone","qtd_fixo","qtd_movel",
                       "qtd_trab","qtd_rec","dt_serasa","st_hoje","limite_hj",
                       "ped_dist","ped_total","renda_he","melhor_dia","idade",
                       "fx_idade","regiao_uf","localizacao_cidade","perfil","qtd_t",
                       "qtd_bom","qtd_mau","mx_dt_boleto","mi_dt_boleto","mx_dias_atraso",
                       "mi_dias_atraso","fx_d_atraso","conj","mae","pai","qtd_filho",
                       "media_boleto","max_boleto")

View(credhermes)
credhemes <- credhemes %>%
      mutate_if(is.character, ~ str_to_lower(.)) %>% 
      mutate_if(is.character, ~ rm_accent(.)) %>% 
      mutate_if(is.numeric,   ~ replace_na(.,0))
  
#
## BASE NORTE
#
cred_norte <- credhermes %>% filter(regiao_uf %in% c("NORTE","NORDESTE"))
write.csv(cred_norte,"~/cred_norte.txt", row.names=TRUE)
  
#  
## BASE SUL
#
cred_sul <- credhermes %>% filter(regiao_uf %notin% c("NORTE","NORDESTE"))  
write.csv(cred_sul,"~/cred_sul.txt", row.names=TRUE)

#
# Fechar data.frame 
#
rm(credhermes);rm(cred_sul) # Fechar as bases não ultilidas
View(cred_norte)

# V.a STATUS
cred_norte <- mutate(cred_norte, status = if_else(perfil == 'BOM', 1,0))
cred_norte %>% count(status)

# Gerar base treino e teste

ale <- runif(length(cred_norte$nocliente),0,1)
cred_norte=data.frame(ale,cred_norte)# Agregar a base as variaveis criadas 
write.csv(cred_norte,"~/cred_norte.txt", row.names=TRUE)
rm(ale) # Limpa da memorias as variáveis

#### SELEÇÃO DA BASE DE TREINO
cred_norteBOM <- filter(cred_norte, status == 1)
cred_norteMAU <- Filter(cred_norte, status == 0)
cred.norte_dev1 <- cred_norteBOM[runif(119807, 1, nrow(cred_norteBOM)),]
cred.norte_dev2 <- cred_norteMAU[runif(119807, 1, nrow(cred_norteMAU)),]
cred.norte_dev <- rbind(cred.norte_dev1,cred.norte_dev2)
write.csv(cred.norte_dev,"~/cred_norte.txt", row.names=TRUE)
rm(cred.norte_dev1,cred.norte_dev2,cred_norteBOM,cred_norteMAU,cred_norte)

# Retirar o attach
# detach(credhermes)
# Limpar a area de trabalho
# rm(list = ls(all = TRUE))

##########################################
### CEP
View(cred.norte_dev)
table(cred.norte_dev$cep3)

# Calcula a média e quantidade de clientes, agrupado por cep3
agregar.cep <- 
  cbind(
    aggregate(
        cred.norte_dev$status
        ,by=list(cred.norte_dev$cep3)
        ,FUN=mean
      )
      ,"Soma" = aggregate( 
                   cred.norte_dev$status
                  ,by=list(cred.norte_dev$cep3)
                  ,FUN=sum
                  )[,2]
    )

write.table(agregar.cep,file = "agregar.cep.csv", sep = ";",
            col.names = c("cep","media","soma"),qmethod = "double")
str(agregar.cep)

rank(agregar.cep,ties.method= "random")
#### fim CEP
#######################################


# ### Base de dados - Com as dummy criadas no SPSS
# credRs <- read.delim("~/credRs.dat")
# str(credRs)
# attach(credRs)


#########################################
############  Programas auxiliares #####
### Gráfico KS e acuracia
graf.Ks <- function(mod,corte){
  # mod = Modelo ajustado
  # corte = ponto de corte
  # Passo 1 - Calculo da acuracia  
  B=table(mod$y,cut(mod$fitted.values,breaks=c(0,corte,1)))
  sen.mod=B[1,1]/(sum(B[1,]));esp.mod=B[2,2]/(sum(B[2,]))
  acur.mod=(B[1,1]+B[2,2])/sum(B)
  result = cbind(c("Acuracia"=acur.mod,"esp"=esp.mod,"sen"=sen.mod))
  # Passo 2 - Caculo do KS  
  percentil=cbind("valor"=quantile(mod$fitted.values,probs =seq(0,1,.1)))
  class=cut(mod$fitted.values,breaks=percentil)
  base=data.frame(rbind(table(class,mod$y)))
  freq.acum0=cumsum(base$X0/sum(base$X0))
  freq.acum1=cumsum(base$X1/sum(base$X1))
  KS.null=freq.acum0-freq.acum1; KS=max(KS.null)
  text.plot=round(rbind(result,"KS"=max(KS.null)),3)
  # Passo 3 Gráfico
  plot(freq.acum0,type="b",col="red", xlab="Classes",ylab="Freq. Acumulada",
       main="Curva KS")
  lines(freq.acum1,col="blue",type="b");lines(KS.null,lty=2)
  legend(1,1,legend=c("Mau", "Bom","KS.null"),cex=.8,
         lty=c(1,1,2), col=c("red","blue",1), pch=c(1,1,1), bty="n")
  mtext(side=3, at=par("usr")[3], adj=-.3,
        cex=.8, col="gray40", line=-1,
        text=paste("Acu =",text.plot[1],"-- Sen =",text.plot[3],
                   "-- Esp =",text.plot[2],"-- Ks =",text.plot[4]))
} # Fim - graf.KS

# Programa para calculo do IC da odds
IC_odds<-function(mod,alpha){
  # mod = Modelo austado através do gml
  # alpha = Nivel de significância
  sum.mod=summary(mod)
  cof=li=ls=COFF=VAR=odds=li.odds=ls.odds=NULL
  for(i in 1:length(mod$coefficients)){
    COFF[i]=sum.mod$coefficients[i];VAR[i]=sum.mod$cov.unscaled[i,i]
    li[i] <- COFF[i]-qnorm(1-alpha/2)*sqrt(VAR[i])
    ls[i] <- COFF[i]+qnorm(1-alpha/2)*sqrt(VAR[i])
    odds[i]=exp(COFF[i]);li.odds[i]=exp(li[i]); ls.odds[i]=exp(ls[i])
    cof[i]=names(mod$coefficients[i])
  }
  resultado=data.frame("Coeficiente"=cof,"ODDS"=odds,"Limite Inferior"=li.odds,"Limite Superior"=ls.odds)
  resultado
}

# Pacotes Ultilizados
library(descr)# Auxulio a analise descritiva
library(ROCR) # Curva ROC
###################### Fim #############


# Modelo teste

modt = glm(STATUS ~ dcep31 + dcep32 + dcep33 + dcep36 + dcep37 + dcep38 + 
              d_idade4 + d_idade5 + dsexo1 + destcivil1
            ,family=binomial)
summary(modt)
plot(modt)
anova(modt)
graf.Ks(modt,.5)
rm(mod.t)


########## MODELO 0 Todas as Variaveis (
mod0 = glm(STATUS ~ dcep31 + dcep32 + dcep34+ dcep35 
                  + dcep36 + dcep37 + dcep38 + d_idade1
                  + d_idade2 + d_idade3 + d_idade5 +d_idade4 
                  + dsexo1 + D_PROF + D_RENDA + destcivil1
          ,family=binomial)
summary(mod0)
anova(mod0)
step.mod0 <- step(mod0)
stepwise(mod0.10,.05)
layout(matrix(c(1,2,3,4),2,2)) # optional 4 graphs/page
plot(mod0)

step.mod0$fitted.values
summary(step.mod0)
A0=table(STATUS,cut(mod0$linear.predictors,breaks=c(0,.5,1)))
sen.mod0=A0[1,1]/(sum(A0[1,]));
esp.mod0=A0[2,2]/(sum(A0[2,]));
acur.mod0 = (A0[1,1]+A0[2,2])/sum(A0)
rbind("Acuracia"=acur.mod0,"esp"=esp.mod0,"sen"=sen.mod0)


########## MODELO 1 com Renda e ped_total (
mod1 = glm(STATUS ~ dcep31 + dcep32 + dcep36 + dcep37 + dcep38
           + d_idade1 + d_idade5 +d_idade4 + dsexo1 + D_PROF + D_RENDA1 + D_RENDA2
           ,family=binomial)
summary(mod1)
plot(mod1)
anova(mod1)
    
#Curva de ROC
pp.mod1 <- mod1$fitted.values
ll <- mod1$y
pred <- prediction(pp.mod1,ll)
perf <- performance(pred,'tpr','fpr')
# par(mfrow=c(2,2)) 
plot(perf, colorize=T, lwd=2,main='ROC curves from 10-fold cross-validation')
plot(perf, avg='vertical', spread.estimate='stderror',lwd=3,main='Vertical averaging + 1 standard error',col='blue')
plot(perf, avg='horizontal', spread.estimate='boxplot',lwd=3,main='Horizontal averaging + boxplots',col='blue')
plot(perf, avg='threshold', spread.estimate='stddev',lwd=2, main='Threshold averaging + 1 standard deviation',colorize=T)


graf.Ks(mod1,.5)

########## MODELO 2 sem Renda e ped_total (
mod2 = glm(STATUS ~ dcep31+dcep32+dcep36+dcep37+dcep38+d_idade1
                    +d_idade4+d_idade5+dsexo1+D_PROF
         ,family=binomial)
summary(mod2)
anova(mod2)
IC_odds(mod2,0.05)

#Curva de ROC
pp.mod2 <- mod2$fitted.values
ll <- mod2$y
pred <- prediction(pp.mod2,ll)
perf <- performance(pred,'tpr','fpr')
# par(mfrow=c(2,2))
plot(perf, colorize=T, lwd=2,main='ROC curves from 10-fold cross-validation')
plot(perf, avg='vertical', spread.estimate='stderror',lwd=3,main='Vertical averaging + 1 standard error',col='blue')
plot(perf, avg='horizontal', spread.estimate='boxplot',lwd=3,main='Horizontal averaging + boxplots',col='blue')
plot(perf, avg='threshold', spread.estimate='stddev',lwd=2, main='Threshold averaging + 1 standard deviation',colorize=T)
#) Fim - MOD2

graf.Ks(mod2,.5)

fwf2csv(status)

########## MODELO 2 sem Renda e ped_total (
mod3 = glm(STATUS ~ dcep31+dcep32+dcep36+dcep37+dcep38+d_idade1
           +d_idade4+d_idade5+dsexo1
           ,family=binomial)
summary(mod3)
anova(mod3)
graf.Ks(mod3,.5)
IC_odds(mod2,0.05)



### Calcular Acuracia
acuracia<-function(mod,corte){
  B=table(STATUS,cut(mod$fitted.values,breaks=c(0,corte,1)))
  sen.mod2=B[1,1]/(sum(B[1,]))
  esp.mod2=B[2,2]/(sum(B[2,]))
  acur.mod2=(B[1,1]+B[2,2])/sum(B)
result = c("Acuracia"=acur.mod2,"esp"=esp.mod2,"sen"=sen.mod2)
result
}

acuracia(mod3,.9)
acuracia(mod2,.9)
