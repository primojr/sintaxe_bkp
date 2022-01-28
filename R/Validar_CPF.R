#
# Acerto do CPF -----
# OBJ: Paronizar para texto sem caracteres especiais
#
variavelCPF <- c("39.110.685-65","039.110.685-65","39606440","   222210  ", "     01", "000-000.000-20")

acertoCPF <- function(variavelCPF)
{# 1. Remover espaçamento no inicio e final do Texto
  library(dplyr); library(stringr)
  variavelCPF <- variavelCPF %>% str_trim(.) %>%
    str_remove_all(.,"\\.|-" )  %>% sprintf("%11s", .) %>% str_replace_all(.," ","0")  
  variavelCPF
} # Fim Acerto CPF

acertoCPF(Va)
#
# Função Validar o CPF ------
# Objetivo: Criar uma coluna na base dizendo se o cpf é valido, ou não.
# 
#
CriarValidacaoCPF <- function(Variavelcpf){
  cpf <- Variavelcpf
  # Validar o Primeiro Digito
  d1 <- (10*(as.numeric(substr(cpf,1,1))*10 +
               as.numeric(substr(cpf,2,2))*9 +
               as.numeric(substr(cpf,3,3))*8 +
               as.numeric(substr(cpf,4,4))*7 +
               as.numeric(substr(cpf,5,5))*6 +
               as.numeric(substr(cpf,6,6))*5 +
               as.numeric(substr(cpf,7,7))*4 +
               as.numeric(substr(cpf,8,8))*3 +
               as.numeric(substr(cpf,9,9))*2))%%11
  # Validar o Segundo DIGITO
  d2 <- (10*(as.numeric(substr(cpf,1,1))*11 +
               as.numeric(substr(cpf,2,2))*10 +
               as.numeric(substr(cpf,3,3))*9 +
               as.numeric(substr(cpf,4,4))*8 +
               as.numeric(substr(cpf,5,5))*7 +
               as.numeric(substr(cpf,6,6))*6 +
               as.numeric(substr(cpf,7,7))*5 +
               as.numeric(substr(cpf,8,8))*4 +
               as.numeric(substr(cpf,9,9))*3 +
               as.numeric(substr(cpf,10,10))*2))%%11
  d2 <- ifelse(d2 == 10,0,d2)
  # Criar Variavel
  verificar <- ifelse(as.numeric(substr(cpf,10,11))== as.numeric(paste0(d1,d2)), "Valido", "Invalido")
  verificar <- ifelse(cpf == "00000000000", "Invalido", verificar)
  verificar
} # FIM 
