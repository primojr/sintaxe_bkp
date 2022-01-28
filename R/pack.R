#
### 01. Instalação do pacotes
#
list_pkt <- c("DBI","sqldf","stringr", "dplyr","ggplot2", "readxl","RODBC","e1071","forecast",
              "tau","devtools","stringi","lubridate","zoo","plotly","DT","XML","rlang","mafs",
              "readr","tidyr","tibble","magrittr","stats","shiny","caret","nnet","tidyquant",
              "patchwork","janitor",'abjutils','data.table')
list_pkt_install <- list_pkt[!list_pkt %in% rownames(installed.packages())]			 

#
#
if(length(list_pkt_install) == 0) {print("Todos packages instalados")} else {install.packages(list_pkt_install)}

#
### 02. Carregar pacotes
#
for( i in 1:length(list_pkt)){
  library(list_pkt[i] , character.only = TRUE)   
}

`%notin%` <- Negate(`%in%`)

# 
### 03. Remover variavel de instalação
# 
rm(i,list_pkt, list_pkt_install)
ls()
