# Diretorios&Pacotes
rm(list = ls())
source("~Documents/_R/Script/Apoio/pack.R")
#source("~GERSON/01_DBM/Script R/")
setwd("~Documents/_GERSON/ML")

## 
# 00. LOOP PARA CARREGAR AS BASES CONSOLIDADAS ----
##
fsn = list.files(recursive = TRUE) %>% 
  .[grepl("csv", .)==TRUE] %>%
  .[str_detect(.,"/2018") == FALSE] #%>% .[str_detect(.,"Mexico") == TRUE]

ml = NULL #1:length(fsn)
for(k in 1:length(fsn)){
  ml.ar0 <- read_csv(fsn[k]) %>%
    select(.,
           Categoria_Nivel_1,Categoria_Nivel_2,Categoria_Nivel_3,
           Categoria_Nivel_4, Categoria_Nivel_5, Categoria_Nivel_6,
           Categoria_Completa, Codigo_de_Publicacion,Titulo_Publicacion,
           Nickname_Vendedor,Categoria_del_Vendedor,PrecioUsd, Precio_Original,
           PrecioMonedaLocal,Moneda,Unidades_Vendidas, Monto_Vendido_Moneda_Local,
           Monto_Vendido_USD, Provincia, Ciudad, Mes, Foto_Publicacion, Link_a_Publicacion
           ,Marca
    ) %>% mutate(., pais = sub("/.*","", fsn[k])) #, Marca = 'Ano2018')
  ml = rbind(ml.ar0,ml)
}
rm(ml.ar0) 
ml = as_tibble(ml)

#write.csv2(ml.pais, "dadosanalise.csv", row.names=FALSE)
#ml = ml %>% mutate(amostra = sample(2,nrow(.), replace = TRUE, prob = c(.9,.1))) %>% filter(amostra == 2)

fwrite(ml, "~Documents/_R/Data.frame/ml_cru.csv",sep = ";")

#
# 01. RECATEGORIZAÇÃO DO SEGEMENTO parte.1 ----
#
# 01.1 DESCARTAR 
filter_seg = ml %>%
   filter(str_detect(Categoria_Nivel_3, 'Escalera|Tanq|Home|Paint') == TRUE) %>% distinct(Codigo_de_Publicacion)
ml = ml %>% filter(Codigo_de_Publicacion %notin% filter_seg$Codigo_de_Publicacion)
rm(filter_seg)

# 01.2 NOVA CLASSIFICAÇÃO
ml = ml %>% mutate(
   Categoria_Nivel_3 = case_when(
      str_detect(Categoria_Nivel_3, "^[Aa]c") ~ 'Accessories'
      ,str_detect(Categoria_Nivel_3, "Man") &
         str_detect(Categoria_Nivel_5,'[Ss]et|[Kk]it|Jog|Jueg|Estojo') ~ 'Accessories'
      ,str_detect(Categoria_Nivel_3, "El[eé]|[Ii]nd|^Amo|^Ator|[Mm]arti") ~  'PowerTools'
      ,str_detect(Categoria_Nivel_3, "Medi") ~ 'Measuring Tools'
      ,str_detect(Categoria_Nivel_3, "Org") ~ 'Toolboxes'
      ,str_detect(Categoria_Nivel_3, "Man") ~ 'Manual Tools'
      ,str_detect(Categoria_Nivel_3, "Jard")  ~ 'Home&Garden'
      ,str_detect(Categoria_Nivel_3, "Otr|Out") ~ 'Others'
      ,str_detect(Categoria_Nivel_3, "Pint")  ~ 'Painting'
      , TRUE ~ 'Others'
   )
)

#
# 02 PADRONIZAR A CATEGORIA DE FERRAMENTAS ----
# Categoria nivel 4 = categoria nivel 5 
#
ml = ml %>%
   mutate(
      Categoria_Nivel_5 = Categoria_Nivel_5 
      %>% if_else(Categoria_Nivel_5 == '-', Categoria_Nivel_4,. )
      %>% if_else(. == Categoria_Nivel_4, ., str_c(Categoria_Nivel_4, ., sep = " "))
      %>% str_to_lower()
      )



# ml %>% tabyl(Categoria_Nivel_5, Categoria_Nivel_3) %>% 
#    write.csv2(.,"~Documents/_R/Data.frame/catXseg.csv", row.names = FALSE, fileEncoding ="UTF-8")  


##
# 03 REMOVER CATEGORIAS QUE NAO SAO DO NOSSO NEGOCIO ----
# Categoria nivel 3 e Categoria nivel 5
# obs: Classificaçao realizada a parti do excel criado (dePara_ML.xlsx)
#      Qualquer atualizaçao deve ser feita diretamente no excel.
##
cat_seg <- read_excel("~Documents/_R/Data.frame/Base Apoio/dePara_ML.xlsx", sheet = 'catXseg')

# Identificar as categoria
seg = distinct(cat_seg, Segmento)
seg = as_character(seg$Segmento)
#k=2
for(k in 1:length(seg)) {
   #
   keyword = cat_seg %>% filter(Segmento == seg[k]) %>% distinct(Palavra_Chaves)
   keyword = as_character(keyword$Palavra_Chaves) %>% str_c(.,"|")
   keyword = toString(keyword) %>% str_replace_all(.,"\\|, ","\\|") %>% str_sub(., end = -2)
   #
   filter_seg = ml %>% filter(Categoria_Nivel_3 == seg[k] & str_detect(Categoria_Nivel_5, keyword) == TRUE)
   filter_seg = distinct(filter_seg,Codigo_de_Publicacion)
   #
   ml = ml %>% filter(Codigo_de_Publicacion %notin% filter_seg$Codigo_de_Publicacion)
}

rm(filter_seg)
ml %>% count(Categoria_Nivel_5) %>% view

## 
# 04. ACERTO DO TITULO DA PUBLICAO  ----
# OBJ: manter sempre o mesmo titulo para o mesmo codigo do anuncio. 
##
titulo = ml %>% 
   select(., Codigo_de_Publicacion, Titulo_Publicacion, Mes) %>% 
   arrange(Codigo_de_Publicacion,desc(Mes)) %>% 
   mutate(ranking = ave(rep(1, length(Codigo_de_Publicacion)), Codigo_de_Publicacion, FUN=cumsum)) %>% 
   filter(ranking == 1) %>% 
   select(Codigo_de_Publicacion, Titulo_Publicacion)

ml <- left_join(ml, titulo, by="Codigo_de_Publicacion") %>% 
   mutate(Titulo_Publicacion = str_to_lower(Titulo_Publicacion.y)
          ,Categoria_Completa = str_trim(Categoria_Completa) %>% str_to_lower(.))
rm(titulo)
# ml0 -> ml

##
# 05. ACERTO DO LINK DA IMAGEM DA PUBLICAO  ----
# OBJ: manter sempre a mesma imagem para o mesmo codigo do anuncio. 
#
foto = ml %>%
   select(., Codigo_de_Publicacion, Foto_Publicacion, Mes) %>%
   arrange(Codigo_de_Publicacion,desc(Mes)) %>%
   mutate(ranking = ave(rep(1, length(Codigo_de_Publicacion)), Codigo_de_Publicacion, FUN=cumsum)) %>%
   filter(ranking == 1) %>%
   select(Codigo_de_Publicacion,Foto_Publicacion)

ml <- left_join(ml, foto, by="Codigo_de_Publicacion") %>%
  mutate(Foto_Publicacion = str_to_lower(Foto_Publicacion.y))
rm(foto)


##
# 06. ACERTO DO LINK DA PUBLICAO  ----
# OBJ: manter sempre o mesmo imagem para o mesmo codigo do anuncio. 
#
link = ml %>%
   select(., Codigo_de_Publicacion, Link_a_Publicacion, Mes) %>%
   arrange(Codigo_de_Publicacion,desc(Mes)) %>%
   mutate(ranking = ave(rep(1, length(Codigo_de_Publicacion)), Codigo_de_Publicacion, FUN=cumsum)) %>%
   filter(ranking == 1) %>%
   select(Codigo_de_Publicacion, Link_a_Publicacion)

ml <- left_join(ml, link, by="Codigo_de_Publicacion") %>%
   mutate(Link_a_Publicacion = str_to_lower(Link_a_Publicacion.y))
rm(link)



## 
# 06 AJUSTE DA CATEGORIA NIVEL 5 (Categoria da Ferramenta) ----
# Obs: Ajuste feito por BU separadamente
#
## 

# ___6.1 ACCESSORIES ----
ml = ml %>% 
   mutate(Categoria_Nivel_5 = Categoria_Nivel_5 %>% str_trim() %>% str_to_title() 
          ### Manual Tools 
          %>% if_else(Categoria_Nivel_3 == 'Manual Tools' & str_detect(Categoria_Nivel_5,'[Ss]et|[Kk]it|Jog|Jueg|Estojo') , 'Screw Driving & Sets',.)
          # Validacao com CATEGORIA_NIVEL_5  
          %>% if_else(Categoria_Nivel_3 == 'Accessories' & str_detect(. ,'Bat[ée]|Carga|Carreg') , 'Batery and Charges',.)
          %>% if_else(Categoria_Nivel_3 == 'Accessories' & str_detect(. ,'Manu') , 'Hacksaw blade',.)
          %>% if_else(Categoria_Nivel_3 == 'Accessories' & str_detect(. ,'Hoja|L[âa]mi') , 'Saw blade',.)
          %>% if_else(Categoria_Nivel_3 == 'Accessories' & str_detect(. ,'Mandril|Broqu') , 'Chucks',.)
          %>% if_else(Categoria_Nivel_3 == 'Accessories' & str_detect(. ,'Pont|Punt|Dado') , 'Bits',.)
          %>% if_else(Categoria_Nivel_3 == 'Accessories' & str_detect(. ,'Sopo|Supo') , 'Suports',.)
          %>% if_else(Categoria_Nivel_3 == 'Accessories' & str_detect(. ,'Talad|Furad|Adap') , 'Drill/Driver Accessories',.)
          %>% if_else(Categoria_Nivel_3 == 'Accessories' & str_detect(. ,'Cinc|Cinz|Talh') , 'Chisels',.)
          %>% if_else(Categoria_Nivel_3 == 'Accessories' & str_detect(. ,'Lij|Lix|Lixas') , 'Coated Abr/Polishing',.)
          %>% if_else(Categoria_Nivel_3 == 'Accessories' & str_detect(. ,'Broca') , 'Screw Driving & Sets',.)
          %>% if_else(Categoria_Nivel_3 == 'Accessories' & str_detect(. ,'Caja') , 'Toolboxes',.)
          %>% if_else(Categoria_Nivel_3 == 'Accessories' & str_detect(. ,'^Otr|^Out') , 'Others',.)
          %>% if_else(Categoria_Nivel_3 == 'Accessories' & str_detect(. ,'Induc|Induz') , 'Spare Parts',.)
          %>% if_else(Categoria_Nivel_3 == 'Accessories' & str_detect(. ,'Pul|Pol|Discos De Corte|Disco De Desbaste') , 'Metal Grinding',.)
          %>% if_else(Categoria_Nivel_3 == 'Accessories' & str_detect(. ,'Desb') , 'Metal Grinding',.)
          %>% if_else(Categoria_Nivel_3 == 'Accessories' & str_detect(. ,'Card|Esc|Cepil') , 'Metal Grinding',.)
          %>% if_else(Categoria_Nivel_3 == 'Accessories' & str_detect(. ,'Disco') , 'Discs others',.)
          %>% if_else(Categoria_Nivel_3 == 'Accessories' & str_detect(. ,'Mech') , 'Screw Driving & Sets',.)
          %>% if_else(Categoria_Nivel_3 == 'Accessories' & str_detect(. ,'Laser') , 'Laser measuring',.)
          %>% if_else(Categoria_Nivel_3 == 'Accessories' & str_detect(. ,'N[í]ivel Laser') , 'Leveling',.)
          %>% if_else(Categoria_Nivel_3 == 'Accessories' & str_detect(. ,'Métr') , 'Measuring tape',.)
          %>% if_else(Categoria_Nivel_3 == 'Accessories' & str_detect(. ,'Fresa') , 'Routing',.)
          %>% if_else(Categoria_Nivel_3 == 'Accessories' & str_detect(. ,'Micro|Mini|Lima') , 'AC Rotary tool',.)
          %>% if_else(Categoria_Nivel_3 == 'Accessories' & str_detect(. ,'-|Othe') , 'Others AC',.)
          # Validacao com TITULO PUBLICAO  
          %>% if_else(Categoria_Nivel_3 == 'Accessories' & str_detect(Titulo_Publicacion,'set de herramientas|juego set|jogo de soquetes|jogo de ferramentas|estojo de ferramentas|
                                                                                         |set de ferramentas|jogo de brocas e bits|ponta bit|ponta philips|bit philips|
                                                                                         |ponta imantada bit|juego de mechas|juego mechas') , 'Screw Driving & Sets',.)
          %>% if_else(Categoria_Nivel_3 == 'Accessories' & str_detect(Titulo_Publicacion,'circular de banco|sierra circular de mesa|sierra ingleteadora de banco|
                                                                                         |sierra de inglete|serra fita de bancada') , 'Bench Saws',.)
          %>% if_else(Categoria_Nivel_3 == 'Accessories' & str_detect(Titulo_Publicacion,'mecha escalonada|broca escalonada|broca passo a passo|brocas passo a passo|
                                                                                          |broca revestida de titaneo|broca cónica|brocas p/ ferro|brocas p/ metal|
                                                                                          |brocas escalonadas|mechas c[óo]nicas|mecha c[óo]nica|brocas c[óo]nica') , 'Metal Drill Bits',.)
          %>% if_else(Categoria_Nivel_3 == 'Accessories' & str_detect(Titulo_Publicacion,'hoja de sierra carnicera|hoja de sierra sable|lamina de sierra sable|hoja sierra sable|
                                                                                          |l[âa]mina serra sabre') , 'Recip Saw Blades',.)
          %>% if_else(Categoria_Nivel_3 == 'Accessories' & str_detect(Titulo_Publicacion,'disco de corte|disco corte|disco desbaste|discos de corte|brocas conicas|
                                                                                          |brocas hss conicas|disco abrasivo|rebolo') , 'Metal Grinding',.)
          %>% if_else(Categoria_Nivel_3 == 'Accessories' & str_detect(Titulo_Publicacion,'skil fresadora') , 'Router tool',.)
          %>% if_else(Categoria_Nivel_3 == 'Accessories' & str_detect(Titulo_Publicacion,'disco flap|flap disc|lija|strip disc|disco de lixa|disco de limpeza') , 'Coated Abr/Polishing',.)
          %>% if_else(Categoria_Nivel_3 == 'Accessories' & str_detect(Titulo_Publicacion,'circular Bosch') , 'Cicular saw',.)
          %>% if_else(Categoria_Nivel_3 == 'Accessories' & str_detect(Titulo_Publicacion,'cepillo electrico') , 'Planers',.)
          %>% if_else(Categoria_Nivel_3 == 'Accessories' & str_detect(Titulo_Publicacion,'fresa|broca forstner|brocas forstner|broca para orificions en madera|Forstner') , 'Routing',.)
          %>% if_else(Categoria_Nivel_3 == 'Accessories' & str_detect(Titulo_Publicacion,'martillo demoledor') , 'Demolition Hammer',.)
          %>% if_else(Categoria_Nivel_3 == 'Accessories' & str_detect(Titulo_Publicacion,'disco sierra|lamina de sierra circular|disco sierra circular|disco freud|
                                                                                          |sierra melamina|disco de serra|ferra freud|lamina serra circular') , 'Circular Saw Blades',.)    
          %>% if_else(Categoria_Nivel_3 == 'Accessories' & str_detect(Titulo_Publicacion,'broca sds plus|mecha sds plus|brocas y cinzeles sds|broca furo quadrado|
                                                                                          |kit sds plus') , 'HammerDrillingsmall',.)    
          %>% if_else(Categoria_Nivel_3 == 'Accessories' & str_detect(Titulo_Publicacion,'broca sds max|mecha sds max') , 'Hammerdr.large/other',.)    
          %>% if_else(Categoria_Nivel_3 == 'Accessories' & str_detect(Titulo_Publicacion,'brocas copa|sierra copa|broca sierra|broca para azulejos|broca concreto pasamuros|
                                                                                          |sierra para hoyo|sierra perforadora|sierra de diamante|sierra diente cortador|
                                                                                          |sierra de taladro|sierra corta circulos|sierra de corona|serra copo') , 'Holesaws',.)
          %>% if_else(Categoria_Nivel_3 == 'Accessories' & str_detect(Titulo_Publicacion,'maletin|caja de herramientas') , 'Toolboxes',.)
          %>% if_else(Categoria_Nivel_3 == 'Accessories' & str_detect(Titulo_Publicacion,'hoja de sierra para caladora|hojas de sierra para caladora|hojas de sierra p caladora|
                                                                                          |hojas p caladora|hoja de sierra p caladora|hoja p caladora|kit serra tico tico|
                                                                                          |l[aâ]mina serra tico tico') , 'Jig Saw Blades',.)
          %>% if_else(Categoria_Nivel_3 == 'Accessories' & str_detect(Titulo_Publicacion,'cuchilla de sierra segmentada|acessório para multicordadora') , 'Starlock',.)
          %>% if_else(Categoria_Nivel_3 == 'Accessories' & str_detect(Titulo_Publicacion,'mecha chata|mechas chatas|broca para madeira|broca chata') , 'Wood Drill Bits',.)
          %>% if_else(Categoria_Nivel_3 == 'Accessories' & str_detect(Titulo_Publicacion,'broca para concreto|broca multi|broca multimaterial') , 'Impact+Rot.Drill Bit',.)
          %>% if_else(Categoria_Nivel_3 == 'Accessories' & str_detect(Titulo_Publicacion,'talhadeira sds') , 'Chisels',.)
          %>% if_else(Categoria_Nivel_3 == 'Accessories' & str_detect(Titulo_Publicacion,'faca plaina|faca para plaina|faca p plaina') , 'Components',.)
          %>% if_else(Categoria_Nivel_3 == 'Accessories' & str_detect(Titulo_Publicacion,'disco diamantado|disco procelanato|broca diamantada') , 'DiamondDiscs/CoreBit',.) 
          %>% if_else(Categoria_Nivel_3 == 'Accessories' & str_detect(Titulo_Publicacion,'maçarico') , 'Others tools',.)
   ) 


# ___6.2 MEASURING TOOLS ----
ml = ml %>% mutate(
   Categoria_Nivel_5 = Categoria_Nivel_5 
   %>% if_else(str_detect(. ,'N[íi]v[ée]') & str_detect(., 'L[áa]ser'), 'Leveling', .)
   %>% if_else(str_detect(. ,'[Mm]edid|[Tt]ren') & str_detect(. ,'L[áa]se|[Dd]ista') , 'Laser Measuring',.)
   %>% if_else(str_detect(Titulo_Publicacion, "gll[ 32]|gcl[ 23]|gpl[ 5]|grl|dw08|skt10|pro3|lv-03"), 'Leveling', .)
   %>% if_else(str_detect(Titulo_Publicacion, "glm[ 12458]|GR 500|BT 300|gwm 32|trena digita"), 'Laser measuring', .)
   %>% if_else(Categoria_Nivel_3 == 'Measuring Tools' & str_detect(. ,'Cint|Flex|M[ée]tricas') , 'Measuring Tape',.)
   %>% if_else(Categoria_Nivel_3 == 'Measuring Tools' & str_detect(. ,'Od|Roda') , 'Measuring wheel',.)
   %>% if_else(Categoria_Nivel_3 == 'Measuring Tools' & str_detect(. ,'[Dd]etec') , 'Metal detect',.)
   %>% if_else(Categoria_Nivel_3 == 'Measuring Tools' & str_detect(. ,'Term|Infra|^[Tt]erm') , 'Thermometer',.)
   %>% if_else(Categoria_Nivel_3 == 'Measuring Tools' & str_detect(. ,'M[ãa]o|Bolha|Burb|Resin') , 'Manual leveling',.)
   %>% if_else(Categoria_Nivel_3 == 'Measuring Tools' & str_detect(. ,'[ÓO]pt') , 'Nivel optico',.)
   %>% if_else(Categoria_Nivel_3 == 'Measuring Tools' & str_detect(. ,'[ÁÂaâá]ng') , 'Measuring Angle',.)
   %>% if_else(Categoria_Nivel_3 == 'Measuring Tools' & str_detect(. ,'Otr|Outr') , 'Others MT',.)
   %>% if_else(. == 'Laser measuring' & str_detect(Titulo_Publicacion,'pro3|lv-03') , 'Leveling',.)
   %>% if_else(. == 'Laser measuring' & str_detect(Titulo_Publicacion,'roda|analogic') , 'Measuring wheel',.)
   %>% if_else(. == 'Laser measuring' & str_detect(Titulo_Publicacion,'detector') , 'Metal detect',.)
   %>% if_else(Categoria_Nivel_3 == 'Measuring Tools' & 
                  str_detect(Titulo_Publicacion,'trena|medidor') == TRUE & 
                  str_detect(Titulo_Publicacion,'laser|digital') == FALSE  , 'Measuring Tape',.)
   %>% if_else(Categoria_Nivel_3 == 'Measuring Tools' & 
                  . %notin% c('Leveling','Laser measuring','Measuring Tape', 'Measuring wheel','Metal detect',
                              'Measuring Angle','Nivel optico','Others MT'),'Others MT', .)
)


ml = ml %>% 
   mutate(Categoria_Nivel_5 = Categoria_Nivel_5 %>% str_to_title() 
          # Tool boxes
          %>% if_else(Categoria_Nivel_3 == 'Toolboxes' & str_detect(Categoria_Nivel_5,'Bols|Moch|Port') , 'Bags',.)
          %>% if_else(Categoria_Nivel_3 == 'Toolboxes' & str_detect(Categoria_Nivel_5,'Caj|Caix|Male') , 'Toolboxes',.)
          %>% if_else(Categoria_Nivel_3 == 'Toolboxes' & str_detect(Categoria_Nivel_5,'Carr') , 'Troley',.)
          
          #PowerTools
          %>% if_else(Categoria_Nivel_3 == 'PowerTools' & str_detect(Categoria_Nivel_5,'Aguj|Furadeira de Bancada|Pedest|Taladros de Banco') , 'Bench drill',.)
          %>% if_else(Categoria_Nivel_3 == 'PowerTools' & str_detect(Categoria_Nivel_5,'Amol|Esmeri') , 'Angle grinder',.)
          %>% if_else(Categoria_Nivel_3 == 'PowerTools' & str_detect(Categoria_Nivel_5,'Aspir|Extrac') , 'Blowers & Vacuum cleaner',.)
          %>% if_else(Categoria_Nivel_3 == 'PowerTools' & str_detect(Categoria_Nivel_5,'Atornil|Destorn|Furad|Furar|Paraf|Talad') , 'Drill/ Drivers',.)
          %>% if_else(Categoria_Nivel_3 == 'PowerTools' & str_detect(Categoria_Nivel_5,'Bate') , 'Batery and Charges',.)
          %>% if_else(Categoria_Nivel_3 == 'PowerTools' & str_detect(Categoria_Nivel_5,'Cepilladoras De Mano|Eléctricos De Mano|Cepillo|Lijad|
                                                                                 |Lixad') , 'Hand sander',.)
          %>% if_else(Categoria_Nivel_3 == 'PowerTools' & str_detect(Categoria_Nivel_5,'Cepillad') , 'Bench sander',.)
          %>% if_else(Categoria_Nivel_3 == 'PowerTools' & str_detect(Categoria_Nivel_5,'Impact') , 'Impact wrench',.)
          %>% if_else(Categoria_Nivel_3 == 'PowerTools' & str_detect(Categoria_Nivel_5,'Circul') , 'Circular Saw',.)
          %>% if_else(Categoria_Nivel_3 == 'PowerTools' & str_detect(Categoria_Nivel_5,'Cizall|Tesoura') , 'Shears',.)
          %>% if_else(Categoria_Nivel_3 == 'PowerTools' & str_detect(Categoria_Nivel_5,'Clava|Engrap|Gramp') , 'Staplers & Nailers',.)
          %>% if_else(Categoria_Nivel_3 == 'PowerTools' & str_detect(Categoria_Nivel_5,'Colect|Colet') , 'Dust colectors',.)
          %>% if_else(Categoria_Nivel_3 == 'PowerTools' & str_detect(Categoria_Nivel_5,'Parede') , 'Wall cutter',.)
          %>% if_else(Categoria_Nivel_3 == 'PowerTools' & str_detect(Categoria_Nivel_5,'Concr|Márm') , 'Marble saw',.)
          %>% if_else(Categoria_Nivel_3 == 'PowerTools' & str_detect(Categoria_Nivel_5,'Laser') , 'Laser measuring',.)
          %>% if_else(Categoria_Nivel_3 == 'PowerTools' & str_detect(Categoria_Nivel_5,'N[í]ivel Laser') , 'Leveling',.)
          %>% if_else(Categoria_Nivel_3 == 'PowerTools' & str_detect(Categoria_Nivel_5,'Disco') , 'Discs others',.)
          %>% if_else(Categoria_Nivel_3 == 'PowerTools' & str_detect(Categoria_Nivel_5,'Fresad|Router|Rutea|Tupia') , 'Router tool',.)
          %>% if_else(Categoria_Nivel_3 == 'PowerTools' & str_detect(Categoria_Nivel_5,'Inglet|Esquad') , 'Miter saw',.)
          %>% if_else(Categoria_Nivel_3 == 'PowerTools' & str_detect(Categoria_Nivel_5,'Lustrad|Polit|Pulid') , 'Polisher',.)
          %>% if_else(Categoria_Nivel_3 == 'PowerTools' & str_detect(Categoria_Nivel_5,'Demol') , 'Demolition Hammer',.)
          %>% if_else(Categoria_Nivel_3 == 'PowerTools' & str_detect(Categoria_Nivel_5,'Martel|Rotoma') , 'Rotary Hammer',.)
          %>% if_else(Categoria_Nivel_3 == 'PowerTools' & str_detect(Categoria_Nivel_5,'Medidor|N[íi]vel') , 'Laser measuring',.)
          %>% if_else(Categoria_Nivel_3 == 'PowerTools' & str_detect(Categoria_Nivel_5,'Micro|Mini|Mototool|Ret[íi]ficas') , 'Rotary tool',.)
          %>% if_else(Categoria_Nivel_3 == 'PowerTools' & str_detect(Categoria_Nivel_5,'Misturad') , 'Paint mixers',.)
          %>% if_else(Categoria_Nivel_3 == 'PowerTools' & str_detect(Categoria_Nivel_5,'Otr|Out') , 'Others',.)
          %>% if_else(Categoria_Nivel_3 == 'PowerTools' & str_detect(Categoria_Nivel_5,'Calor|Pistola El[ée]c|T[ée]rm') , 'Hot air gun',.)
          %>% if_else(Categoria_Nivel_3 == 'PowerTools' & str_detect(Categoria_Nivel_5,'Plaina de Bancada') , 'Bench planers',.)
          %>% if_else(Categoria_Nivel_3 == 'PowerTools' & str_detect(Categoria_Nivel_5,'Plaina|Polid') , 'Planers',.)
          %>% if_else(Categoria_Nivel_3 == 'PowerTools' & str_detect(Categoria_Nivel_5,'Rectificad') , 'Straight grinder',.)
          %>% if_else(Categoria_Nivel_3 == 'PowerTools' & str_detect(Categoria_Nivel_5,'Repuestos') , 'Spare parts',.)
          %>% if_else(Categoria_Nivel_3 == 'PowerTools' & str_detect(Categoria_Nivel_5,'Sensit|Tronzad') , 'Multi cut saws',.)
          %>% if_else(Categoria_Nivel_3 == 'PowerTools' & str_detect(Categoria_Nivel_5,'Serras de Bancada|Cortadora|Sierras De Banco|
                                                                                        |Sierras De Mesa|Sin Fin') , 'Bench Saws',.)
          %>% if_else(Categoria_Nivel_3 == 'PowerTools' & str_detect(Categoria_Nivel_5,'Sabre|Sable') , 'Recip saw',.)
          %>% if_else(Categoria_Nivel_3 == 'PowerTools' & str_detect(Categoria_Nivel_5,'Tico|Caladora') , 'Jigsaw',.)
          %>% if_else(Categoria_Nivel_3 == 'PowerTools' & str_detect(Categoria_Nivel_5,'Sierra Acc|Sierra Acc') , 'Discs others',.)
          %>% if_else(Categoria_Nivel_3 == 'PowerTools' & str_detect(Categoria_Nivel_5,'Serras') , 'Saws',.)
          %>% if_else(Categoria_Nivel_3 == 'PowerTools' & str_detect(Categoria_Nivel_5,'Serras|Sierra') , 'Saws',.)
          %>% if_else(Categoria_Nivel_3 == 'PowerTools' & str_detect(Categoria_Nivel_5,'Vibrador') , 'Concrete vibrator',.)
          
          %>% if_else(Categoria_Nivel_3 == 'PowerTools' & str_detect(Titulo_Publicacion,'cortadora de concreto|marmol|m[áa]rm') , 'Marble Saw',.)
          %>% if_else(Categoria_Nivel_3 == 'PowerTools' & str_detect(Titulo_Publicacion,'sierra circular|serra circular') , 'Cicular saw',.)
          %>% if_else(Categoria_Nivel_3 == 'PowerTools' & str_detect(Titulo_Publicacion,'caladora|tico') , 'Jigsaw',.)
          %>% if_else(Categoria_Nivel_3 == 'PowerTools' & str_detect(Titulo_Publicacion,'sabre|sable') , 'Recipsaw',.)
          %>% if_else(Categoria_Nivel_3 == 'PowerTools' & str_detect(Titulo_Publicacion,'cortador de parede') , 'Wall cutter',.)
          %>% if_else(Categoria_Nivel_3 == 'PowerTools' & str_detect(Titulo_Publicacion,'chave de impacto') , 'Impact wrench',.)
          %>% if_else(Categoria_Nivel_3 == 'PowerTools' & str_detect(Titulo_Publicacion,'taladro') , 'Drill/ Drivers',.)
          #%>% if_else(Categoria_Nivel_3 == 'PowerTools' & str_detect(Titulo_Publicacion,'set|kit') , 'Kits & Sets',.)
          %>% if_else(Categoria_Nivel_3 == 'PowerTools' & str_detect(Titulo_Publicacion,'vibrador') , 'Concrete vibrator',.)
          %>% if_else(Categoria_Nivel_3 == 'PowerTools' & str_detect(Titulo_Publicacion,'deseng|cepillo cantead|desempen') , 'Bench planer',.)
          %>% if_else(Categoria_Nivel_3 == 'PowerTools' & str_detect(Titulo_Publicacion,'esmeril bancada|esmeril de banc') , 'Bench grinder',.)
          %>% if_else(Categoria_Nivel_3 == 'PowerTools' & str_detect(Titulo_Publicacion,'serra de bancada|sierra banco|esquad|cortadora') , 'Bench saws',.)
          %>% if_else(Categoria_Nivel_3 == 'PowerTools' & str_detect(Titulo_Publicacion,'caneta el[ée]') , 'Rotary tool',.)
          %>% if_else(Categoria_Nivel_3 == 'PowerTools' & str_detect(Titulo_Publicacion,'de banco|de bancada') , 'Other Benchtops',.)
          
          %>% if_else(Categoria_Nivel_6 == 'worker' & str_detect(Titulo_Publicacion,'motoesmeril|moto esmeril') , 'Other Benchtops',.)
          
          
          %>% if_else(Categoria_Nivel_5 == 'Kits & Sets' & str_detect(Titulo_Publicacion,'parafusadeira|taladro|furad|atornill') , 'Drill/ Drivers',.)
          %>% if_else(Categoria_Nivel_5 == 'Kits & Sets' & str_detect(Titulo_Publicacion,'serra m[áa]rm') , 'Marble Saw',.)
          %>% if_else(Categoria_Nivel_5 == 'Kits & Sets' & str_detect(Titulo_Publicacion,'martel|rotoma') , 'Rotary Hammer',.)
          %>% if_else(Categoria_Nivel_5 == 'Kits & Sets' & str_detect(Titulo_Publicacion,'amol|esmeri') , 'Angle grinder',.)
          %>% if_else(Categoria_Nivel_5 == 'Kits & Sets' & str_detect(Titulo_Publicacion,'router|tupia') , 'Router tool',.)
          %>% if_else(Categoria_Nivel_5 == 'Kits & Sets' & str_detect(Titulo_Publicacion,'circular') , 'Circular saw',.)
          %>% if_else(Categoria_Nivel_5 == 'Kits & Sets' & str_detect(Titulo_Publicacion,'lijad|lixad') , 'Hand sander',.)
          %>% if_else(Categoria_Nivel_5 == 'Kits & Sets' & str_detect(Titulo_Publicacion,'plaina|cepillo el[ée]') , 'Planers',.)
          %>% if_else(Categoria_Nivel_5 == 'Kits & Sets' & str_detect(Titulo_Publicacion,'policorte|sierra de mes|ingletad|sensitiv') , 'Bench saws',.)
          
          
          %>% if_else(Categoria_Nivel_3 == 'PowerTools' & str_detect(Categoria_Nivel_5,'Eléctrico') , 'Others tools',.)
          %>% if_else(Categoria_Nivel_3 == 'Others' & str_detect(Titulo_Publicacion,'furadei|parafus|taladro') , 'Drill/ Drivers',.)
          %>% if_else(Categoria_Nivel_3 == 'Others' & str_detect(Titulo_Publicacion,'tupia') , 'Router tool',.)
          
   )


##
# 09. RECATEGORIZAÇÃO DO SEGEMENTO parte >> II ----
# jogaR os kits e sets para AC, criar bench, criar micro retifica
## 
ml = ml %>% mutate(
   Categoria_Nivel_3 = Categoria_Nivel_3 
   %>% if_else(str_detect(Categoria_Nivel_5, "Screw Driving & Sets|Dust"),'Accessories',.)
   %>% if_else(str_detect(Categoria_Nivel_5, "Measu|meas|MT|Leve"),'Measuring Tools',.)
   %>% if_else(str_detect(Categoria_Nivel_5, "Toolbox"),'Toolboxes',.)
   %>% if_else(str_detect(Categoria_Nivel_5, "[Bb]ench|Miter|Multi"),'Benchtop',.)
   %>% if_else(str_detect(Categoria_Nivel_5, "Rotary tool"),'Rotary tools',.)
   %>% if_else(str_detect(Categoria_Nivel_5, "Spare"),'Spare parts',.)
   %>% if_else(str_detect(Categoria_Nivel_5, "^Drill/ Drivers|Marble Saw|Rotary Hammer|Angle grinder|
                                            |Router tool|Circular saw|Hand sander|Planers|
                                            |Batery and Charges|tools|Demol"),'PowerTools',.)
)

## 
# 07. AJUSTE NICKNAME VENDEDOR  ----
# Obs: Ajuste a parti do excel dePara_ML(aba: Clas_cliente) 
#      entao todas alterações devem ser feitas diretamente no arquivo
#
## 
vendedor <- read_excel("~Documents/_R/Data.frame/Base Apoio/dePara_ML.xlsx", sheet = 'Clas_Cliente')
ClienteBosch <- as_character(vendedor$Nickname_Vendedor) 
ClienteTT <- vendedor %>% filter(CANAL == 'TT') %>% select(Nickname_Vendedor) 
ClienteMP <- vendedor %>% filter(CANAL == 'MP') %>% select(Nickname_Vendedor)

ml = mutate(ml, 
            # Acerto do Nick Vendedor
            Nickname_Vendedor = Nickname_Vendedor
            %>% str_replace_all(.,"LOJA_DO_MECANICO_OFICIAL 2","LOJA_DO_MECANICO_OFICIAL") 
            %>% str_replace_all(.,"CEFEQFERRAMENTAS","CEFEQ FERRAMENTAS") 
            %>% str_replace_all(.,"SIPARFERRAMENTA","SIPAR FERRAMENTAS")  
            %>% str_replace_all(.,"KAUSBENPARAFUSOS","KAUSBEN PARAFUSOS")
            %>% str_replace_all(.,"RESSEG DISTRIBUIDORA","RESSEGDISTRIBUIDORALTDA")
            # Cliente Bosch
            ,Cliente_bosch = 'N' %>% if_else(Nickname_Vendedor %in% ClienteBosch ,'S',.)
            # Lojas Oficiais
            ,Loja_oficial = 'N' %>% if_else(Nickname_Vendedor %in% "TESTE" ,'S',.) # OBS: INCLUIR A NOVA INFORMAÇAO
            # Canais
            ,canais = case_when(
               Nickname_Vendedor %in% ClienteTT$Nickname_Vendedor ~ 'TT'
              , Nickname_Vendedor %in% ClienteMP$Nickname_Vendedor ~ 'MP'
              , TRUE ~ '-'
             )
            ) # Fim Mutate 

rm(ClienteMP,ClienteTT, ClienteBosch, vendedor)


# 05. Classificação de canais para Brasil ----
# CLASSIFICAÇÃO DE CANAL

ml = mutate(ml,
            canal = case_when(
               # MP
               Nickname_Vendedor %in% c('CARREFOUR.COM', 'DUFRIO REFRIGERAÇÃO'
                                        ,'EBALAROTI','LOJA_DO_MECANICO_OFICIAL'
                                        ,'LOJASTAQI','NOVOMUNDOMVEISEUTILIDAD'
                                        ,'QUALITY SP','QUALITY_SP','TELHANORTE AS') ~ "MP"
               , TRUE ~ "Other")
)





## 
# 08. HIGENIZAÇAO I  ------
# obj: Remover grupo de auncios que não fazem sentido com o 
#      negocio da bosch
#
## 

# Passo 1: Criar o vetor de keywords para deletar (Baseado na planilha dePara_ML)
delete_keyword <- read_excel("~Documents/_R/Data.frame/Base Apoio/dePara_ML.xlsx", sheet = 'KeyWord_excluir')
delete_palavras_chaves = as_character(delete_keyword$Titulo_Publicacion) %>%  str_c(.,"|")
delete_palavras_chaves = toString(delete_palavras_chaves) %>% 
   str_replace_all(.,"\\|, ","\\|") %>% str_sub(., end = -2) 

# Passo 2: Criar filtros segmentos sem classificaçao 
filter_other = ml %>%
   filter(str_detect(Categoria_Nivel_3, 'Others') == TRUE) %>% 
   select(Codigo_de_Publicacion, Titulo_Publicacion) %>% unique()

filter_termometro = ml %>%
   filter(str_detect(Categoria_Nivel_5, 'Therm') == TRUE) %>% 
   select(Codigo_de_Publicacion, Titulo_Publicacion) %>% unique()

filter_acessorios = ml %>%
   filter(Categoria_Nivel_3 == 'Accessories' & str_detect(Categoria_Nivel_5, 'Suports') == TRUE) %>% 
   select(Codigo_de_Publicacion, Titulo_Publicacion) %>% unique()


filter_palavras_chaves = ml %>% 
   filter(str_detect(Titulo_Publicacion, delete_palavras_chaves)==TRUE) %>% 
   select(Codigo_de_Publicacion, Titulo_Publicacion)


filter_palavras_ac = ml %>% 
   filter(Categoria_Nivel_3 == 'Accessories' & str_detect(Titulo_Publicacion,delete_palavras_chaves)==TRUE) %>% 
   select(Codigo_de_Publicacion, Titulo_Publicacion)

# Passo 3: Exluir da base os aunicios
ml = ml %>% filter(Codigo_de_Publicacion %notin% filter_palavras_chaves$Codigo_de_Publicacion)
ml = ml %>% filter(Codigo_de_Publicacion %notin% filter_termometro$Codigo_de_Publicacion)
ml = ml %>% filter(Codigo_de_Publicacion %notin% filter_other$Codigo_de_Publicacion)
ml = ml %>% filter(Codigo_de_Publicacion %notin% filter_palavras_ac$Codigo_de_Publicacion)
ml = ml %>% filter(Codigo_de_Publicacion %notin% filter_acessorios$Codigo_de_Publicacion)

# Passo 4: Remover categorias ajustas
rm(filter_palavras_chaves, filter_termometro, filter_other, filter_palavras_ac, filter_acessorios)

#  /.\
#  |||
#  |||


##
# 12. AJUSTE CAMBIO ---- 
##

# Ajustes na base
cambio = read_excel("~Documents/_R/Campanha ML/cambioo.xlsx"
                    , sheet = 'Exchange Rates ') %>% .[21:nrow(.),3:ncol(.)] 
names(cambio) <- str_c("v",seq(1:13))
cambio = cambio %>%
   mutate(ch = str_c(v3, v4, str_sub(v8,1,2)) %>% str_to_upper()
          ,v9 = as.numeric(v9)
          ,v12 = as.numeric(v12)) %>% 
   select(ch,v9,v12) 

# Criacao da chave e transformaÃ§ao 
ml = mutate(ml, ch = str_c(year(Mes), month(Mes),str_sub(pais,1,2)) %>% str_to_upper())
ml = left_join(ml, cambio, by = 'ch')
ml = mutate(ml, Monto_Vendido_USD = Monto_Vendido_Moneda_Local/v9
            ,PrecioUsd = Monto_Vendido_Moneda_Local/v12)
ml = select(ml, -ch,-v9,-v12)

ml = rename(ml, Valor_EURwoc = PrecioUsd, Valor_EURwic = Monto_Vendido_USD)
rm(cambio)

#mlbackup2 <- ml
#mlbackup2 -> ml


## 
# 10. AJUSTE DE MARCA ----
##
# Passo 1: Capta informação a partir do titulo da publicação 
ml = mutate(ml
            ,Categoria_Nivel_6 = as.character(Categoria_Nivel_6)
            # %>% if_else(. %in% c('-', 'Outras Marcas', 'Outros', 'Otras Marcas', 'otros'), Marca, .)
            %>% str_remove_all(., "&") %>% str_trim(.) %>% str_replace_all(.," +"," ") 
            %>% if_else(str_detect(., pattern = "ecker$")==TRUE, 'Black & Decker', .) 
            # PREENCHIMENTO DA MARCA A PARTIR DO TITULO DO AUNUNCIO
            %>% if_else(str_detect(Titulo_Publicacion, 'bosc|gll[ 32]|gcl[ 23]|gpl[ 5]|grl|gsb|gsr|gws') == TRUE, 'Bosch', .)
            %>% if_else(str_detect(Titulo_Publicacion, 'skil') == TRUE, 'Skil', .)
            %>% if_else(str_detect(Titulo_Publicacion, 'dremel') == TRUE, 'Dremel', .) 
            %>% if_else(str_detect(Titulo_Publicacion, 'black & decker|black&decker|black[ -+]decke') == TRUE, 'Black & Decker', .)
            %>% if_else(str_detect(Titulo_Publicacion, 'dewalt|dcd') == TRUE, 'Dewalt', .)
            %>% if_else(str_detect(Titulo_Publicacion, 'stanley') == TRUE, 'Stanley', .) 
            %>% if_else(str_detect(Titulo_Publicacion, 'makita') == TRUE, 'Makita', .)
            %>% if_else(str_detect(Titulo_Publicacion, 'einhell') == TRUE, 'Einhell', .)
            %>% if_else(str_detect(Titulo_Publicacion, 'hilti') == TRUE, 'Hilti', .)
            %>% if_else(str_detect(Titulo_Publicacion, 'milwaukee') == TRUE, 'Milwaukee', .) 
            %>% if_else(str_detect(Titulo_Publicacion, 'metabo') == TRUE, 'Metabo', .) 
            %>% if_else(str_detect(Titulo_Publicacion, 'wurth') == TRUE, 'Wurth', .) 
            %>% if_else(str_detect(Titulo_Publicacion, 'bauker') == TRUE, 'Bauker', .)
            %>% if_else(str_detect(Titulo_Publicacion, 'dexter') == TRUE, 'Dexter', .)
            %>% if_else(str_detect(Titulo_Publicacion, 'carrefour') == TRUE, 'Carrefour', .)
            %>% if_else(str_detect(Titulo_Publicacion, 'mondial|fpf-') == TRUE, 'Mondial', .) 
            %>% if_else(str_detect(Titulo_Publicacion, 'vonder|pfv012') == TRUE, 'Vonder', .) 
            %>% if_else(str_detect(Titulo_Publicacion, 'philco') == TRUE, 'Philco', .) 
            %>% if_else(str_detect(Titulo_Publicacion, 'wesco') == TRUE, 'Wesco', .) 
            %>% if_else(str_detect(Titulo_Publicacion, 'hammer') == TRUE, 'Hammer', .) 
            %>% if_else(str_detect(Titulo_Publicacion, 'gamma|g1910') == TRUE, 'Gamma', .)
            %>% if_else(str_detect(Titulo_Publicacion, 'tramontina') == TRUE, 'Tramontina', .) 
            %>% if_else(str_detect(Titulo_Publicacion, 'schulz') == TRUE, 'Schulz', .) 
            %>% if_else(str_detect(Titulo_Publicacion, 'goodyear') == TRUE, 'Goodyear', .) 
            %>% if_else(str_detect(Titulo_Publicacion, 'dwt') == TRUE, 'Dwt', .)
            %>% if_else(str_detect(Titulo_Publicacion, 'brit[âa]nia') == TRUE, 'Britania', .) 
            %>% if_else(str_detect(Titulo_Publicacion, 'songhe') == TRUE, 'Songhe', .)
            %>% if_else(str_detect(Titulo_Publicacion, 'ferrari') == TRUE, 'Ferrari', .) 
            %>% if_else(str_detect(Titulo_Publicacion, 'fortg') == TRUE, 'Fortg', .)
            %>% if_else(str_detect(Titulo_Publicacion, 'ingco') == TRUE, 'Ingco', .)
            %>% if_else(str_detect(Titulo_Publicacion, 'maksiwa') == TRUE, 'Maksiwa', .)
            %>% if_else(str_detect(Titulo_Publicacion, 'worx') == TRUE, 'Worx', .)
            %>% if_else(str_detect(Titulo_Publicacion, 'wagner') == TRUE, 'Wagner', .) 
            %>% if_else(str_detect(Titulo_Publicacion, 'nagano') == TRUE, 'Nagano', .)
            %>% if_else(str_detect(Titulo_Publicacion, 'deko|3d 12|12linha') == TRUE, 'Deko', .) 
            %>% if_else(str_detect(Titulo_Publicacion, 'clipper') == TRUE, 'Clipper', .) 
            %>% if_else(str_detect(Titulo_Publicacion, 'mtx') == TRUE, 'Mtx', .)
            %>% if_else(str_detect(Titulo_Publicacion, 'siga tools') == TRUE, 'Siga Tools', .) 
            %>% if_else(str_detect(Titulo_Publicacion, 'awt') == TRUE, 'Awt', .)
            %>% if_else(str_detect(Titulo_Publicacion, 'xiaomi') == TRUE, 'Xiaomi', .)
            %>% if_else(str_detect(Titulo_Publicacion, 'starrett') == TRUE, 'Starret', .)
            ## Categorizaao usando descricao completa
            %>% if_else(Categoria_Nivel_6 == '-' & str_detect(Categoria_Completa,'bosch' ) == TRUE, 'Bosch', .) 
            %>% if_else(Categoria_Nivel_6 == '-' & str_detect(Categoria_Completa,'skil' ) == TRUE, 'Skil', .) 
            %>% if_else(Categoria_Nivel_6 == '-' & str_detect(Categoria_Completa,'dremel' ) == TRUE, 'Dremel', .) 
            %>% if_else(Categoria_Nivel_6 == '-' & str_detect(Categoria_Completa,'black & decker' ) == TRUE, 'Black & Decker', .) 
            %>% if_else(Categoria_Nivel_6 == '-' & str_detect(Categoria_Completa,'dewalt' ) == TRUE, 'Dewalt', .)
            %>% if_else(Categoria_Nivel_6 == '-' & str_detect(Categoria_Completa,'stanley' ) == TRUE, 'Stanley', .)
            %>% if_else(Categoria_Nivel_6 == '-' & str_detect(Categoria_Completa,'makita' ) == TRUE, 'Makita', .)
            %>% if_else(Categoria_Nivel_6 == '-' & str_detect(Categoria_Completa,'makita mt' ) == TRUE, 'Makita Mt', .) 
            %>% if_else(Categoria_Nivel_6 == '-' & str_detect(Categoria_Completa,'einhell' ) == TRUE, 'Einhell', .) 
            %>% if_else(Categoria_Nivel_6 == '-' & str_detect(Categoria_Completa,'hilti' ) == TRUE, 'Hilti', .)
            %>% if_else(Categoria_Nivel_6 == '-' & str_detect(Categoria_Completa,'milwaukee' ) == TRUE, 'Milwaukee', .) 
            %>% if_else(Categoria_Nivel_6 == '-' & str_detect(Categoria_Completa,'metabo' ) == TRUE, 'Metabo', .) 
            %>% if_else(Categoria_Nivel_6 == '-' & str_detect(Categoria_Completa,'w[ü]rth|wrth' ) == TRUE, 'Wurth', .) 
            %>% if_else(Categoria_Nivel_6 == '-' & str_detect(Categoria_Completa,'bauker' ) == TRUE, 'Bauker', .)
            %>% if_else(Categoria_Nivel_6 == '-' & str_detect(Categoria_Completa,'dexter' ) == TRUE, 'Dexter', .)
            %>% if_else(Categoria_Nivel_6 == '-' & str_detect(Categoria_Completa,'carrefour' ) == TRUE, 'Carrefour', .)
            %>% if_else(Categoria_Nivel_6 == '-' & str_detect(Categoria_Completa,'mondial' ) == TRUE, 'Mondial', .)
            %>% if_else(Categoria_Nivel_6 == '-' & str_detect(Categoria_Completa,'vonder|pfv012' ) == TRUE, 'Vonder', .) 
            %>% if_else(Categoria_Nivel_6 == '-' & str_detect(Categoria_Completa,'philco' ) == TRUE, 'Philco', .)
            %>% if_else(Categoria_Nivel_6 == '-' & str_detect(Categoria_Completa,'wesco' ) == TRUE, 'Wesco', .) 
            %>% if_else(Categoria_Nivel_6 == '-' & str_detect(Categoria_Completa,'hammer' ) == TRUE, 'Hammer', .) 
            %>% if_else(Categoria_Nivel_6 == '-' & str_detect(Categoria_Completa,'gamma' ) == TRUE, 'Gamma', .) 
            %>% if_else(Categoria_Nivel_6 == '-' & str_detect(Categoria_Completa,'tramontina' ) == TRUE, 'Tramontina', .)
            %>% if_else(Categoria_Nivel_6 == '-' & str_detect(Categoria_Completa,'schulz' ) == TRUE, 'Schulz', .) 
            %>% if_else(Categoria_Nivel_6 == '-' & str_detect(Categoria_Completa,'goodyear' ) == TRUE, 'Goodyear', .) 
            %>% if_else(Categoria_Nivel_6 == '-' & str_detect(Categoria_Completa,'dwt' ) == TRUE, 'Dwt', .) 
            %>% if_else(Categoria_Nivel_6 == '-' & str_detect(Categoria_Completa,'brit[âa]nia' ) == TRUE, 'Britania', .) 
            %>% if_else(Categoria_Nivel_6 == '-' & str_detect(Categoria_Completa,'songhe' ) == TRUE, 'Songhe', .) 
            %>% if_else(Categoria_Nivel_6 == '-' & str_detect(Categoria_Completa,'ferrari' ) == TRUE, 'Ferrari', .)
            %>% if_else(Categoria_Nivel_6 == '-' & str_detect(Categoria_Completa,'fortg' ) == TRUE, 'Fortg', .) 
            %>% if_else(Categoria_Nivel_6 == '-' & str_detect(Categoria_Completa,'ingco' ) == TRUE, 'Ingco', .) 
            %>% if_else(Categoria_Nivel_6 == '-' & str_detect(Categoria_Completa,'maksiwa' ) == TRUE, 'Maksiwa', .) 
            %>% if_else(Categoria_Nivel_6 == '-' & str_detect(Categoria_Completa,'worx' ) == TRUE, 'Worx', .)
            %>% if_else(Categoria_Nivel_6 == '-' & str_detect(Categoria_Completa,'wagner' ) == TRUE, 'Wagner', .) 
            %>% if_else(Categoria_Nivel_6 == '-' & str_detect(Categoria_Completa,'nagano' ) == TRUE, 'Nagano', .) 
            %>% if_else(Categoria_Nivel_6 == '-' & str_detect(Categoria_Completa,'deko|3d 12|12linha' ) == TRUE, 'Deko', .) 
            %>% if_else(Categoria_Nivel_6 == '-' & str_detect(Categoria_Completa,'clipper' ) == TRUE, 'Clipper', .) 
            %>% if_else(Categoria_Nivel_6 == '-' & str_detect(Categoria_Completa,'siga tools' ) == TRUE, 'Siga Tools', .) 
            %>% if_else(Categoria_Nivel_6 == '-' & str_detect(Categoria_Completa,'awt' ) == TRUE, 'Awt', .) 
            %>% if_else(Categoria_Nivel_6 == '-' & str_detect(Categoria_Completa,'[ -]mtx' ) == TRUE, 'Mtx', .)
            %>% if_else( str_detect(., "VEKCER|Veker|VECKER"), 'Veker', .)
            %>% if_else( str_detect(., "otros|Otros"), 'Other', .)
            %>% if_else( str_detect(., "makita"), 'Makita', .)
            %>% if_else(Categoria_Nivel_5 == Categoria_Nivel_6 & Categoria_Nivel_5 != '-', Categoria_Nivel_5, . )
            %>% if_else(str_detect(Titulo_Publicacion,"similar|parecido|n[ã]o [ée]") == TRUE, "Similar(Imitação)",.)
) #fim mutate

# Passo 2: RECUPERAR INFORMAÇÃO DE MARCA DO CAMPO 'marca'
# Obj: A parti da info que temos 
ml = ml %>% 
   mutate(Marca = str_to_title(Marca) 
          %>% str_remove_all(.,"[0-9]")
          %>% if_else(str_length(.) < 3, "Sin Marca",.)
          ,Categoria_Nivel_6 = Categoria_Nivel_6 
          %>% if_else(str_to_title(.) %in% c('-', 'Outras Marcas', 'Outros', 'Otras Marcas', 'Otros'), Marca, .)
          %>% if_else(str_detect(. ,"[Ss]imila"), "Similar(Imitação)", .)
          %>% str_to_lower()
   )


# Passo 3: AGRUPAMENTO DE MARCAS
#ml %>% count(Categoria_Nivel_6) %>% arrange(desc(n)) %>% view()

Share_marca = ml %>% group_by(pais, Categoria_Nivel_6) %>% summarise(Valor_EURwoc = sum(Valor_EURwoc)) %>% 
   mutate(share = round((Valor_EURwoc/sum(Valor_EURwoc))*100, 2),
          ch    = paste0(pais, Categoria_Nivel_6)) %>% 
   filter(share < 1) %>% select(ch) %>%  unique() %>% as_tibble()

ml <- ml %>% 
   mutate(Categoria_Nivel_1 = ""
          %>% if_else(Categoria_Nivel_6 %in% c('bosch','dremel','skil') == TRUE, "Bosch Grp",.)
          %>% if_else(Categoria_Nivel_6 %in% c('dewalt','stanley','black & decker') == TRUE, "SBD",.)
          %>% if_else(Categoria_Nivel_6 == 'makita', "Makita",.)
          %>% if_else(Categoria_Nivel_6 == 'wurth', "Wurth",.)
          %>% if_else(Categoria_Nivel_6 == 'metabo', "Metabo",.)
          %>% if_else(Categoria_Nivel_6 == 'einhell', "Einhell",.)
          %>% if_else(Categoria_Nivel_6 == 'tti', "TTI",.)
          %>% if_else(Categoria_Nivel_6 == 'hilti', "Hilti",.)
          %>% if_else(Categoria_Nivel_6 == 'hitachi', "Hitachi",.)
          %>% if_else(. == "" & paste0(pais, Categoria_Nivel_6) %in% Share_marca$ch, 'no names', .)
          %>% if_else(. == "", Categoria_Nivel_6, .)
   )

##
# 11. DELETAR MARCAS ---- 
# Obj: Exluir da base todas as marcas que não tem relação com o nosso negocio
##
ml = mutate(ml,Categoria_Nivel_6 = str_to_lower(Categoria_Nivel_6))
delete_marcas <- read_excel("~/Documents/_R/Data.frame/Base Apoio/dePara_ML.xlsx", sheet = 'excluirMarcas')
delete_marcas <- as_character(delete_marcas$Categoria_nivel_6) 
filter_marcas = ml %>% filter(Categoria_Nivel_6 %in% delete_marcas )
ml = ml %>% filter(Codigo_de_Publicacion %notin% filter_marcas$Codigo_de_Publicacion)
rm(filter_marcas,delete_marcas)

ml = rename(ml, Grupo_marca = Categoria_Nivel_1)



#
#
# 13. CLASSIFICACAO TIPO FERRAMENTA  ----
var = str_c("|",seq(100,999,10),"[ w]") %>%
   as_character(.) %>%
   toString() %>%
   str_replace_all(.,", ","") %>%
   str_sub(2,nchar(.))

ml = ml %>% 
   mutate(Moneda = 'Not assigned' 
          ### Menor que 10V
          %>% if_else(str_detect(Titulo_Publicacion, "3[,.]6v|3,6 v|3.6 v|4[,.]8v|4[,.]8 v|9[,.]6v|9[,.]6 v|pf-08"), '<10v', .)
          ### 12V
          %>% if_else(str_detect(Titulo_Publicacion, "12[.,]0v|12[.,]0 v|12v|12 v|12-v|10[,.]8v|10[,.]8 v|gsr1000|gsr 1000|hp331|impacto 3/8 vonder|clx202sax"), '12v', .)
          %>% if_else(str_detect(Titulo_Publicacion, "14v|14 v|14-v|14,4v|dcd716"), '12v', .)
          ### 18V
          %>% if_else(str_detect(Titulo_Publicacion, "18v|18 v|18-v|18[.,]0v|18[.,]0 v"), '18v', .)
          %>% if_else(str_detect(Titulo_Publicacion, " 20v| 20 v| 20-v|20[.,]0v|20[.,]0 v|dga504"), '18v', .)
          %>% if_else(str_detect(Titulo_Publicacion, "36v|36 v|36-v"), 'Cordless[not assigned]', .)
          ### Corded
          %>% if_else(. == 'Not assigned' & str_detect(Titulo_Publicacion, var), 'Corded', .)
          %>% if_else(. == 'Not assigned' & str_detect(Titulo_Publicacion, 'gex 125|4100nh3z|esmerilhadeira 4.1/2 9002|110[wv]|220[wv]|127[wv]'), 'Corded', .)
          %>% if_else(. == 'Not assigned' & Categoria_Nivel_3 == 'PowerTools' & str_detect(Titulo_Publicacion, 'parafusadeira|furadeira|esmeri|serra|tupia|plania|plaina|politriz|lixadeira|pancada|soprador|marte'), 'Corded', .)
          ### Cordless [not assigned]   
          %>% if_else(str_detect(Titulo_Publicacion, "36v|36 v|36-v"), 'Cordless[not assigned]', .)
          %>% if_else(. == 'Not assigned' & str_detect(Titulo_Publicacion, 'bateria|s[ /]fio|sem fio|brushless|inal[aá]mbric'), 'Cordless[not assigned]', .)
          ### Outras BU
          %>% if_else(Categoria_Nivel_3 == 'Measuring Tools', 'MT', .)
          %>% if_else(Categoria_Nivel_3 == 'Accessories', 'AC', .)
   )

ml =  rename(ml, Tipo_ferramenta = Moneda)


ml %>% tabyl(Tipo_ferramenta) %>%  arrange(desc(n))


## 
# 14. PADRONIZAR DADOS ANTES DE SALVAR ----
# #
ml = ml %>% select(.,- Link_a_Publicacion.x,-Link_a_Publicacion.y)   
ml =  ml %>% mutate(
   Categoria_Nivel_6 = str_to_title(Categoria_Nivel_6)
   ,Categoria_Nivel_5 = str_to_title(Categoria_Nivel_5)
)
ml = as_tibble(ml)

write.csv2(ml,"~/Documents/_R/Data.frame/202008_df_ml_LA_setv2.csv", row.names = FALSE, fileEncoding ="UTF-8")  

fwrite(ml,"~/Documents/_R/Data.frame/202008_df_ml_LA_set.csv", row.names = FALSE, sep = ';') 

ml1 = fread("~/Documents/_R/Data.frame/202008_df_ml_LA_set.csv", sep=';') 

# 15.ACABOU !!! 
#     \0/  
#      |   
#     / \  
