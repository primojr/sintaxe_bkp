
# Diretorios&Pacotes
#rm(list = ls())
source("~Documents/_R/Script/Apoio/pack.R")
setwd("~/03_Dashboard/02_SiteBosch/Base/Input/Iris")


df20 = read.csv2("~/03_Dashboard/02_SiteBosch/Base/Output/base_fb_IG20.csv")
# df20 = mutate(df20,ano = '2020')
# write.csv2(df20, "~/03_Dashboard/02_SiteBosch/Base/Output/base_fb_IG20.csv",row.names=FALSE, na = '0')

#
# 01. ler Base FaCEBOOK -----
#
fb <- read_excel("Bosch - Relatório Geral.xlsx", sheet = 'Base de dados - Report')
names(fb) <- fb %>% clean_names() %>% names %>% rm_accent()

fb = fb %>%  
  mutate_if(is.character, ~ str_to_lower(.)) %>% 
  mutate_if(is.character, ~ rm_accent(.)) %>% 
  mutate_if(is.numeric,   ~ replace_na(.,0))

# TRANSFORMAÇÃO DADOS FB
fb = fb %>% filter(is.na(campaign_name) == FALSE) %>% 
  mutate(
    campaign_name = str_remove(campaign_name,'\\"')
    %>% str_remove(., "^001 - ") 
    %>% str_replace_all(., 'x-lock',"x_lock")
    %>% str_trim()
    # ajuste ad_set_names + ad_name
  # PAIS  
   ,country = str_remove(campaign_name, "bosch ") 
    %>% str_remove(., " .*") 
    %>% if_else(nchar(.) > 3, str_sub(.,1,2),.) 
    %>% str_replace(.,'me', 'mx')
  # Tipo de aparelho
  ,device_type = case_when(
    str_detect(placement,'mobile')   ~ 'mobile'
   ,str_detect(placement,'desktop')  ~ 'desktop')
  ,type_midia = str_split_fixed(placement,"\\|",3)[,1]
  ,placement1 = str_split_fixed(placement,"\\|",3)[,2]
  ,placement = case_when( 
                   str_detect(placement1,"feed")    ~ 'feed'
                  ,str_detect(placement1,"search")  ~ 'search'
                  ,str_detect(placement1,"stories") ~ 'stories'
                  ,str_detect(placement1,"marketplace") ~ 'marketplace'
                  ,str_detect(placement1,'right_hand_colum') ~'right_hand_colum'
                  ,str_detect(placement1,'video') ~'video'
                 , TRUE ~ 'others')
    # NOME DA CAMPANHA
  , campaign = str_split_fixed(campaign_name," ",3)[,3] %>% str_remove(.,"-.*") 
  ,campaign0 = campaign_name %>% str_replace(.," -  - ",' - ') 
  , campaign = campaign 
               %>% if_else(.== '', str_split_fixed(campaign0," - ",3)[,2], .)
               %>% str_trim() %>% str_remove(.,"post |posts") 
               %>% if_else(str_detect(., "cordless$") == TRUE, "cordless",.) 
               %>% if_else(str_detect(.,"patrocina"),"post_patrocinado",.)
               %>% str_trim()
  )

fb = fb %>% select(-campaign_name, -campaign_id, -campaign0, -placement1) %>% mutate(ano = 2021)

fb = rbind(fb,df20)
fb$reach = ifelse(str_detect(fb$reach,"can")==T , 0,fb$reach)

write.csv2(fb, "~/03_Dashboard/02_SiteBosch/Base/Output/base_fb_IG.csv",
           row.names=FALSE, na = '0')


# %>% # FIM MUTATE  %>%
#   group_by(country) %>% 
#   summarise(soma = sum(reach), avg = mean(reach))

