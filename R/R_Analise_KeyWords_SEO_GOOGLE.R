# Diretorios&Pacotes
rm(list = ls())
source("~/Documents/_R/Script/Apoio/pack.R")
#source("U:/_GERSON/01_DBM/Script R/")
setwd("~/03_Dashboard/02_SiteBosch/Base/Input/Iris")

library(forcats)
## lER A BASE

df <- read_excel("MT - Beta.xlsx", sheet = 'Keyword Heatmap Tool ', skip = 2)
names(df) <- df %>% clean_names() %>% names %>% abjutils::rm_accent()

df <- df %>% 
  mutate(segmentacao = campaign_name %>% sub(".*Goo", "Goo",.)
                       %>% sub(" - .*","",.) %>% str_to_lower() 
                       %>% str_remove_all(., 'google|[0-9]|niveladores|medidores|ii|\\[|\\]')
                       %>% rm_accent() %>% str_trim() 
                       %>% if_else(str_detect(.,"utili"),'utilizacao',.) 
                       %>% if_else(. == '', ad_group_name, . )
                   
  )

 df = df %>% filter(campanha == 'MT') 
 attach(df)

# CLASSIFICAÇAO DOS DADOS
df = df %>% group_by(year, month, campanha, pais, categoria, segmentacao, keyword) %>% 
  summarise(clicks = sum(clicks)
            ,cost = sum(cost)
            ,conversions = sum(conversions)) %>% 
  mutate(
    clicks = clicks %>% if_else(clicks > 10, clicks*.8, .)
    ,conversions = conversions %>% if_else(. > clicks, clicks,.)
    ,cpc = cost/clicks
    ,conversion_rate = conversions/clicks 
    ,cat_cpc = case_when(
      cpc < 0.25             ~ '01.$ < 0.25'
      ,cpc >= .25 & cpc < .50 ~ '02.$ .25 |- .50'
      ,cpc >= 0.5 & cpc < 1.0 ~ '03.$ .50 |- 1.0'
      ,cpc >= 1.0 & cpc < 1.5 ~ '04.$ 1.0 |- 1.5'
      ,cpc >= 1.5 & cpc < 2.0 ~ '05.$ 1.5 |- 2.0'
      ,cpc >= 2.0 & cpc < 5.0 ~ '06.$ 2.0 |- 5.0'
      ,cpc >= 5.0 & cpc < 10  ~ '07.$ 5.0 |- 10.0'
      ,cpc >= 10              ~ '08.$ 10[ou+]'
    )
    ,cat_conversao = case_when(
      conversion_rate == 0                           ~ '01. Sem Conversao'
      ,conversion_rate > 0 & conversion_rate < .05    ~ '02.% <05%'
      ,conversion_rate >= .05 & conversion_rate < .10 ~ '03. 05% |- 10%'
      ,conversion_rate >= .10 & conversion_rate < .20 ~ '04. 10% |- 20%'
      ,conversion_rate >= .20  ~ '05. 20%[ou+]')
)


df = as_tibble(df)
write.csv2(df, "~03_Dashboard/02_SiteBosch/Base/Output/base_keyWord.csv",
           row.names=FALSE, na = '0')

## DESCRICAO POR PAIS
# Taxa de conversao por pais
df1 = df %>% group_by(pais, categoria) %>% 
  summarise(conversions = sum(conversions)
            ,clicks = sum(clicks)
            ,cost   = sum(cost)
            ,cpc = cost/clicks
            ,conversion_rate = round(conversions/clicks,3)*100) %>% 
  mutate(Cat_Pais = str_c(pais,str_sub(categoria,1,1), sep = '_') %>% str_to_title()) %>% 
  arrange(Cat_Pais)

rownames(df1) = df1$Cat_Pais

#
# Gráfico de investimento 
df1 %>% group_by(categoria) %>%
  summarise(cost = sum(cost)) %>% 
  mutate(cost_ = round(cost/sum(cost),2)*100
         ,ymax = cumsum(cost_)
         ,ymin = c(0, head(ymax, n=-1))
         ,labelPosition = (ymax + ymin)/2
         ,label = paste0(categoria, "\n R$: ", cost)
         ) %>% 
  ggplot(., aes(ymax=ymax, ymin=ymin, xmax=4, xmin=3, fill=categoria)) +
  geom_rect() + geom_label(x=3.5, aes(y=labelPosition, label=label), size=3,colour = "white", fontface = "bold") +
  coord_polar(theta="y") +xlim(c(2, 4)) +theme_void() +
  theme(legend.position = "none") + 
  labs(title = 'Distribuição do Investimento')



## GRAFICO DE PEFORMACE
ggplot(df1, aes(x=cpc, y=conversion_rate)) +
  geom_point(aes(colour = factor(categoria)), size = 5) +
 geom_text(
    label= rownames(df1),size = 3, 
    nudge_x = 0.08, nudge_y = 0.04, 
    check_overlap = T) + 
 labs(
    title  = 'Analise - Palavras Chaves',
    subtitle = 'cpc x conversion_rate',
    x = "Custo por Click (BRL)",
    y = "Taxa de Conversão (%)",
    colour = "Categoria",
    shape = "Pais") +
  geom_vline(xintercept = 1, color="red", linetype="dashed", size=.8, name="Meta") +
  geom_hline(yintercept = 10, color="red", linetype="dashed", size=.8, name="Meta") +
  theme_light()



library(treemap)
## GRAFICO DE QUANTIDADE
df2 = df1 %>% group_by(categoria) %>% 
  mutate(pec_cost = round(cost/sum(cost),2)*100
         ,pec_clic = round(clicks/sum(clicks),2)*100
         ,pais_cust = str_c(pais, pec_cost, sep = "\n")
         ,pais_cust = paste0(pais_cust,"%")
         ,pais_click = str_c(pais, pec_clic, sep = "\n")
         ,pais_click = paste0(pais_click,"%")
  )


# NIVELADORES custo vs click
df_n = df2 %>% filter(categoria == 'Niveladores') %>% 
  select(categoria,pais_click,pais_cust,clicks,cost)

# treemap - custo
Invet_n = sum(df_n$cost) %>% paste("YTD R$",.)
treemap(df_n,index=c("pais_cust"),vSize="cost",type="value",vColor = "cost",
          border.col=c("white","white"),palette="RdBu",
          title = paste("Distribuição do Investimento [NIVELADORES]", Invet_n, sep = '\n'),
          align.labels = list(c("left","top"),c("center","center")),
          format.legend = list(scientific = FALSE, big.mark = " ")
  )

# treemap - Clique
Clicks_n = sum(df_n$clicks) %>% paste("YTD - ",.)
treemap(df_n,index=c("pais_click"),vSize="clicks",type="value",vColor = "clicks",
        border.col=c("white","white"),palette="RdBu",
        title = paste("Click por Pais [NIVELADORES]", Clicks_n, sep = '\n'),
        align.labels = list(c("left","top"),c("center","center")),
        format.legend = list(scientific = FALSE, big.mark = " ")
)

#
# MEDIDORES custo vs click
#
df_m = df2 %>% filter(categoria == 'Medidores') %>% 
  select(categoria,pais_click,pais_cust,clicks,cost)

# treemap - custo
Invet_n = sum(df_m$cost) %>% paste("YTD R$",.)
treemap(df_m,index=c("pais_cust"),vSize="cost",type="value",vColor = "cost",
        border.col=c("white","white"),palette="RdBu",
        title = paste("Investimento por Pais [MEDIDORES]", Invet_n, sep = '\n'),
        align.labels = list(c("left","top"),c("center","center")),
        format.legend = list(scientific = FALSE, big.mark = " ")
)

# treemap - Clique
Clicks_n = sum(df_m$clicks) %>% paste("YTD - ",.)
treemap(df_m,index=c("pais_click"),vSize="clicks",type="value",vColor = "clicks",
        border.col=c("white","white"),palette="RdBu",
        title = paste("Click por Pais [MEDIDORES]", Clicks_n, sep = '\n'),
        align.labels = list(c("left","top"),c("center","center")),
        format.legend = list(scientific = FALSE, big.mark = " ")
)



# TOP 15 PALAVRAS POR PAIS
rm(df1)
pais1 = 'México'
df1 = df %>% filter(pais == as.factor(pais1) & categoria == 'Niveladores') %>% arrange(desc(clicks)) %>% head(15) %>% 
  rbind(df %>% filter(pais == as.factor(pais1) & categoria == 'Medidores') %>% arrange(desc(clicks)) %>% head(15)) %>% 
  mutate(Cat_keys = str_c(keyword,str_sub(categoria,1,1), sep = '_') %>% str_to_title() %>% make_clean_names()
         ,conversion_rate = round(conversions/clicks,3)*100) 

rownames(df1) = df1$Cat_keys


## GRAFICO DE PEFORMACE
p1 = ggplot(df1, aes(x=cpc, y=conversion_rate, size = clicks)) +
  geom_point(aes(colour = factor(categoria)), size = 5) +
  geom_text(
    label= rownames(df1),size = 3, 
    nudge_x = 0.02, nudge_y = 0.02, 
    check_overlap = T) + 
  labs(
    subtitle = 'cpc x conversion_rate',
    x = "Custo por Click (BRL)",
    y = "Taxa de Conversão (%)",
    colour = "Categoria",
    shape = "keyword") +
  geom_vline(xintercept = 1, color="red", linetype="dashed", size=.8, name="Meta") +
  geom_hline(yintercept = 10, color="red", linetype="dashed", size=.8, name="Meta") +
  theme_light() + theme(legend.position = "bottom")


# Graph

p2 = df1 %>% filter(categoria  == 'Niveladores') %>% 
  mutate(keyword = fct_reorder(keyword, clicks)) %>% 
  ggplot(., aes(x=keyword, y=clicks)) +
  geom_bar(stat="identity", fill="#f68060", alpha=.8, width=.6) +
  coord_flip() +
  xlab("") +
  theme_light() +
  labs(subtitle = "Top 15 - Key Word - Niveladores"
       ,title = pais1)

p3 = df1 %>% filter(categoria  == 'Medidores') %>% 
  mutate(keyword = fct_reorder(keyword, clicks)) %>% 
  ggplot(., aes(x=keyword, y=clicks)) +
  geom_bar(stat="identity", fill="#f68060", alpha=.8, width=.6) +
  coord_flip() +
  xlab("") +
  theme_light() +
  labs(subtitle = "Top 15 - Key Word - Medidores")

(p2/p3) - (p1) 


