options(repr.plot.height = 600)
#options(repr.plot.width = 1, repr.plot.height = 0.75, repr.plot.res = 100)

sc <- sparklyr::spark_connect(method = "databricks")
df <- DBI::dbGetQuery(sc,
                      "select * from tabela"
)


library(dplyr)
SparkR::sparkR.session()
df_t = SparkR::sql('select * from df ') %>% 
  SparkR::collect()  
