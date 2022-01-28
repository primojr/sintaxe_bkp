-- !preview conn=DBI::dbConnect(RSQLite::SQLite())

-- ****************************************************
-- *********  SELECAO CLIENTES PUSH *******************
-- ****************************************************
with   
basePush (
select 
   'Push' as canal
   ,date(ps.datetimesend) as date_partition
   ,hour(ps.datetimesend) as hora_envio
   ,ps.datetimesend as dt_hora_envio
   ,if(ps.Status ='Success', ps.UserId, null) as UserId
   ,replace(trim(ps.MessageName), char(9),' ') as MessageName
   ,ps.MessageContent as MessageContent
   ,IF(lower(left(trim(ps.MessageName), 2)) = 'r-' or lower(left(trim(ps.MessageName), 2)) = 'r_'
       ,UPPER(SUBSTR(SUBSTR(trim(ps.MessageName), 3, 1000), 1, INSTR(SUBSTR(trim(ps.MessageName), 3, 1000),"-") -1))
       ,UPPER(SUBSTR(SUBSTR(trim(ps.MessageName), 12, 1000), 1, INSTR(SUBSTR(trim(ps.MessageName), 12, 1000),"-") -1))
    ) as TipoCampanha
   ,((ps.DeviceId||ps.DateTimeSend||ps.MessageName||ps.userID||ps.PushJobId)) as chave_enviados 
   ,(if(ps.Status ='Success', ps.DeviceId||ps.userid||ps.MessageName|, null)) as chave_entregue
   ,(if(ps.MessageOpened ='yes' OR ps.InboxMessageOpened IS NOT NULL,ps.MessageName||ps.userID , null)) as chave_abertura
   ,null as click
   ,null as unsubscribe
   ,null as bounce
from dl_salesforce_crm.tb_push_sent ps
----------- TEMPO PARA DADOS INCREMENTAIS -----------------------
where
(
 ps.date_partition >= date_sub(now(), 2)
and upper(trim(ps.MessageName)) not like '%TEST%'
and upper(trim(ps.MessageName)) not like '%TST%'
and upper(trim(ps.MessageName)) like '%202%'
)
or ps.MessageName in ( '2021-11-23-PURCHASE_ACOM-VIRADA_BLAST-PUSH-teste'
                      ,'2021-11-28-PURCHASE_ACOM-BLAST-SMARTPHONE-PUSH-TESTE'
                      ,'2021-11-25-PURCHASE_ACOM-INATIVOSAMEATIVOSONUS_AUDIO-PUSH-teste'
                      )
 ),
 

-- ****************************************************
-- *********  TRANSACIONAL TRATADA E COM GRUPO  *******
-- * Filtro: Transações dos ultimos 40dias 
-- ****************************************************
 
tbOrderComGrupo (
 Select 
     t.customer_id
    ,t.order_date_partition as DatePartition
    ,t.order_created_at
    ,case
      when t.order_type_micro in ('PURCHASE_BR', 'PURCHASE_BR_GEOLOC', 'PURCHASE_BR_LINX' ) then 'PURCHASE_OFFUS_BR'
      when t.order_type_micro in ('PURCHASE_LASA_DELIVERY') then 'PURCHASE_LASA'
      else t.order_type_micro end as OrderTypeMicro
    ,t.order_id as OrderId
    ,(t.order_total_value) as TPV
    ,nvl(gr.Grupo,'N/D') as GrupoCampanha 
  from da_pandora.ds_transaction t
    left join dash_campanhas_grupo gr on (t.order_type_micro = gr.orderTypeMicro)
  Where t.order_date_partition >= date_sub(now(),5)
    and t.order_status in ('CAPTURED','AUTHORIZED')
    and t.order_type_micro <> 'GIFT_CASHBACK_IN'
),


-- ****************************************************
-- *********  CALCULA CONVERSÃO PUSH ******************
-- ****************************************************
conversaoPush (
 select 
     cp.MessageName
    ,cp.date_partition
    ,cp.hora_envio
   --,cp.dt_hora_envio
    ,cp.UserID
    ,cp.TipoCampanha
    ,cp.GrupoCampanha
    ,cp.chave_abertura
    ,cp.chave_entregue
    ,cp.chave_enviados
    ,cp.MessageContent
    ,cp.click
    ,cp.unsubscribe
    ,cp.bounce
    ,cp.canal
-- Conversao Micro
    ,if(t2.customer_id is not null, t2.OrderId, null) as TransacoesOrderTypeMicro
    ,if(t2.customer_id is not null, t2.TPV, null) as TPVOrderTypeMicro
    ,if(t2.customer_id is not null, 1, 0) as ConversaoOrderTypeMicro
-- Conversao Grupo 
    ,if(t3.customer_id is not null, t3.OrderId, null) as TransacoesGrupoOrderType
    ,if(t3.customer_id is not null, t3.TPV, null) as TPVGrupoOrderType
    ,if(t3.customer_id is not null, 1, 0) as ConversaoGrupoOrderType
-- Conversao Total
    ,if(t.customer_id  is not null, t.OrderId, null) as TransacoesTotal
    ,if(t.customer_id  is not null, t.TPV, null) as TPVTotal    
    ,if(t.customer_id  is not null, 1, 0) as ConversaoTotal
  from 
    (select
      aux.* 
    -- Se o grupo não for definido pelo order type assumo que o grupo está no nome da campanha (TipoCampanha)
      ,if(gr.Grupo is null, aux.TipoCampanha, gr.Grupo) as GrupoCampanha
    from basePush as aux 
      left join dash_campanhas_grupo gr on (gr.OrderTypeMicro = UPPER(SUBSTR(SUBSTR(aux.MessageName, 12, 1000), 1, INSTR(SUBSTR(aux.MessageName, 12, 1000),"-") -1)))
      ) cp
      left join  tbOrderComGrupo   as t on (cp.userID = t.Customer_id 
                                            and  ((bigint(t.order_created_at))-(bigint(cp.dt_hora_envio)))/(60) between 0 and 720) -- Transações até 12h pos envio
                                            -- and t.DatePartition between date(cp.date_partition) and date_add(date(cp.date_partition), 3))  --3 dias conversão após a campanha
      left join  tbOrderComGrupo   as t2 on (cp.userID = t2.Customer_id 
                                             and  ((bigint(t2.order_created_at))-(bigint(cp.dt_hora_envio)))/(60) between 0 and 720 -- Transações até 12h pos envio
                                             --and t2.DatePartition between date(cp.date_partition) and date_add(date(cp.date_partition), 3) --3 dias conversão após a campanha
                                             and cp.TipoCampanha = t2.OrderTypeMicro)
      left join  tbOrderComGrupo   as t3 on (cp.userID = t3.Customer_id 
                                             and ((bigint(t3.order_created_at))-(bigint(cp.dt_hora_envio)))/(60) between 0 and 720 -- Transações até 12h pos envio   
                                             --and t3.DatePartition between date(cp.date_partition) and date_add(date(cp.date_partition), 3) --3 dias conversão após a campanha
                                             and cp.GrupoCampanha = t3.GrupoCampanha)
),

git remote add origin https://github.com/primojr/backup_sintaxe.git
