-- !preview conn=DBI::dbConnect(RSQLite::SQLite())

with regra_06 as ( -- Cartao verificado e ao menos uma compra
      with pand as (
        select distinct
          wallet_document_number, min(order_date_partition) as date_partition_regra_cc
        from da_pandora.ds_transaction
        where refund_value is null and order_status ='CAPTURED' and tpv = true
--         and order_date_partition >= (current_date() - interval 30 day)
        group by 1
        having count(*) > 0
      ),
      auth_card as (
        select distinct
          tbown.document
        from dl_wallet.tb_creditcard tbcred  
        left join dl_wallet.tb_wallet tbwall  on tbcred.wallet_id = tbwall.id
        left join dl_wallet.tb_owner tbown    on tbwall.owner_id = tbown.id
        where tbcred.verified_by_ame >= 1
      )
      select ac.document, date_partition_regra_cc
      from auth_card ac
      inner join pand p on ac.document=p.wallet_document_number
),

regra_06_b as (  --  ter 2 transacoes na Ame
  select wallet_document_number, order_date_partition as date_partition_regra_transacao
  from (
          select regra_b.wallet_document_number, order_date_partition, row_number()over(partition by regra_b.wallet_document_number order by order_created_at) as rn
          from da_pandora.ds_transaction
          join (
                    select distinct
                      wallet_document_number
                    from da_pandora.ds_transaction
                    where refund_value is null and order_status ='CAPTURED' and tpv = true
                    --and order_date_partition < (current_date() - interval 30 day)
                    group by 1
                    having count(*) >= 2 and sum(order_total_value) >= 200
          ) regra_b on ds_transaction.wallet_document_number = regra_b.wallet_document_number
          where refund_value is null and order_status ='CAPTURED' and tpv = true
  ) where rn = 2
),

agg_regras as (
  select coalesce(document, wallet_document_number) as document, date_partition_regra_cc, date_partition_regra_transacao
  from regra_06
  full join regra_06_b on regra_06.document = regra_06_b.wallet_document_number  -- full join regra_cc ou regra_transac
)
