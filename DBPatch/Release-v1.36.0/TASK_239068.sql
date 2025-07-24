update mawb_stocks_link set status = 'Unused', updated_at =NOW(), entity_type = null, entity_id = null, ship_cons_number = null where id=6676;


update consolidation_details set bol ='006-38375643' where id='61333';
update shipment_details set master_bill ='006-38375643' where id='80881';
update awb set awb_number='006-38375643' where id in ('1479','1481');
update mawb_stocks_link set mawb_number='006-38375643' where id='7270';

 