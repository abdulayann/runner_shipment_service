update consolidation_details
set receiving_branch=590
where id in(select id from consolidation_details 
where consolidation_number in('NBOCSS25071421') and source_guid  is null and tenant_id =706);

update shipment_details
set consignor_dps_address_id=95859
where shipment_id in('QINS25051571') and tenant_id=474;

DELETE FROM triangulation_partner_consolidation WHERE consolidation_id in(select id from consolidation_details where consolidation_number in('SNZCSS25060598') and
  tenant_id = 459);