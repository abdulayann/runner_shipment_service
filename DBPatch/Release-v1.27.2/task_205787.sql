update shipment_details
set shipment_type = 'LCL'
where shipment_type = 'FCL'
and tenant_id = 520
and shipment_id in ('BCNS25021328', 'BCNS25021330');