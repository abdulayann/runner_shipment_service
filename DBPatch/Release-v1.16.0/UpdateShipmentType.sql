update shipment_details
set direction = 'IMP'
where shipment_id in ('SGNS24060646', 'SGNS24060647') and tenant_id = 481;