UPDATE shipment_details sd
SET is_deleted = 'true'
WHERE sd.shipment_id = 'ESTA24110539'
and tenant_id = 551;