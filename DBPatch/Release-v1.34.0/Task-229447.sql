UPDATE shipment_details
SET shipment_type = 'LCL'
WHERE shipment_id IN ('SNZS25063531', 'SNZS25063534', 'SNZS25063536')
  AND tenant_id = 583;