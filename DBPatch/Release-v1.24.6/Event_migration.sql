UPDATE events e
SET tenant_id = sd.tenant_id
FROM shipment_details sd
WHERE e.entity_id = sd.id
  AND e.entity_type = 'SHIPMENT'
  AND e.tenant_id != sd.tenant_id;