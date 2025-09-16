--shipment no HR2A25090362
DELETE FROM network_transfer WHERE entity_id = 99156 AND entity_type = 'SHIPMENT' AND tenant_id = 663;

UPDATE shipment_details
SET receiving_branch = 477
WHERE id = 99156;