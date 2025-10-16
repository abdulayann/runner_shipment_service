DELETE FROM network_transfer WHERE entity_id = 83617 AND entity_type = 'SHIPMENT' AND tenant_id = 465;
UPDATE shipment_details
SET receiving_branch = 465
WHERE id = 83617;