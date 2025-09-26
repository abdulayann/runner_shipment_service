DELETE FROM network_transfer WHERE entity_id = 67637 AND entity_type = 'CONSOLIDATION' AND tenant_id = 490;

UPDATE consolidation_details
SET receiving_branch = 646
WHERE id = 67637;

DELETE FROM network_transfer WHERE entity_id = 68880 AND entity_type = 'SHIPMENT' AND tenant_id = 490;

UPDATE shipment_details
SET receiving_branch = 646
WHERE id = 88568;