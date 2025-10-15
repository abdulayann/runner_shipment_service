DELETE FROM network_transfer WHERE entity_id = 81744 AND entity_type = 'CONSOLIDATION' AND tenant_id = 459;

UPDATE consolidation_details
SET receiving_branch = 477
WHERE id = 81744