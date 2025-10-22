DELETE FROM network_transfer WHERE entity_id = 80639 AND entity_type = 'CONSOLIDATION' AND tenant_id = 590;
UPDATE consolidation_details
SET receiving_branch = 591
WHERE id = 80639;