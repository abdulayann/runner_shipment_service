
DELETE FROM network_transfer WHERE entity_id = 78598 AND entity_type = 'CONSOLIDATION' AND tenant_id = 728;
UPDATE consolidation_details
SET receiving_branch = 459
WHERE id = 78598;
