DELETE FROM network_transfer WHERE entity_id = 55428 AND entity_type = 'CONSOLIDATION' AND tenant_id = 612;
UPDATE consolidation_details
SET receiving_branch = 676
WHERE id = 55428;