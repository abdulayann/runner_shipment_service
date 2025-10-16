DELETE FROM network_transfer WHERE entity_id = 82027 AND entity_type = 'CONSOLIDATION' AND tenant_id = 477;
UPDATE consolidation_details
SET receiving_branch = 459
WHERE id = 82027;