DELETE FROM network_transfer WHERE entity_id = 78307 AND entity_type = 'CONSOLIDATION' AND tenant_id = 523;

UPDATE consolidation_details
SET receiving_branch = 591
WHERE id = 78307