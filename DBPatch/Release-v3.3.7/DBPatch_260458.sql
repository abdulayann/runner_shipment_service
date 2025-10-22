DELETE FROM network_transfer WHERE entity_id = 81780 AND entity_type = 'CONSOLIDATION' AND tenant_id = 612;
UPDATE consolidation_details
SET receiving_branch = 520
WHERE id = 81780;