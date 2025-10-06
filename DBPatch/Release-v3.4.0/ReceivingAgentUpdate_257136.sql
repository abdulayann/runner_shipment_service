DELETE FROM network_transfer WHERE entity_id = 73917 AND entity_type = 'CONSOLIDATION' AND tenant_id = 474;
UPDATE consolidation_details
SET receiving_branch = 481
WHERE id = 73917 and tenant_id = 474;