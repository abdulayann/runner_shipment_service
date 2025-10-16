DELETE FROM network_transfer WHERE entity_id = 74879 AND entity_type = 'CONSOLIDATION' AND tenant_id = 474;
UPDATE consolidation_details
SET receiving_branch = 551
WHERE id = 74879;