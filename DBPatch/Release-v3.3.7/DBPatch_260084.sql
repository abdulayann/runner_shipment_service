DELETE FROM network_transfer WHERE entity_id = 74690 AND entity_type = 'CONSOLIDATION' AND tenant_id = 612;
UPDATE consolidation_details
SET receiving_branch = 676
WHERE id = 74690;