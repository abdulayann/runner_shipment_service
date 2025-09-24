--consolidation no BOMS25080133
DELETE FROM network_transfer WHERE entity_id = 65858 AND tenant_id = 459;

UPDATE consolidation_details
SET receiving_branch = 477
WHERE id = 65858;