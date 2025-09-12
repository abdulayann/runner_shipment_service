--consolidation no SGHA25086339
DELETE FROM network_transfer WHERE entity_id = 68812 AND tenant_id = 551;

UPDATE consolidation_details
SET receiving_branch = 484
WHERE id = 68812;