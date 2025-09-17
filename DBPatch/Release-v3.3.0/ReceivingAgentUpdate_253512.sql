--consolidation no LNZS25074034
DELETE FROM network_transfer WHERE entity_id = 61699 AND entity_type = 'CONSOLIDATION' AND tenant_id = 713;

UPDATE consolidation_details
SET receiving_branch = 705
WHERE id = 61699;

DELETE FROM network_transfer WHERE entity_id = 68880 AND entity_type = 'SHIPMENT' AND tenant_id = 713;

UPDATE shipment_details
SET receiving_branch = 705
WHERE id = 68880;