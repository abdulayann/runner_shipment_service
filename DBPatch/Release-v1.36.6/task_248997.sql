--consolidation no DUBS25050288
DELETE FROM network_transfer WHERE entity_id = 45683 AND tenant_id = 525;

DELETE FROM triangulation_partner_consolidation WHERE consolidation_id = 45683;
