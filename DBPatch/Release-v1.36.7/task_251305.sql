DELETE FROM network_transfer WHERE entity_id = 67476 and entity_type = 'CONSOLIDATION' and tenant_id = 709;

Update triangulation_partner_consolidation set partner_id = 685, is_accepted = false WHERE consolidation_id = 67476 and partner_id = 709;