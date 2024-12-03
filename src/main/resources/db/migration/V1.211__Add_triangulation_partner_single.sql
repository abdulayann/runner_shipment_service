ALTER TABLE IF EXISTS shipment_details
    ADD COLUMN IF NOT EXISTS triangulation_partner_single BIGINT;

ALTER TABLE IF EXISTS consolidation_details
    ADD COLUMN IF NOT EXISTS triangulation_partner_single BIGINT;



UPDATE shipment_details s
SET triangulation_partner_single = t.triangulation_partner
FROM triangulation_partner_shipment t
WHERE s.id = t.shipment_id;

UPDATE consolidation_details s
SET triangulation_partner_single = t.triangulation_partner
FROM triangulation_partner_consolidation t
WHERE s.id = t.consolidation_id;