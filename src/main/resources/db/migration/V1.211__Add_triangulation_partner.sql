ALTER TABLE shipment_details ADD COLUMN triangulation_partner BIGINT;
ALTER TABLE consolidation_details ADD COLUMN triangulation_partner BIGINT;

UPDATE shipment_details s
SET triangulation_partner = t.triangulation_partner
FROM triangulation_partner_shipment t
WHERE s.id = t.shipment_id;

UPDATE consolidation_details s
SET triangulation_partner = t.triangulation_partner
FROM triangulation_partner_consolidation t
WHERE s.id = t.consolidation_id;