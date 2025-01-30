ALTER TABLE IF EXISTS triangulation_partner_consolidation
    ADD COLUMN IF NOT EXISTS is_accepted BOOLEAN DEFAULT FALSE;

ALTER TABLE IF EXISTS triangulation_partner_shipment
    ADD COLUMN IF NOT EXISTS is_accepted BOOLEAN DEFAULT FALSE;