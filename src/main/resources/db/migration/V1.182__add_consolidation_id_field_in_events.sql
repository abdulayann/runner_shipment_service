ALTER TABLE IF EXISTS events
    ADD COLUMN IF NOT EXISTS consolidation_id bigint,
    ADD COLUMN IF NOT EXISTS shipment_number varchar(50);

UPDATE events e set consolidation_id = e.entity_id where e.entity_type = 'CONSOLIDATION';
UPDATE events e set shipment_number = (select s.shipment_id from shipment_details s where s.id = e.entity_id) where entity_type = 'SHIPMENT';
