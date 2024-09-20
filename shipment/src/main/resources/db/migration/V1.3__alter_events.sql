ALTER TABLE IF EXISTS events
    DROP COLUMN IF EXISTS shipment_id,
    DROP COLUMN IF EXISTS consolidation_id,
    ADD COLUMN IF NOT EXISTS entity_id bigint,
    ADD COLUMN IF NOT EXISTS entity_type varchar;