ALTER TABLE IF EXISTS consolidation_details
    DROP COLUMN IF EXISTS booking_agent_number,
    DROP COLUMN IF EXISTS booking_agent_bl_number,
    ADD COLUMN IF NOT EXISTS co_load_carrier_name VARCHAR(64);

ALTER TABLE IF EXISTS shipment_details
    ADD COLUMN IF NOT EXISTS container_assigned_to_shipment_cargo bigint;