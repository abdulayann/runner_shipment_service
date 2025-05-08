ALTER TABLE IF EXISTS shipment_details
    ADD COLUMN IF NOT EXISTS borrowed_from bigint;