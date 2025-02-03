ALTER TABLE IF EXISTS consolidation_details
    ADD COLUMN IF NOT EXISTS transfer_status varchar(255);

ALTER TABLE IF EXISTS shipment_details
    ADD COLUMN IF NOT EXISTS transfer_status varchar(255);
