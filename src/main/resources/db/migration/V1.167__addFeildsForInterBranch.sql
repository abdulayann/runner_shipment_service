ALTER TABLE IF EXISTS console_shipment_mapping
    ADD COLUMN IF NOT EXISTS is_attachment_done BOOLEAN DEFAULT TRUE,
    ADD COLUMN If NOT EXISTS request_type int;

ALTER TABLE IF EXISTS consolidation_details
    ADD COLUMN IF NOT EXISTS open_for_attachment BOOLEAN DEFAULT TRUE,
    ADD COLUMN IF NOT EXISTS open_for_interbranch_attachment BOOLEAN DEFAULT FALSE;
