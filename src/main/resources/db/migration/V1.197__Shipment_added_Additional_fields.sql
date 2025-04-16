ALTER TABLE shipment_additional_details
    ADD COLUMN IF NOT EXISTS is_export_custom_clearance_completed BOOLEAN,
    ADD COLUMN IF NOT EXISTS bl_instruction_received TIMESTAMP,
    ADD COLUMN IF NOT EXISTS cargo_out_for_delivery TIMESTAMP;