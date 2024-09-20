ALTER TABLE IF EXISTS shipment_details
    ADD COLUMN IF NOT EXISTS order_management_number varchar;