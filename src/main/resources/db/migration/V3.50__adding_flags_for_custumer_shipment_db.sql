ALTER TABLE customer_booking
    ADD COLUMN IF NOT EXISTS is_shipper_client_equal BOOLEAN DEFAULT FALSE,
    ADD COLUMN IF NOT EXISTS is_consignee_client_equal BOOLEAN DEFAULT FALSE;

ALTER TABLE shipment_details
    ADD COLUMN IF NOT EXISTS is_shipper_client_equal BOOLEAN DEFAULT FALSE,
    ADD COLUMN IF NOT EXISTS is_consignee_client_equal BOOLEAN DEFAULT FALSE;
