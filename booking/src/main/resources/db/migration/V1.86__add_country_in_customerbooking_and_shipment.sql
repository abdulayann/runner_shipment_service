ALTER TABLE IF EXISTS customer_booking
    ADD COLUMN IF NOT EXISTS client_country VARCHAR,
    ADD COLUMN IF NOT EXISTS consignor_country VARCHAR,
    ADD COLUMN IF NOT EXISTS consignee_country VARCHAR,
    ADD COLUMN IF NOT EXISTS notify_party_country VARCHAR;

ALTER TABLE IF EXISTS shipment_details
    ADD COLUMN IF NOT EXISTS client_country VARCHAR,
    ADD COLUMN IF NOT EXISTS consignor_country VARCHAR,
    ADD COLUMN IF NOT EXISTS consignee_country VARCHAR,
    ADD COLUMN IF NOT EXISTS notify_party_country VARCHAR;