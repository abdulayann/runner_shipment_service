ALTER TABLE IF EXISTS customer_booking
    ADD COLUMN is_consignor_address_free_text BOOLEAN default false,
    ADD COLUMN is_consignee_address_free_text BOOLEAN default false,
    ADD COLUMN is_customer_address_free_text BOOLEAN default false,
    ADD COLUMN is_notify_party_address_free_text BOOLEAN default false;

