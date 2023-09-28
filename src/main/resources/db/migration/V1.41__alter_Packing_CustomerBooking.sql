ALTER TABLE IF EXISTS packing
    ADD COLUMN is_dimension BOOLEAN default false;
ALTER TABLE IF EXISTS customer_booking
    ADD COLUMN is_package_manual BOOLEAN default false;
