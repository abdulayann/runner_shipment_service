ALTER TABLE IF EXISTS shipment_setting
    ADD COLUMN IF NOT EXISTS weight_decimal_place INTEGER,
    ADD COLUMN IF NOT EXISTS volume_decimal_place INTEGER;
