ALTER TABLE IF EXISTS shipment_setting
    ADD COLUMN IF NOT EXISTS iata_tact_flag boolean DEFAULT false;