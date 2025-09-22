ALTER TABLE IF EXISTS shipment_setting
    ADD COLUMN IF NOT EXISTS default_pack_unit varchar(255);