ALTER TABLE IF EXISTS shipment_setting
    ADD COLUMN IF NOT EXISTS enable_lcl_consolidation boolean DEFAULT false;

ALTER TABLE IF EXISTS consolidation_details
    ADD COLUMN IF NOT EXISTS cfs_cut_off_date timestamp;

ALTER TABLE IF EXISTS shipment_details
    ADD COLUMN IF NOT EXISTS shipment_gate_in_date timestamp,
    ADD COLUMN IF NOT EXISTS date_type varchar(31),
    ADD COLUMN IF NOT EXISTS shipment_pack_status varchar(31);

ALTER TABLE IF EXISTS packing
    ADD COLUMN IF NOT EXISTS cargo_gate_in_date timestamp,
    ADD COLUMN IF NOT EXISTS date_type varchar(31);

ALTER TABLE IF EXISTS carrier_details
    ADD COLUMN IF NOT EXISTS cfs varchar(100);