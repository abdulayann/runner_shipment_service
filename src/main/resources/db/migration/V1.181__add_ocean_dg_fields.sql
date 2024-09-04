ALTER TABLE IF EXISTS packing
    ADD COLUMN IF NOT EXISTS un_number VARCHAR(31),
    ADD COLUMN IF NOT EXISTS proper_shipping_name VARCHAR(63),
    ADD COLUMN IF NOT EXISTS packing_group VARCHAR(31),
    ADD COLUMN IF NOT EXISTS minimum_flash_point numeric(19,2),
    ADD COLUMN IF NOT EXISTS minimum_flash_point_unit VARCHAR(3),
    ADD COLUMN IF NOT EXISTS marine_pollutant BOOLEAN DEFAULT FALSE;


ALTER TABLE IF EXISTS containers
    ADD COLUMN IF NOT EXISTS un_number VARCHAR(31),
    ADD COLUMN IF NOT EXISTS proper_shipping_name VARCHAR(63),
    ADD COLUMN IF NOT EXISTS packing_group VARCHAR(31),
    ADD COLUMN IF NOT EXISTS minimum_flash_point numeric(19,2),
    ADD COLUMN IF NOT EXISTS minimum_flash_point_unit VARCHAR(3),
    ADD COLUMN IF NOT EXISTS marine_pollutant BOOLEAN DEFAULT FALSE;