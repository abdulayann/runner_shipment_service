ALTER TABLE ti_packages
ADD COLUMN proper_shipping_name VARCHAR(63),
ADD COLUMN packing_group VARCHAR(31),
ADD COLUMN minimum_flash_point numeric,
ADD COLUMN minimum_flash_point_unit VARCHAR(3),
ADD COLUMN marine_pollutant BOOLEAN DEFAULT false,
ADD COLUMN dg_class_description VARCHAR(255);

ALTER TABLE ti_containers
ADD COLUMN proper_shipping_name VARCHAR(63),
ADD COLUMN packing_group VARCHAR(31),
ADD COLUMN minimum_flash_point numeric,
ADD COLUMN minimum_flash_point_unit VARCHAR(3),
ADD COLUMN marine_pollutant BOOLEAN DEFAULT false;