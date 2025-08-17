-- Add new columns to containers table
ALTER TABLE IF EXISTS packing
ADD COLUMN IF NOT EXISTS volume_per_pack NUMERIC(19,2),
ADD COLUMN IF NOT EXISTS volume_per_pack_unit VARCHAR,
ADD COLUMN IF NOT EXISTS cargo_weight_per_pack NUMERIC(19,2),
ADD COLUMN IF NOT EXISTS pack_weight_unit VARCHAR;


-- Add new columns to packing table
ALTER TABLE IF EXISTS containers
ADD COLUMN IF NOT EXISTS packages_per_container BIGINT,
ADD COLUMN IF NOT EXISTS container_package_type VARCHAR,
ADD COLUMN IF NOT EXISTS cargo_weight_per_container NUMERIC(19,2),
ADD COLUMN IF NOT EXISTS container_weight_unit VARCHAR;