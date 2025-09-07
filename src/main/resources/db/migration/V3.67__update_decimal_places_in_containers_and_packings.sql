ALTER TABLE IF EXISTS packing
    ALTER COLUMN IF EXISTS volume_per_pack TYPE NUMERIC,
    ALTER COLUMN IF EXISTS cargo_weight_per_pack TYPE NUMERIC;

ALTER TABLE IF EXISTS containers
    ALTER COLUMN IF EXISTS cargo_weight_per_container NUMERIC;