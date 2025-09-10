ALTER TABLE IF EXISTS packing
    ALTER COLUMN volume_per_pack TYPE NUMERIC,
    ALTER COLUMN cargo_weight_per_pack TYPE NUMERIC;

ALTER TABLE IF EXISTS containers
    ALTER COLUMN cargo_weight_per_container TYPE NUMERIC;