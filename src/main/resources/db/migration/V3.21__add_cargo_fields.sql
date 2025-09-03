ALTER TABLE IF EXISTS allocations
    ADD COLUMN IF NOT EXISTS dg_packs INTEGER,
    ADD COLUMN IF NOT EXISTS dg_packs_type varchar(16),
    ADD COLUMN IF NOT EXISTS dg_container_count INTEGER;

ALTER TABLE IF EXISTS achieved_quantities
    ADD COLUMN IF NOT EXISTS dg_packs INTEGER,
    ADD COLUMN IF NOT EXISTS dg_packs_type varchar(16),
    ADD COLUMN IF NOT EXISTS dg_container_count INTEGER,
    ADD COLUMN IF NOT EXISTS slac_count INTEGER;