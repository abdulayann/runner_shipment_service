ALTER TABLE IF EXISTS allocations
    ADD COLUMN IF NOT EXISTS weight_volume numeric(19,2),
    ADD COLUMN IF NOT EXISTS weight_volume_unit varchar(16),
    ADD COLUMN IF NOT EXISTS packs INTEGER,
    ADD COLUMN IF NOT EXISTS packs_type varchar(16),
    ADD COLUMN IF NOT EXISTS container_count INTEGER,
    ADD COLUMN IF NOT EXISTS teu_count numeric(19,2);

ALTER TABLE IF EXISTS achieved_quantities
    ADD COLUMN IF NOT EXISTS packs INTEGER,
    ADD COLUMN IF NOT EXISTS packs_type varchar(16),
    ADD COLUMN IF NOT EXISTS container_count INTEGER,
    ADD COLUMN IF NOT EXISTS teu_count numeric(19,2);