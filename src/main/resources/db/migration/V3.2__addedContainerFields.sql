ALTER TABLE IF EXISTS containers
        ADD COLUMN IF NOT EXISTS humidity numeric(19,2),
        ADD COLUMN IF NOT EXISTS vents numeric(19,2);