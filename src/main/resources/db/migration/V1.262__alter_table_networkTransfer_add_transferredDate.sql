ALTER TABLE IF EXISTS network_transfer
    Add COLUMN IF NOT EXISTS transferred_date timestamp;
