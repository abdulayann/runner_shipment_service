ALTER TABLE IF EXISTS consolidation_details
    ADD COLUMN IF NOT EXISTS reefer boolean DEFAULT false;