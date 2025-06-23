ALTER TABLE IF EXISTS consolidation_details
    ADD COLUMN IF NOT EXISTS hazardous boolean DEFAULT false;