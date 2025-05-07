ALTER TABLE IF EXISTS consolidation_details
    ADD COLUMN IF NOT EXISTS is_borrowed BOOLEAN DEFAULT FALSE,
    ADD COLUMN IF NOT EXISTS borrowed_from_organization_id bigint;