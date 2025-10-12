ALTER TABLE hbl
    DROP COLUMN IF EXISTS hbl_freights_and_charges;

ALTER TABLE hbl
    ADD COLUMN IF NOT EXISTS hbl_revenue_charges jsonb;