ALTER TABLE IF EXISTS shipment_additional_details
    ADD COLUMN IF NOT EXISTS bl_terms_and_conditions_id varchar(16),
    ADD COLUMN IF NOT EXISTS bl_comments varchar(2500),
    ADD COLUMN IF NOT EXISTS cargo_terms varchar(16),
    ADD COLUMN IF NOT EXISTS cargo_terms_description varchar(2500),
    ADD COLUMN IF NOT EXISTS bl_remarks varchar(16),
    ADD COLUMN IF NOT EXISTS bl_remarks_description varchar(2500)
