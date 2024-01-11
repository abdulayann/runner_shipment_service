ALTER TABLE IF EXISTS hbl_lock_settings
    ADD COLUMN bl_terms_and_conditions_id_lock boolean,
    ADD COLUMN cargo_terms_lock boolean,
    ADD COLUMN cargo_terms_description_lock boolean,
    ADD COLUMN bl_remarks_lock boolean,
    ADD COLUMN bl_remarks_description_lock boolean;