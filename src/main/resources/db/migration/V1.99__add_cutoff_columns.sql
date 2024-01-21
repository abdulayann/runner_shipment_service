ALTER TABLE IF EXISTS consolidation_details
    ADD COLUMN latest_full_equ_delivered_to_carrier timestamp,
    ADD COLUMN earliest_drop_off_full_equ_to_carrier timestamp,
    ADD COLUMN earliest_empty_equ_pick_up timestamp;