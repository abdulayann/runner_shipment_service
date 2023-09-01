ALTER TABLE IF EXISTS consolidation_details
    DROP COLUMN do_place_of_issue_id,
    ADD COLUMN do_place_of_issue VARCHAR,
    ADD COLUMN place_of_issue VARCHAR;