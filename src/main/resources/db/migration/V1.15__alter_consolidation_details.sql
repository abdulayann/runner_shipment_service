ALTER TABLE IF EXISTS consolidation_details
    DROP COLUMN arrival_departure_details_id,
    ADD COLUMN arrival_details_id,
    ADD COLUMN departure_details_id,
    ADD COLUMN sending_agent_id,
    ADD COLUMN receiving_agent_id,
    ADD COLUMN borrowed_from_id,
    ADD COLUMN creditor_id,
    ADD COLUMN co_load_with_id;