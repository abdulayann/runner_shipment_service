ALTER TABLE IF EXISTS consolidation_details
    DROP COLUMN arrival_departure_details_id,
    ADD COLUMN arrival_details_id bigint,
    ADD COLUMN departure_details_id bigint,
    ADD COLUMN sending_agent_id bigint,
    ADD COLUMN receiving_agent_id bigint,
    ADD COLUMN borrowed_from_id bigint,
    ADD COLUMN creditor_id bigint,
    ADD COLUMN co_load_with_id bigint;