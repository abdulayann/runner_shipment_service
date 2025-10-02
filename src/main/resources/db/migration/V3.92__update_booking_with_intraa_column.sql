ALTER TABLE IF EXISTS carrier_booking
    ADD COLUMN IF NOT EXISTS inttra_reference VARCHAR;

ALTER TABLE shipping_instruction
    ADD COLUMN IF NOT EXISTS requestor_id BIGINT,
    ADD CONSTRAINT fk_shipping_instruction_requestor FOREIGN KEY (requestor_id) REFERENCES parties(id);
