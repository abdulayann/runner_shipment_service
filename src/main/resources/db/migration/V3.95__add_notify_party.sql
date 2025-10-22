ALTER TABLE shipping_instruction
    ADD COLUMN IF NOT EXISTS notify_party_id BIGINT,
    ADD CONSTRAINT fk_shipping_instruction_notify_party FOREIGN KEY (notify_party_id) REFERENCES parties(id);
