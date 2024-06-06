ALTER TABLE pickup_delivery_details
    ADD COLUMN IF NOT EXISTS pickup_delivery_gate_in TIMESTAMP,
    ADD COLUMN IF NOT EXISTS pickup_delivery_gate_out TIMESTAMP;
