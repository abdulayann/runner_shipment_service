ALTER TABLE pickup_delivery_details
    ADD COLUMN IF NOT EXISTS actual_delivery TIMESTAMP,
    ADD COLUMN IF NOT EXISTS estimated_delivery TIMESTAMP,
    ADD COLUMN IF NOT EXISTS delivery_gate_in TIMESTAMP,
    ADD COLUMN IF NOT EXISTS delivery_gate_out TIMESTAMP,
    ADD COLUMN IF NOT EXISTS actual_pickup TIMESTAMP,
    ADD COLUMN IF NOT EXISTS estimated_pickup TIMESTAMP,
    ADD COLUMN IF NOT EXISTS pickup_gate_out TIMESTAMP,
    ADD COLUMN IF NOT EXISTS pickup_gate_in TIMESTAMP;