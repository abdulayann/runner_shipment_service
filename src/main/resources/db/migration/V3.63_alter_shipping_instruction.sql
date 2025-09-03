ALTER TABLE IF EXISTS shipment_instruction
ADD COLUMN IF NOT EXISTS sailing_information_id INTEGER;