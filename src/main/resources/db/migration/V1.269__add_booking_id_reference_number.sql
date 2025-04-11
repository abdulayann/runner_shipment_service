-- Step 1: Add the foreign key column to reference customer_booking
ALTER TABLE reference_numbers
ADD COLUMN booking_id BIGINT;

-- Step 2: Add the foreign key constraint
ALTER TABLE reference_numbers
ADD CONSTRAINT fk_reference_booking
FOREIGN KEY (booking_id) REFERENCES customer_booking(id);
