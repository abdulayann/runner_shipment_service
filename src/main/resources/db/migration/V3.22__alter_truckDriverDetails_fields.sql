ALTER TABLE IF EXISTS truck_driver_details
    ADD COLUMN IF NOT EXISTS driver_id VARCHAR(50);

DO $$
BEGIN
    IF EXISTS (
        SELECT 1 FROM information_schema.columns
        WHERE table_name='truck_driver_details' AND column_name='truck_number_plate'
    ) THEN
        ALTER TABLE truck_driver_details
            ALTER COLUMN truck_number_plate TYPE VARCHAR(50);
    END IF;

    IF EXISTS (
        SELECT 1 FROM information_schema.columns
        WHERE table_name='truck_driver_details' AND column_name='trailer_number_plate'
    ) THEN
        ALTER TABLE truck_driver_details
            ALTER COLUMN trailer_number_plate TYPE VARCHAR(50);
    END IF;
END $$;
