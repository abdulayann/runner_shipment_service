-- Step 1: Create the trigger function
CREATE OR REPLACE FUNCTION updated_at_column()
RETURNS TRIGGER AS $$
DECLARE
    column_exists BOOLEAN;
BEGIN
    -- Check if the `updated_at` column exists in the current table
    SELECT EXISTS (
        SELECT 1
        FROM information_schema.columns
        WHERE table_name = TG_TABLE_NAME
        AND column_name = 'updated_at'
    ) INTO column_exists;

    -- If the `updated_at` column exists and a change occurred, update it
    IF column_exists AND (ROW(NEW.*) IS DISTINCT FROM ROW(OLD.*)) THEN
        NEW.updated_at = NOW();
    END IF;

    -- Return the modified row
    RETURN NEW;
END;
$$ LANGUAGE plpgsql;

-- Step 2: Loop through all tables and create a trigger if the `updated_at` column exists
DO $$ 
DECLARE
    table_record RECORD;
    column_exists BOOLEAN;
BEGIN
    -- Loop through all tables in the public schema
    FOR table_record IN 
        SELECT table_name
        FROM information_schema.tables
        WHERE table_schema = 'public'
          AND table_type = 'BASE TABLE'
    LOOP
        -- Check if the `updated_at` column exists in the current table
        SELECT EXISTS (
            SELECT 1
            FROM information_schema.columns
            WHERE table_name = table_record.table_name
            AND column_name = 'updated_at'
        ) INTO column_exists;

        -- Create the trigger if the `updated_at` column exists
        IF column_exists THEN
            EXECUTE format('CREATE TRIGGER update_timestamp_trigger
                            BEFORE UPDATE ON %I
                            FOR EACH ROW
                            EXECUTE FUNCTION updated_at_column();', table_record.table_name);
        END IF;
    END LOOP;
END $$;
