DO $$
DECLARE
    old_schema TEXT := 'public';
    new_schema TEXT := 'public_' || to_char(now(), 'YYYYMMDD_HH24MI');
    r RECORD;
BEGIN
    -- Create the new schema dynamically
    EXECUTE format('CREATE SCHEMA %I', new_schema);

    -- Loop over each table in the old schema
    FOR r IN (
        SELECT tablename
        FROM pg_tables
        WHERE schemaname = old_schema
    ) LOOP
        -- Create table and copy data
        EXECUTE format(
            'CREATE TABLE %I.%I AS TABLE %I.%I',
            new_schema, r.tablename,
            old_schema, r.tablename
        );
    END LOOP;

    RAISE NOTICE '✅ Tables copied from "%" to "%" successfully.', old_schema, new_schema;
END $$;