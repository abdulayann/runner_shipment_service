DO $$
DECLARE
    old_schema TEXT := 'public';
    new_schema TEXT := '__SCHEMA__';
    r RECORD;
BEGIN
    -- Create the new schema dynamically
    EXECUTE format('CREATE SCHEMA %I', new_schema);

    -- Loop over each table in the old schema, excluding certain tables
    FOR r IN (
        SELECT tablename
        FROM pg_tables
        WHERE schemaname = old_schema
          AND tablename NOT IN (
              'events_dump',
              'logs_history',
              'date_time_change_logs',
              'audit_log',
              'shipment_backup',
			  'consolidation_backup',
              'customer_booking_backup',
              'network_transfer_backup'
          )
    ) LOOP
        -- Create table and copy data
        EXECUTE format(
            'CREATE TABLE %I.%I AS TABLE %I.%I',
            new_schema, r.tablename,
            old_schema, r.tablename
        );
    END LOOP;

    RAISE NOTICE '✅ Tables copied from "%" to "%" successfully, excluding specific tables.', old_schema, new_schema;
END $$;
