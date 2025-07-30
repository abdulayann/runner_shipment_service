DO $$
BEGIN
    -- Check if the constraint exists
    IF EXISTS (
        SELECT 1
        FROM pg_constraint
        WHERE conname = 'section_details_section_name_key'
        AND conrelid = 'section_details'::regclass
    ) THEN
        ALTER TABLE section_details
        DROP CONSTRAINT section_details_section_name_key;
    END IF;
END $$;

-- Check if tenant_id column exists and add it if it doesn't
DO $$
BEGIN
    IF NOT EXISTS (SELECT 1
                   FROM information_schema.columns
                   WHERE table_name = 'section_details'
                   AND column_name = 'tenant_id') THEN
        ALTER TABLE section_details ADD COLUMN tenant_id INTEGER;
    END IF;
END $$;

-- Add unique constraint on (section_name, tenant_id) if it doesn't already exist
DO $$
BEGIN
    -- Check if the constraint already exists
    IF NOT EXISTS (
        SELECT 1
        FROM pg_constraint
        WHERE conname = 'unique_section_name_tenant'
    ) THEN
        ALTER TABLE section_details
        ADD CONSTRAINT unique_section_name_tenant UNIQUE (section_name, tenant_id);
    END IF;
END $$;
