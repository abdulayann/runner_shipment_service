INSERT INTO app_config (
    "key", value, guid, created_at, updated_at, created_by, updated_by, is_deleted
)
VALUES (
    'ENABLE_CARRIER_ROUTING_MIGRATION_FOR_LOC_CODE', 'true', uuid_generate_v4(), NOW(), NOW(), 'SYSTEM', 'SYSTEM', false
)
ON CONFLICT ("key") DO NOTHING;