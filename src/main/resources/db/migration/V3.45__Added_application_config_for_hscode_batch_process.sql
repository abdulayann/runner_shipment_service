INSERT INTO app_config (
    "key", value, guid, created_at, updated_at, created_by, updated_by, is_deleted
)
VALUES (
    'HS_CODE_BATCH_PROCESS_LIMIT', '100', uuid_generate_v4(), NOW(), NOW(), 'SYSTEM', 'SYSTEM', false
)
ON CONFLICT ("key") DO NOTHING;