INSERT INTO app_config (
    "key", value, guid, created_at, updated_at, created_by, updated_by, is_deleted
)
VALUES (
    'MANDATORY_FIELD_FOR_CONTAINER_UPLOAD',
    '["Type", "Cargo Wt.", "Cargo Wt. Unit"]'::jsonb,
    uuid_generate_v4(),
    NOW(),
    NOW(),
    'SYSTEM',
    'SYSTEM',
    false
)
ON CONFLICT ("key") DO NOTHING;