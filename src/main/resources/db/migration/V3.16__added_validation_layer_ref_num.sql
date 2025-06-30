INSERT INTO validations(created_at, created_by, guid, updated_at, updated_by, tenant_id, entity, schema_validator,
                       lifecycle_hook)
VALUES (NOW(), 1, uuid_generate_v4(), NOW(), 1, 1, 'REFERENCE_NUMBERS',
       '{"properties":{"referenceNumber":{"required":true},"type":{"required":true}}}',
       'ON_CREATE')
ON CONFLICT (entity, lifecycle_hook, tenant_id)
DO UPDATE SET schema_validator = EXCLUDED.schema_validator;