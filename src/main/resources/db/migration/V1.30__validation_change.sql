INSERT INTO validations(created_at, created_by, guid, updated_at, updated_by, tenant_id, entity, schema_validator,
                       lifecycle_hook)
VALUES (NOW(), 1, uuid_generate_v4(), NOW(), 1, 1, 'SHIPMENT',
       '{"properties":{"client":{"type":"object","properties":{"orgData":{"required":true},"addressData":{"required":true}}},"direction":{"required":true},"shipmentType":{"required":true},"transportMode":{"required":true},"carrierDetails":{"type":"object","properties":{"eta":{"type":"date-time","compare":[{"operator":"greater-than","compareTo":"carrierDetails.etd"}]},"origin":{"required":true},"destination":{"required":true}}},"bookingReference":{"type":"string","required":true}}}',
       'ON_CREATE')
ON CONFLICT (entity, lifecycle_hook, tenant_id)
DO UPDATE SET schema_validator = EXCLUDED.schema_validator;





INSERT INTO validations(created_at, created_by, guid, updated_at, updated_by, tenant_id, entity, schema_validator,
                       lifecycle_hook)
VALUES (NOW(), 1, uuid_generate_v4(), NOW(), 1, 1, 'ROUTING',
       '{"properties":{"leg":{"required":true},"mode":{"required":true}}}',
       'ON_CREATE')
ON CONFLICT (entity, lifecycle_hook, tenant_id)
DO UPDATE SET schema_validator = EXCLUDED.schema_validator;




INSERT INTO validations(created_at, created_by, guid, updated_at, updated_by, tenant_id, entity, schema_validator,
                       lifecycle_hook)
VALUES (NOW(), 1, uuid_generate_v4(), NOW(), 1, 1, 'CONTAINER',
       '{"properties":{"containerCode":{"required":true},"containerCount":{"required":true}}}',
       'ON_CREATE')
ON CONFLICT (entity, lifecycle_hook, tenant_id)
DO UPDATE SET schema_validator = EXCLUDED.schema_validator;





INSERT INTO validations(created_at, created_by, guid, updated_at, updated_by, tenant_id, entity, schema_validator,
                        lifecycle_hook)
VALUES (NOW(), 1, uuid_generate_v4(), NOW(), 1, 1, 'JOBS',
        '{"properties":{"currency":{"required":true},"incoTerm":{"required":true},"buyerDetail":{"type":"object","properties":{"orgCode":{"required":true}}},"orderNumber":{"required":true},"orderStatus":{"required":true},"serviceMode":{"required":true},"transportMode":{"required":true},"supplierDetail":{"type":"object","properties":{"orgCode":{"required":true}}},"countryOfOrigin":{"required":true}}}',
        'ON_CREATE')
ON CONFLICT (entity, lifecycle_hook, tenant_id)
DO UPDATE SET schema_validator = EXCLUDED.schema_validator;





INSERT INTO validations(created_at, created_by, guid, updated_at, updated_by, tenant_id, entity, schema_validator,
                        lifecycle_hook)
VALUES (NOW(), 1, uuid_generate_v4(), NOW(), 1, 1, 'EVENTS',
        '{"properties":{"eventCode":{"required":true}}}',
        'ON_CREATE')
ON CONFLICT (entity, lifecycle_hook, tenant_id)
DO UPDATE SET schema_validator = EXCLUDED.schema_validator;



INSERT INTO validations(created_at, created_by, guid, updated_at, updated_by, tenant_id, entity, schema_validator,
                        lifecycle_hook)
VALUES (NOW(), 1, uuid_generate_v4(), NOW(), 1, 1, 'CARRIAGE',
        '{"properties":{"carriageType":{"required":true}}}',
        'ON_CREATE')
ON CONFLICT (entity, lifecycle_hook, tenant_id)
DO UPDATE SET schema_validator = EXCLUDED.schema_validator;




INSERT INTO validations(created_at, created_by, guid, updated_at, updated_by, tenant_id, entity, schema_validator,
                       lifecycle_hook)
VALUES (NOW(), 1, uuid_generate_v4(), NOW(), 1, 1, 'CONSOLIDATION',
       '{"properties":{"firstLoad":{"required":true},"bookingType":{"required":true},"lastDischarge":{"required":true},"transportMode":{"required":true},"carrierDetails":{"type":"object","properties":{"origin":{"required":true},"destination":{"required":true}}},"consolidationType":{"required":true}}}',
       'ON_CREATE')
ON CONFLICT (entity, lifecycle_hook, tenant_id)
DO UPDATE SET schema_validator = EXCLUDED.schema_validator;





INSERT INTO validations(created_at, created_by, guid, updated_at, updated_by, tenant_id, entity, schema_validator,
                       lifecycle_hook)
VALUES (NOW(), 1, uuid_generate_v4(), NOW(), 1, 1, 'PACKING',
       '{"properties":{"commodity":{"required":true},"packsType":{"required":true},"packs":{"required":true}}}',
       'ON_CREATE')
ON CONFLICT (entity, lifecycle_hook, tenant_id)
DO UPDATE SET schema_validator = EXCLUDED.schema_validator;