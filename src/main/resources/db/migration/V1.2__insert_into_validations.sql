INSERT INTO validations(created_at, created_by, guid, updated_at, updated_by, tenant_id, entity, schema_validator,
                        lifecycle_hook)
VALUES (NOW(), 1, uuid_generate_v4(), NOW(), 1, 1, 'SHIPMENT',
        '{"properties":{"transportMode":{"type":"string","required":true,"enum":["SEA","AIR","ROA"],"maxSize":3},"direction":{"required":true,"enum":["EXP","IMP"],"maxSize":3},"bookingReference":{"type":"string","required":true},"carrierDetails":{"type":"object","properties":{"origin":{"required":true},"destination":{"required":true},"eta":{"type":"date-time","compare":[{"compareTo":"carrierDetails.etd","operator":"greater-than"}]}}}}}',
        'ON_CREATE');
