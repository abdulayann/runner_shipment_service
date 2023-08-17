ALTER TABLE validations
ADD CONSTRAINT unique_entity_lifecycle_hook UNIQUE (entity, lifecycle_hook);





INSERT INTO validations(created_at, created_by, guid, updated_at, updated_by, tenant_id, entity, schema_validator,
                        lifecycle_hook)
VALUES (NOW(), 1, uuid_generate_v4(), NOW(), 1, 1, 'SHIPMENT',
        '{"properties":{"client":{"type":"object","properties":{"orgData":{"required":true},"addressData":{"required":true}}},"direction":{"enum":["EXP","IMP"],"maxSize":3,"required":true},"shipmentType":{"enum":["FCL","LCL","BBK","BLK","LQD","ROR"],"maxSize":3,"required":true},"transportMode":{"enum":["SEA","AIR","ROA","COU","FAS","FSA","MAI","RAI"],"type":"string","maxSize":3,"required":true},"carrierDetails":{"type":"object","properties":{"eta":{"type":"date-time","compare":[{"operator":"greater-than","compareTo":"carrierDetails.etd"}]},"origin":{"required":true},"destination":{"required":true}}},"bookingReference":{"type":"string","required":true}}}',
        'ON_CREATE')
ON CONFLICT (entity, lifecycle_hook)
DO UPDATE SET schema_validator = EXCLUDED.schema_validator;





INSERT INTO validations(created_at, created_by, guid, updated_at, updated_by, tenant_id, entity, schema_validator,
                        lifecycle_hook)
VALUES (NOW(), 1, uuid_generate_v4(), NOW(), 1, 1, 'CONSOLIDATION',
        '{"properties":{"firstLoad":{"required":true},"bookingType":{"enum":["EXP","IMP"],"maxSize":3,"required":true},"lastDischarge":{"required":true},"transportMode":{"enum":["SEA","AIR","ROA","RAI"],"type":"string","maxSize":3,"required":true},"carrierDetails":{"type":"object","properties":{"origin":{"required":true},"destination":{"required":true}}},"consolidationType":{"enum":["AGT","CHT","CLA","CLD"],"maxSize":3,"required":true}}}',
        'ON_CREATE')
ON CONFLICT (entity, lifecycle_hook)
DO UPDATE SET schema_validator = EXCLUDED.schema_validator;





INSERT INTO validations(created_at, created_by, guid, updated_at, updated_by, tenant_id, entity, schema_validator,
                        lifecycle_hook)
VALUES (NOW(), 1, uuid_generate_v4(), NOW(), 1, 1, 'ROUTING',
        '{"properties":{"leg":{"required":true},"mode":{"enum":["AIR","RAI","ROW","SEA"],"type":"string","maxSize":3,"required":true},"routingStatus":{"enum":["CFD","HLD","PLD"],"type":"string","maxSize":3}}}',
        'ON_CREATE')
ON CONFLICT (entity, lifecycle_hook)
DO UPDATE SET schema_validator = EXCLUDED.schema_validator;





INSERT INTO validations(created_at, created_by, guid, updated_at, updated_by, tenant_id, entity, schema_validator,
                        lifecycle_hook)
VALUES (NOW(), 1, uuid_generate_v4(), NOW(), 1, 1, 'PACKING',
        '{"properties":{"packsType":{"enum":["AMM","BAG","BAL","BBG","BBK","BBL","BDL","BEM","BIC","BIN","BKG","BKT","BLC","BLE","BLK","BLU","BND","BOB","BOT","BOX","BRG","BSK","BXI","BXT","CAB","CAG","CAN","CAR","CAS","CBC","CBY","CCS","CHE","CHS","CLD","CNA","CNB","CNC","CND","CNE","CNF","CNT","CNX","COI","COL","CON","COR","CRD","CRT","CSK","CTN","CUB","CYL","DBK","DOC","DOZ","DRK","DRM","DSK","DTB","DUF","ENV","FIR","FLO","FLX","FRM","FSK","FTR","FWR","GAL","GOH","GRS","HED","HGH","HMP","HPT","HRB","HRK","HTB","JAR","JUG","KEG","KIT","KRK","KTB","LBK","LIF","LOG","LSE","LUG","LVN","MIX","MLV","MPK","MRP","MSV","MXD","NOL","OVW","PAI","PCE","PCK","PCL","PCS","PIR","PKG","PLF","PLN","PLT","POV","PRK","QTR","RAL","RCK","REL","RLL","ROL","RVR","SAK","SBC","SCS","SHK","SHT","SID","SKD","SKE","SLP","SLV","SPI","SPL","SVN","TBE","TBN","TIN","TKR","TKT","TLD","TNK","TOT","TRC","TRI","TRK","TRY","TSS","TTC","TUB","UNP","UNT","VEH","VPK","WDC","WHE","WLC","WRP"],"maxSize":3,"required":true},"commodity":{"required":true},"innerPacksCount":{"required":true}}}',
        'ON_CREATE')
ON CONFLICT (entity, lifecycle_hook)
DO UPDATE SET schema_validator = EXCLUDED.schema_validator;





INSERT INTO validations(created_at, created_by, guid, updated_at, updated_by, tenant_id, entity, schema_validator,
                        lifecycle_hook)
VALUES (NOW(), 1, uuid_generate_v4(), NOW(), 1, 1, 'CONTAINER',
        '{"properties":{"containerCode":{"enum":["20DR","20FR","20FT","20GP","20HC","20HD","20NOR","20OT","20PF","20PL","20RE","20RF","40DR","40footGP","40GP","40HC","40NOR","40OT","40PL","40RE","40REHC","40RF","42GP","45HC"],"required":true},"containerCount":{"required":true}}}',
        'ON_CREATE')
ON CONFLICT (entity, lifecycle_hook)
DO UPDATE SET schema_validator = EXCLUDED.schema_validator;