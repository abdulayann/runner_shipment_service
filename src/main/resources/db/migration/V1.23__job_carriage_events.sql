ALTER TABLE validations DROP CONSTRAINT unique_entity_lifecycle_hook;

ALTER TABLE validations
ADD CONSTRAINT unique_entity_lifecycle_hook_tenantId UNIQUE (entity, lifecycle_hook, tenant_id);



INSERT INTO validations(created_at, created_by, guid, updated_at, updated_by, tenant_id, entity, schema_validator,
                        lifecycle_hook)
VALUES (NOW(), 1, uuid_generate_v4(), NOW(), 1, 1, 'JOBS',
        '{"properties":{"buyerDetail":{"type":"object","properties":{"orgCode":{"required":true}}},"supplierDetail":{"type":"object","properties":{"orgCode":{"required":true}}},"orderNumber":{"required":true},"orderStatus":{"enum":["CAN","CNF","DLV","INC","PLC","PRT","SHP"],"maxSize":3,"required":true},"transportMode":{"enum":["AIR","BGD","BGI","CNIB","COU","FAS","FSA","Intermodal","MAI","RAI","ROA","SEA","UNK"],"required":true},"serviceMode":{"enum":["D2D","D2P","F2P","I2P","Interland","Outerland","P2D","P2I","P2P"],"required":true},"incoTerm":{"enum":["CFR","CIF","CIP","CPT","DAP","DAT","DDP","EXW","FAS","FCA","FOB"],"maxSize":3,"required":true},"currency":{"enum":["AED","AUD","BGN","BRL","CAD","CHF","CNY","CZK","DKK","EUR","GBP","HKD","HRK","HUF","IDR","ILS","INR","ISK","JPY","KRW","MXN","MYR","NOK","NZD","OFF","ON","PHP","PLN","RON","RUB","SEK","SGD","THB","TRY","USD","ZAR"],"required":true},"countryOfOrigin":{"enum":["ABW","AFG","AGO","AIA","ALA","ALB","AND","ARE","ARG","ARM","ASM","ATA","ATF","ATG","AUS","AUT","AZE","BDI","BEL","BEN","BES","BFA","BGD","BGR","BHR","BHS","BIH","BLM","BLR","BLZ","BMU","BOL","BRA","BRB","BRN","BTN","BVT","BWA","CAF","CAN","CCK","CHE","CHL","CHN","CIV","CMR","COD","COG","COK","COL","COM","CPV","CRI","CUB","CUW","CXR","CYM","CYP","CZE","DEU","DJI","DMA","DNK","DOM","DZA","ECU","EGY","ERI","ESH","ESP","EST","ETH","FIN","FJI","FLK","FRA","FRO","FSM","GAB","GBR","GEO","GGY","GHA","GIB","GIN","GLP","GMB","GNB","GNQ","GRC","GRD","GRL","GTM","GUF","GUM","GUY","HKG","HMD","HND","HRV","HTI","HUN","IDN","IMN","IND","IOT","IRL","IRN","IRQ","ISL","ISR","ITA","JAM","JEY","JOR","JPN","KAZ","KEN","KGZ","KHM","KIR","KNA","KOR","KWT","LAO","LBN","LBR","LBY","LCA","LIE","LKA","LSO","LTU","LUX","LVA","MAC","MAF","MAR","MCO","MDA","MDG","MDV","MEX","MHL","MKD","MLI","MLT","MMR","MNE","MNG","MNP","MOZ","MRT","MSR","MTQ","MUS","MWI","MYS","MYT","MZM","NAM","NCL","NER","NFK","NGA","NIC","NIU","NLD","NOR","NPL","NRU","NZL","OMN","PAK","PAN","PCN","PER","PHL","PLW","PNG","POL","PRI","PRK","PRT","PRY","PSE","PYF","QAT","REU","ROU","RUS","RWA","SAU","SDN","SEN","SGP","SGS","SHN","SJM","SLB","SLE","SLV","SMR","SOM","SPM","SRB","SSD","STP","SUR","SVK","SVN","SWE","SWZ","SXM","SYC","SYR","TCA","TCD","TGO","THA","TJK","TKL","TKM","TLS","TON","TTO","TUN","TUR","TUV","TWN","TZA","UGA","UKR","UMI","URY","UZB","VAT","VCT","VEN","VGB","VIR","VNM","VUT","WLF","WSM","YEM","ZAF","ZMB","ZWE"],"required":true}}}',
        'ON_CREATE')
ON CONFLICT (entity, lifecycle_hook, tenant_id)
DO UPDATE SET schema_validator = EXCLUDED.schema_validator;



INSERT INTO validations(created_at, created_by, guid, updated_at, updated_by, tenant_id, entity, schema_validator,
                        lifecycle_hook)
VALUES (NOW(), 1, uuid_generate_v4(), NOW(), 1, 1, 'EVENTS',
        '{"properties":{"eventCode":{"enum":["ARVBRG","ARVDEST","ARVORG","ARVRL","ARVRLS","ARVTRN","BKNGCONFM","BRTHDEST","BRTHTRN","CAROWUS","CARREL","CFG","CNPKUPTK","CNRECCAR","CNRETCAR","CNTNOASN","CNTRN","CNTRNS","CRGARVCFS","CRGARVWH","CRGDLV","CUSREL","DPRBRG","DPRORG","DPRRL","DPRTK","DPRTRN","DSGBRG","DSGRL","DSGVS","DSGVSTRN","EMPCNDLV","EMPCNGI","EMPCNGO","EMPCNNTH","EMPCNPKU","EMPCNRELCAR","EMPCNRTN","FLCNDLV","FLCNGI","FLCNGO","FLCNPKU","HBGNTD","IMPCUSHLD","LODBRG","LODRL","LODVS","OCF","OPL","ORD","OSH","RDYLOD","SHPCMPLT","SHPCNFRM","STFCMP","STFSTR","TPIELCRT","USTFCMP","USTFSTR","VSARV","VSDPR"],"required":true}}}',
        'ON_CREATE')
ON CONFLICT (entity, lifecycle_hook, tenant_id)
DO UPDATE SET schema_validator = EXCLUDED.schema_validator;



INSERT INTO validations(created_at, created_by, guid, updated_at, updated_by, tenant_id, entity, schema_validator,
                        lifecycle_hook)
VALUES (NOW(), 1, uuid_generate_v4(), NOW(), 1, 1, 'CARRIAGE',
        '{"properties":{"carriageType":{"enum":["Main","OnCarriage","PreCarriage"],"type":"string","required":true}}}',
        'ON_CREATE')
ON CONFLICT (entity, lifecycle_hook, tenant_id)
DO UPDATE SET schema_validator = EXCLUDED.schema_validator;


INSERT INTO validations(created_at, created_by, guid, updated_at, updated_by, tenant_id, entity, schema_validator,
                       lifecycle_hook)
VALUES (NOW(), 1, uuid_generate_v4(), NOW(), 1, 1, 'SHIPMENT',
       '{"properties":{"client":{"type":"object","properties":{"orgData":{"required":true},"addressData":{"required":true}}},"direction":{"enum":["EXP","IMP"],"maxSize":3,"required":true},"shipmentType":{"enum":["FCL","LCL","BBK","BLK","LQD","ROR"],"maxSize":3,"required":true},"transportMode":{"enum":["SEA","AIR","ROA","COU","FAS","FSA","MAI","RAI"],"type":"string","maxSize":3,"required":true},"carrierDetails":{"type":"object","properties":{"eta":{"type":"date-time","compare":[{"operator":"greater-than","compareTo":"carrierDetails.etd"}]},"origin":{"required":true},"destination":{"required":true}}},"bookingReference":{"type":"string","required":true}}}',
       'ON_CREATE')
ON CONFLICT (entity, lifecycle_hook, tenant_id)
DO UPDATE SET schema_validator = EXCLUDED.schema_validator;





INSERT INTO validations(created_at, created_by, guid, updated_at, updated_by, tenant_id, entity, schema_validator,
                       lifecycle_hook)
VALUES (NOW(), 1, uuid_generate_v4(), NOW(), 1, 1, 'CONSOLIDATION',
       '{"properties":{"firstLoad":{"required":true},"bookingType":{"enum":["EXP","IMP"],"maxSize":3,"required":true},"lastDischarge":{"required":true},"transportMode":{"enum":["SEA","AIR","ROA","RAI"],"type":"string","maxSize":3,"required":true},"carrierDetails":{"type":"object","properties":{"origin":{"required":true},"destination":{"required":true}}},"consolidationType":{"enum":["AGT","CHT","CLA","CLD"],"maxSize":3,"required":true}}}',
       'ON_CREATE')
ON CONFLICT (entity, lifecycle_hook, tenant_id)
DO UPDATE SET schema_validator = EXCLUDED.schema_validator;





INSERT INTO validations(created_at, created_by, guid, updated_at, updated_by, tenant_id, entity, schema_validator,
                       lifecycle_hook)
VALUES (NOW(), 1, uuid_generate_v4(), NOW(), 1, 1, 'ROUTING',
       '{"properties":{"leg":{"required":true},"mode":{"enum":["AIR","RAI","ROW","SEA"],"type":"string","maxSize":3,"required":true},"routingStatus":{"enum":["CFD","HLD","PLD"],"type":"string","maxSize":3}}}',
       'ON_CREATE')
ON CONFLICT (entity, lifecycle_hook, tenant_id)
DO UPDATE SET schema_validator = EXCLUDED.schema_validator;



INSERT INTO validations(created_at, created_by, guid, updated_at, updated_by, tenant_id, entity, schema_validator,
                       lifecycle_hook)
VALUES (NOW(), 1, uuid_generate_v4(), NOW(), 1, 1, 'PACKING',
       '{"properties":{"packsType":{"enum":["AMM","BAG","BAL","BBG","BBK","BBL","BDL","BEM","BIC","BIN","BKG","BKT","BLC","BLE","BLK","BLU","BND","BOB","BOT","BOX","BRG","BSK","BXI","BXT","CAB","CAG","CAN","CAR","CAS","CBC","CBY","CCS","CHE","CHS","CLD","CNA","CNB","CNC","CND","CNE","CNF","CNT","CNX","COI","COL","CON","COR","CRD","CRT","CSK","CTN","CUB","CYL","DBK","DOC","DOZ","DRK","DRM","DSK","DTB","DUF","ENV","FIR","FLO","FLX","FRM","FSK","FTR","FWR","GAL","GOH","GRS","HED","HGH","HMP","HPT","HRB","HRK","HTB","JAR","JUG","KEG","KIT","KRK","KTB","LBK","LIF","LOG","LSE","LUG","LVN","MIX","MLV","MPK","MRP","MSV","MXD","NOL","OVW","PAI","PCE","PCK","PCL","PCS","PIR","PKG","PLF","PLN","PLT","POV","PRK","QTR","RAL","RCK","REL","RLL","ROL","RVR","SAK","SBC","SCS","SHK","SHT","SID","SKD","SKE","SLP","SLV","SPI","SPL","SVN","TBE","TBN","TIN","TKR","TKT","TLD","TNK","TOT","TRC","TRI","TRK","TRY","TSS","TTC","TUB","UNP","UNT","VEH","VPK","WDC","WHE","WLC","WRP"],"maxSize":3,"required":true},"commodity":{"required":true},"innerPacksCount":{"required":true}}}',
       'ON_CREATE')
ON CONFLICT (entity, lifecycle_hook, tenant_id)
DO UPDATE SET schema_validator = EXCLUDED.schema_validator;



INSERT INTO validations(created_at, created_by, guid, updated_at, updated_by, tenant_id, entity, schema_validator,
                       lifecycle_hook)
VALUES (NOW(), 1, uuid_generate_v4(), NOW(), 1, 1, 'CONTAINER',
       '{"properties":{"containerCode":{"enum":["20DR","20FR","20FT","20GP","20HC","20HD","20NOR","20OT","20PF","20PL","20RE","20RF","40DR","40footGP","40GP","40HC","40NOR","40OT","40PL","40RE","40REHC","40RF","42GP","45HC"],"required":true},"containerCount":{"required":true}}}',
       'ON_CREATE')
ON CONFLICT (entity, lifecycle_hook, tenant_id)
DO UPDATE SET schema_validator = EXCLUDED.schema_validator;