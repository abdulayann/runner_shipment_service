UPDATE validations
set schema_validator = '{"properties":{"packsType":{"required":true},"packs":{"required":true}}}'
where entity = 'PACKING'