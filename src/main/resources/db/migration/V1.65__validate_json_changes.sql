Update validations set schema_validator = '{"properties":{"client":{"type":"object","properties":{"orgData":{"required":true},"addressData":{"required":true}}},"direction":{"required":true},"shipmentType":{"required":true},"transportMode":{"required":true},"carrierDetails":{"type":"object","properties":{"eta":{"type":"date-time","compare":[{"operator":"greater-than","compareTo":"carrierDetails.etd"}]},"originPort":{"required":true},"destinationPort":{"required":true}}}}}' where entity = 'SHIPMENT';

Update validations set schema_validator = '{"properties":{"shipmentType":{"required":true},"transportMode":{"required":true},"carrierDetails":{"type":"object","properties":{"origin":{"required":true},"originPort":{"required":true},"destination":{"required":true},"destinationPort":{"required":true}}},"consolidationType":{"required":true}}}' where entity = 'CONSOLIDATION';

ALTER TABLE IF EXISTS containers
    ADD COLUMN IF NOT EXISTS handling_info varchar(2500);

ALTER TABLE IF EXISTS consolidation_details
    ADD COLUMN IF NOT EXISTS carrier_booking_ref varchar(64),
    DROP COLUMN IF EXISTS first_load,
    DROP COLUMN IF EXISTS last_discharge;