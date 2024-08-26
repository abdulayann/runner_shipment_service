UPDATE shipment_details SET job_status = 'INV', updated_at = now() at time zone 'utc' where shipment_id = 'RTMS24070396' and tenant_id = 468;

UPDATE shipment_details SET job_status = 'WRK', updated_at = now() at time zone 'utc' where shipment_id = 'MILS24080004' and tenant_id = 612;