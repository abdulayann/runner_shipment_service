UPDATE shipment_details
SET job_status = 'WRK',
    updated_at = now() at time zone 'utc'
where shipment_id = 'MILS24080004' and tenant_id = 612;