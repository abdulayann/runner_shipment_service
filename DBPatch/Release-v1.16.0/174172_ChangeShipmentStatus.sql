UPDATE shipment_details
SET status = 0,
    updated_at = now() at time zone 'utc'
where shipment_id = 'PARA24060010' and tenant_id = 581;