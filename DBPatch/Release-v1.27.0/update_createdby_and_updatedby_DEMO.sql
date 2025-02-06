UPDATE events
SET
    created_by = COALESCE(created_by, 'serviceaccountdemo@dpworld.com'),
    updated_by = COALESCE(updated_by, 'serviceaccountdemo@dpworld.com')
WHERE event_type = 'INVOICE';