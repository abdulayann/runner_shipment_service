UPDATE events
SET
    created_by = COALESCE(created_by, 'serviceaccountP100'),
    updated_by = COALESCE(updated_by, 'serviceaccountP100')
WHERE event_type = 'INVOICE';