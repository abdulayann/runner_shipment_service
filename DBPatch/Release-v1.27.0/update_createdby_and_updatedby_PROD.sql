UPDATE events
SET
    created_by = COALESCE(created_by, 'servicceaccountvessel'),
    updated_by = COALESCE(updated_by, 'servicceaccountvessel')
WHERE event_type = 'INVOICE';