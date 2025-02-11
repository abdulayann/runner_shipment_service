UPDATE events
SET
    created_by = COALESCE(created_by, 'serviceAccountEgypt'),
    updated_by = COALESCE(updated_by, 'serviceAccountEgypt')
WHERE event_type = 'INVOICE';