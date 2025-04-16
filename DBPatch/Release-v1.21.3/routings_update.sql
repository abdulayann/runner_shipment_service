UPDATE routings r
SET is_deleted = true,
    updated_at = '2024-11-06 00:00:00'
FROM shipment_details s
WHERE r.shipment_id = s.id
  AND r.carriage IS NULL
  AND (r.mode <> s.transport_mode
   OR (r.mode IS NULL AND s.transport_mode IS NOT NULL)
   OR (r.mode IS NOT NULL AND s.transport_mode IS NULL));

UPDATE routings
SET carriage = 1
WHERE is_deleted = false and shipment_id is not null;