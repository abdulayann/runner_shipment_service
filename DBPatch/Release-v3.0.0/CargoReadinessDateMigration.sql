UPDATE shipment_details
SET cargo_readiness_date = cargo_ready_date
WHERE cargo_ready_date IS NOT NULL
  AND cargo_readiness_date IS DISTINCT FROM cargo_ready_date;