UPDATE public.shipment_details s
SET fmc_tlc_id = 'FMC000'
FROM carrier_details c
WHERE s.carrier_detail_id = c.id
  AND s.fmc_tlc_id IS NULL
  AND s.transport_mode = 'SEA'
  AND (c.origin_port_loc_code LIKE 'US%' OR c.destination_port_loc_code LIKE 'US%');