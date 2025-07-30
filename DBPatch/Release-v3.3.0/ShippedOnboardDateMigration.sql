UPDATE shipment_additional_details
SET shipped_onboard = carrier_details.atd
FROM shipment_details
JOIN carrier_details ON shipment_details.carrier_detail_id = carrier_details.id
WHERE shipment_details.additional_details_id = shipment_additional_details.id
AND carrier_details.atd IS NOT NULL