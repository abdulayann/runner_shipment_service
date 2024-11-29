UPDATE carrier_details
SET 
    ATA = ETA
FROM
    shipment_details
WHERE
    shipment_details.carrier_detail_id = carrier_details.id
    AND shipment_details.status = 2
    AND carrier_details.ATA IS NULL
    AND carrier_details.ETA <= '2024-10-31 23:59:59';    


UPDATE carrier_details
SET 
    ATD = ETD
FROM
    shipment_details
WHERE
    shipment_details.carrier_detail_id = carrier_details.id    
    AND shipment_details.status = 2 
    AND carrier_details.ATD IS NULL
    AND carrier_details.ETD <= '2024-09-30 23:59:59';