UPDATE carrier_details cd
SET vessel = '99676089-14ff-4919-8523-d2847abb3e0d'
FROM shipment_details sd
WHERE cd.id = sd.carrier_detail_id
AND sd.shipment_id = 'MIAS24040644';
