UPDATE awb
SET awb_shipment_info = jsonb_set(awb_shipment_info, '{consigneeName}', '"CARGO SERVICES AIRFREIGHT (SHANGHAI) LTD. (CSL)"', true)
WHERE consolidation_id = 63901;


UPDATE awb
SET awb_shipment_info = jsonb_set(awb_shipment_info, '{consigneeAddress}', '"2735,FujinRoad,Baoshan District,Shanghai, China 201901"', true)
WHERE consolidation_id = 63901;


UPDATE awb
SET awb_shipment_info = jsonb_set(awb_shipment_info, '{consigneeTaxRegistrationNumber}', '"91310115717856770B"', true)
WHERE consolidation_id = 63901;


UPDATE awb
SET awb_shipment_info = jsonb_set(awb_shipment_info, '{consigneeContactName}', '"Ivy"', true)
WHERE consolidation_id = 63901;


UPDATE awb
SET awb_shipment_info = jsonb_set(awb_shipment_info, '{consigneePhone}', '"00862136632709"', true)
WHERE consolidation_id = 63901;