UPDATE awb 
SET air_message_status = 'AWB_GENERATED' 
WHERE shipment_id = 66474;

UPDATE awb 
SET 
    air_message_status = 'AWB_GENERATED',
    linked_hawb_air_message_status = 'AWB_GENERATED'
WHERE consolidation_id = 50230;