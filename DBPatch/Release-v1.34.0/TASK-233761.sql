UPDATE shipment_details
SET master_bill = '020-06182256',
    updated_at = NOW()
WHERE id = 70773
  AND shipment_id = 'BSLA25060312'
  AND tenant_id = 685;


UPDATE consolidation_details
SET mawb = '020-06182256',
    updated_at = NOW()
WHERE id = 53418
  AND consolidation_number = 'BSLA25060316'
  AND tenant_id = 685;
