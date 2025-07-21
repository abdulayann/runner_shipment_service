UPDATE shipment_details
SET master_bill = '369-80701305',
    updated_at = NOW()
WHERE id = 76519
  AND shipment_id = 'SSZA25072242'
  AND tenant_id = 654;

UPDATE consolidation_details
SET mawb = '369-80701305',
    updated_at = NOW()
WHERE id = 57948
  AND tenant_id = 654;