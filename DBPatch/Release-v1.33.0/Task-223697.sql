UPDATE shipment_details
SET department = CASE
    WHEN transport_mode = 'AIR' AND shipment_type = 'CTS' THEN 'ACT'
    WHEN transport_mode = 'AIR' AND shipment_type = 'DOM' THEN 'AE'
    WHEN transport_mode = 'AIR' AND shipment_type = 'EXP' THEN 'AE'
    WHEN transport_mode = 'AIR' AND shipment_type = 'IMP' THEN 'AI'
    WHEN transport_mode = 'ROA' AND shipment_type = 'CTS' THEN 'RE'
    WHEN transport_mode = 'ROA' AND shipment_type = 'DOM' THEN 'RD'
    WHEN transport_mode = 'ROA' AND shipment_type = 'EXP' THEN 'RE'
    WHEN transport_mode = 'ROA' AND shipment_type = 'IMP' THEN 'RI'
    WHEN transport_mode = 'SEA' AND shipment_type = 'CTS' THEN 'OCT'
    WHEN transport_mode = 'SEA' AND shipment_type = 'DOM' THEN 'OE'
    WHEN transport_mode = 'SEA' AND shipment_type = 'EXP' THEN 'OE'
    WHEN transport_mode = 'SEA' AND shipment_type = 'IMP' THEN 'OI'
END
where department is null;


UPDATE consolidation_details
SET department = CASE
    WHEN transport_mode = 'AIR' AND shipment_type  = 'CTS' THEN 'ACT'
    WHEN transport_mode = 'AIR' AND shipment_type = 'DOM' THEN 'AE'
    WHEN transport_mode = 'AIR' AND shipment_type = 'EXP' THEN 'AE'
    WHEN transport_mode = 'AIR' AND shipment_type = 'IMP' THEN 'AI'
    WHEN transport_mode = 'ROA' AND shipment_type = 'CTS' THEN 'RE'
    WHEN transport_mode = 'ROA' AND shipment_type = 'DOM' THEN 'RD'
    WHEN transport_mode = 'ROA' AND shipment_type = 'EXP' THEN 'RE'
    WHEN transport_mode = 'ROA' AND shipment_type = 'IMP' THEN 'RI'
    WHEN transport_mode = 'SEA' AND shipment_type = 'CTS' THEN 'OCT'
    WHEN transport_mode = 'SEA' AND shipment_type = 'DOM' THEN 'OE'
    WHEN transport_mode = 'SEA' AND shipment_type = 'EXP' THEN 'OE'
    WHEN transport_mode = 'SEA' AND shipment_type = 'IMP' THEN 'OI'
END
where department is null;
