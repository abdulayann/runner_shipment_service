UPDATE shipment_details
SET client_dps_address_id = 15891
WHERE shipment_id IN ('AMSS24060173', 'AMSS24060166', 'AMSA24060142')
  AND tenant_id = 484;

UPDATE parties
SET
    org_code = 'FRC00003938',
    org_data = '{
        "Id": 60841,
        "City": "Macclesfield",
        "Email": "Bieyadien.adams@skao.int",
        "Phone": "07468862789",
        "State": "Cheshire",
        "Source": "CRP",
        "Country": "GBR",
        "Address1": "Jordel Bank",
        "FullName": "Skao  global hq",
        "Payables": false,
        "TenantId": 442,
        "Receivables": true,
        "ZipPostCode": "SK119FT",
        "ActiveClient": true,
        "ForworderAgent": false,
        "OrganizationCode": "FRC00003938"
    }',
    address_code = 'FRO00002976',
    address_data = '{
        "Id": 85134,
        "City": "Macclesfield",
        "Guid": "c401d703-01a7-454c-b019-9e59a2a82bdf",
        "Email": "Bieyadien.adams@skao.int",
        "State": "Cheshire",
        "label": "Skao  global hq\nJordel Bank, Macclesfield, Cheshire\nGBR, SK119FT\n",
        "value": "FRO00002976",
        "Country": "GBR",
        "Address1": "Jordel Bank",
        "OrgSource": "CRP",
        "AddressType": 2,
        "CompanyName": "Skao  global hq",
        "OrgFullName": "Skao  global hq",
        "OrgPayables": false,
        "ZipPostCode": "SK119FT",
        "TaxRegNumber": "GB029862490000",
        "OrgReceivables": true,
        "OrgActiveClient": true,
        "AddressShortCode": "FRO00002976",
        "OrgOrganizationCode": "FRC00003938"
    }'
WHERE
    id IN (
        SELECT client_id
        FROM shipment_details
        WHERE shipment_id IN ('AMSS24060173', 'AMSS24060166', 'AMSA24060142')
          AND tenant_id = 484
    );