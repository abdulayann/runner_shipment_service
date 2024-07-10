update
	shipment_details
set
	incoterms = 'DAP',
	updated_at = current_timestamp
where
    shipment_id in ('HOUR24010001 ','HOUR24010003 ','HOUR24010003 ','HOUR24010005 ','HOUR24010006 ','hour24010008 ',
    'HOUR24010010 ','HOUR24010012 ','HOUR24010014 ','HOUR24010016 ','HOUR24010018 ','HOUR24010020 ','HOUR24010022 ',
    'HOUR24010024 ','HOUR24010026 ','HOUR24010028 ','Hour24010030 ','HOUR24010410 ','SHP00029108')
    and tenant_id = 470;

update shipment_details set client_dps_address_id =
15891
where shipment_id in (
'AMSS24060173'
,
'AMSS24060166'
,
'AMSA24060142'
) and tenant_id = 484;

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
