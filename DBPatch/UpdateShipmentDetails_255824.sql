-- Shipment ID : (99575)
-- Update Parties
update
    parties
set
    org_code = 'FRC00047497',
    address_code = 'FRO00049974',
    org_id = '164458',
    address_id = '282850',
    updated_at = NOW(),
    org_data = '{
    "Id": 164458,
    "City": "SURREY",
    "Guid": "9836ec3f-7a4c-4fe0-ac99-625966472114",
    "Email": "sales@badencanada.com",
    "State": "BC",
    "label": "BSC SPORT (CANADA) LTD",
    "value": "FRC00047497",
    "Country": "CAN",
    "Address1": "604-17665 66A AVENUE",
    "Address2": "",
    "FullName": "BSC SPORT (CANADA) LTD",
    "Payables": false,
    "CompanyId": 372,
    "Receivables": false,
    "ZipPostCode": "V3S 2A7",
    "ForworderAgent": false,
    "OrganizationCode": "FRC00047497",
    "InsertUserIdUsername": "p100serviceaccountegypt@dpworld.com",
    "UpdateUserIdUsername": "p100serviceaccountegypt@dpworld.com"
                }',
    address_data = '{
    "Id": 282850,
    "City": "SURREY",
    "Guid": "29a7fc30-dc9f-409b-bbf7-def182b57cbe",
    "Email": "sales@badencanada.com",
    "State": "BC",
    "Country": "CAN",
    "OrgGuid": "9836ec3f-7a4c-4fe0-ac99-625966472114",
    "Address1": "604-17665 66A AVENUE",
    "Address2": "",
    "OrgSource": "CRP",
    "AddressType": 2,
    "CompanyName": "BSC SPORT (CANADA) LTD",
    "OrgFullName": "BSC SPORT (CANADA) LTD",
    "OrgPayables": false,
    "ZipPostCode": "V3S 2A7",
    "KnownConsignor": false,
    "OrgReceivables": false,
    "RegulatedAgent": false,
    "OrgActiveClient": true,
    "AddressShortCode": "FRO00049974",
    "OrgOrganizationCode": "FRC00047497"
}'
where
    id = 1586874 and tenant_id = 706;
-- Update Shipments
UPDATE shipment_details
SET
    client_dps_address_id = 111752,
    updated_at = NOW()
WHERE
    id = 99575
    AND tenant_id = 706;