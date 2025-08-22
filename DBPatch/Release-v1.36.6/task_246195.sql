-- Query 1: Update for FRC00009712 / FRO00010383 (IMPERIAL CLEARING AND FORWARDING SOUTH AFRICA)
-- Shipment: GVAA25070210
update
    parties
set
    org_code = 'FRC00009712',
    address_code = 'FRO00010383',
    org_id = '122975',
    address_id = '155286',
    updated_at = NOW(),
    org_data = '{
    "Id": 122975,
    "City": "BOKSBURG",
    "Guid": "d6bb71e7-af01-4083-b01c-b528929f3a99",
    "Email": "bianca.dejager@dpworld.com",
    "State": "Gauteng",
    "label": "IMPERIAL CLEARING AND FORWARDING SOUTH AFRICA (PTY) LTD",
    "value": "FRC00009712",
    "Unloco": 1056633,
    "Country": "ZAF",
    "Address1": "BUILDING K, CLEARWATER BUSINESS PARK, CNR PARK AND ATLAS ROAD, BOKSBURG",
    "FullName": "IMPERIAL CLEARING AND FORWARDING SOUTH AFRICA (PTY) LTD",
    "Payables": false,
    "CompanyId": 372,
    "UnlocoCode": "ZAAET",
    "Receivables": false,
    "ZipPostCode": "1459",
    "ForworderAgent": false,
    "OrganizationCode": "FRC00009712",
    "PaymentTermsCode": "I30",
    "InsertUserIdUsername": "p100serviceaccountegypt@dpworld.com",
    "UpdateUserIdUsername": "p100serviceaccountegypt@dpworld.com"
}',
    address_data = '{
    "Id": 155286,
    "City": "BOKSBURG",
    "Guid": "59705029-975e-442d-9678-ff6991ed1e95",
    "Email": "bianca.dejager@dpworld.com",
    "State": "Gauteng",
    "label": "IMPERIAL CLEARING AND FORWARDING SOUTH AFRICA (PTY) LTD\\nBUILDING K, CLEARWATER BUSINESS PARK, CNR PARK AND ATLAS ROAD, BOKSBURG, BOKSBURG, Gauteng\\nZAF, 1459\\n",
    "value": "FRO00010383",
    "Country": "ZAF",
    "OrgGuid": "d6bb71e7-af01-4083-b01c-b528929f3a99",
    "Address1": "BUILDING K, CLEARWATER BUSINESS PARK, CNR PARK AND ATLAS ROAD, BOKSBURG",
    "OrgSource": "CRP",
    "AddressType": 2,
    "CompanyName": "IMPERIAL CLEARING AND FORWARDING SOUTH AFRICA (PTY) LTD",
    "OrgFullName": "IMPERIAL CLEARING AND FORWARDING SOUTH AFRICA (PTY) LTD",
    "OrgPayables": false,
    "ZipPostCode": "1459",
    "TaxRegNumber": "4320103023",
    "OrgReceivables": false,
    "OrgActiveClient": true,
    "AddressShortCode": "FRO00010383",
    "OrgOrganizationCode": "FRC00009712"
}'
where
    id = (select client_id from shipment_details where id=82616);

update
    shipment_details
set
    client_dps_address_id = 101037,
    updated_at = NOW()
where
    id=82616;

-- Query 2: Update for FRC00038277 / FRO00039943 (GREILSAMMER SA)
-- Shipment: PARA25071395
update
    parties
set
    org_code = 'FRC00038277',
    address_code = 'FRO00039943',
    org_id = '155732',
    address_id = '259510',
    updated_at = NOW(),
    org_data = '{
    "Id": 155732,
    "City": "CAROUGE",
    "Guid": "5f5934e2-e24d-40e0-bd44-8d2c71c601c9",
    "Email": "jjelk@greilsammer.com",
    "label": "GREILSAMMER SA",
    "value": "FRC00038277",
    "Country": "CHE",
    "Address1": "AVENUE CARDINAL-MERMILLOD 36",
    "Address2": "C/O FIDEXPERT SA",
    "FullName": "GREILSAMMER SA",
    "Payables": false,
    "CompanyId": 372,
    "Receivables": true,
    "ZipPostCode": "1227",
    "ForworderAgent": false,
    "OrganizationCode": "FRC00038277",
    "InsertUserIdUsername": "p100serviceaccountegypt@dpworld.com",
    "UpdateUserIdUsername": "p100serviceaccountegypt@dpworld.com"
}',
    address_data = '{
    "Id": 259510,
    "City": "CAROUGE",
    "Guid": "290ff989-e96f-45e3-a2d8-707d9e9f012f",
    "Email": "jjelk@greilsammer.com",
    "Country": "CHE",
    "OrgGuid": "5f5934e2-e24d-40e0-bd44-8d2c71c601c9",
    "Address1": "AVENUE CARDINAL-MERMILLOD 36",
    "Address2": "C/O FIDEXPERT SA",
    "OrgSource": "CRP",
    "AddressType": 2,
    "CompanyName": "GREILSAMMER SA",
    "OrgFullName": "GREILSAMMER SA",
    "OrgPayables": false,
    "ZipPostCode": "1227",
    "TaxRegNumber": "CHE468785209",
    "KnownConsignor": false,
    "OrgReceivables": true,
    "RegulatedAgent": false,
    "OrgActiveClient": true,
    "AddressShortCode": "FRO00039943",
    "OrgOrganizationCode": "FRC00038277"
}'
where
    id = (select client_id from shipment_details where id=72076);

update
    shipment_details
set
    client_dps_address_id = 103839,
    updated_at = NOW()
where
    id=72076;

-- Query 3: Update for FRC00038277 / FRO00039943 (GREILSAMMER SA)
-- Shipment: PARA25071394
update
    parties
set
    org_code = 'FRC00038277',
    address_code = 'FRO00039943',
    org_id = '155732',
    address_id = '259510',
    updated_at = NOW(),
    org_data = '{
    "Id": 155732,
    "City": "CAROUGE",
    "Guid": "5f5934e2-e24d-40e0-bd44-8d2c71c601c9",
    "Email": "jjelk@greilsammer.com",
    "label": "GREILSAMMER SA",
    "value": "FRC00038277",
    "Country": "CHE",
    "Address1": "AVENUE CARDINAL-MERMILLOD 36",
    "Address2": "C/O FIDEXPERT SA",
    "FullName": "GREILSAMMER SA",
    "Payables": false,
    "CompanyId": 372,
    "Receivables": true,
    "ZipPostCode": "1227",
    "ForworderAgent": false,
    "OrganizationCode": "FRC00038277",
    "InsertUserIdUsername": "p100serviceaccountegypt@dpworld.com",
    "UpdateUserIdUsername": "p100serviceaccountegypt@dpworld.com"
}',
    address_data = '{
    "Id": 259510,
    "City": "CAROUGE",
    "Guid": "290ff989-e96f-45e3-a2d8-707d9e9f012f",
    "Email": "jjelk@greilsammer.com",
    "Country": "CHE",
    "OrgGuid": "5f5934e2-e24d-40e0-bd44-8d2c71c601c9",
    "Address1": "AVENUE CARDINAL-MERMILLOD 36",
    "Address2": "C/O FIDEXPERT SA",
    "OrgSource": "CRP",
    "AddressType": 2,
    "CompanyName": "GREILSAMMER SA",
    "OrgFullName": "GREILSAMMER SA",
    "OrgPayables": false,
    "ZipPostCode": "1227",
    "TaxRegNumber": "CHE468785209",
    "KnownConsignor": false,
    "OrgReceivables": true,
    "RegulatedAgent": false,
    "OrgActiveClient": true,
    "AddressShortCode": "FRO00039943",
    "OrgOrganizationCode": "FRC00038277"
}'
where
    id = (select client_id from shipment_details where id=72073);

update
    shipment_details
set
    client_dps_address_id = 103839,
    updated_at = NOW()
where
    id=72073;