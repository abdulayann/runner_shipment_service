-- Query 1: Update for FRC00027912 / FRO00028809 (Modern Supply Trading Company)
-- Shipment: ANRS25070759
update
    parties
set
    org_code = 'FRC00027912',
    address_code = 'FRO00028809',
    org_id = '155396',
    address_id = '258853',
    updated_at = NOW(),
    org_data = '{
        "Id": 155396,
        "City": "Jeddah",
        "Guid": "407641f9-0fba-400a-a68c-c9d1a778e0f9",
        "Email": "usaid@amscosal.com",
        "label": "Modern Supply Trading Company",
        "value": "FRC00027912",
        "Country": "SAU",
        "Address1": "Al shehri Street , Andalusia Dist , Ibrahim Al-Juffali, Makkah",
        "FullName": "Modern Supply Trading Company",
        "Payables": false,
        "CompanyId": 372,
        "Receivables": true,
        "ZipPostCode": "37748 23326",
        "ForworderAgent": false,
        "OrganizationCode": "FRC00027912",
        "InsertUserIdUsername": "p100serviceaccountegypt@dpworld.com",
        "UpdateUserIdUsername": "p100serviceaccountegypt@dpworld.com"
    }',
    address_data = '{
        "Id": 258853,
        "City": "Jeddah",
        "Guid": "f8993dbb-0031-4e8b-be04-3049b92add73",
        "Email": "usaid@amscosal.com",
        "Country": "SAU",
        "OrgGuid": "407641f9-0fba-400a-a68c-c9d1a778e0f9",
        "Address1": "Al shehri Street , Andalusia Dist , Ibrahim Al-Juffali, Makkah",
        "OrgSource": "CRP",
        "AddressType": 1,
        "CompanyName": "Modern Supply Trading Company",
        "OrgFullName": "Modern Supply Trading Company",
        "OrgPayables": false,
        "ZipPostCode": "37748 23326",
        "TaxRegNumber": "310415735200003",
        "KnownConsignor": false,
        "OrgReceivables": true,
        "RegulatedAgent": false,
        "SiteIdentifier": "1917750_001_FF-SA_001_B",
        "OrgActiveClient": true,
        "AddressShortCode": "FRO00028809",
        "OrgOrganizationCode": "FRC00027912"
    }'
where
    id = 83586 and tenant_id = 482;

update
    shipment_details
set
    client_dps_address_id = 103631,
    updated_at = NOW()
where
    id = 83586 and tenant_id = 482;

-- Query 2: Update for FRC00008918 / FRO00009576 (ZAMIL GROUP TRADE & SERVICES CO. LTD.)
-- Shipment: ANRS25070741
update
    parties
set
    org_code = 'FRC00008918',
    address_code = 'FRO00009576',
    org_id = '122187',
    address_id = '153587',
    updated_at = NOW(),
    org_data = '{
        "Id": 122187,
        "City": "DAMMAM",
        "Guid": "af5b1b24-ba79-4900-856a-e4f152be50f2",
        "Email": "Sayed.Zabiullah@zamilts.com",
        "State": "Eastern Province",
        "label": "ZAMIL GROUP TRADE & SERVICES CO. LTD.",
        "value": "FRC00008918",
        "Country": "SAU",
        "Address1": "DAMMAM 1ST INDUSTRIAL AREA PO BOX 13793",
        "FullName": "ZAMIL GROUP TRADE & SERVICES CO. LTD.",
        "Payables": false,
        "CompanyId": 372,
        "Receivables": false,
        "ZipPostCode": "31414",
        "ForworderAgent": false,
        "OrganizationCode": "FRC00008918",
        "InsertUserIdUsername": "p100serviceaccountegypt@dpworld.com"
    }',
    address_data = '{
        "Id": 153587,
        "City": "DAMMAM",
        "Guid": "3ebd32d2-11fc-436a-b853-c66efb74b102",
        "Email": "Sayed.Zabiullah@zamilts.com",
        "State": "Eastern Province",
        "Country": "SAU",
        "Address1": "DAMMAM 1ST INDUSTRIAL AREA PO BOX 13793",
        "OrgSource": "CRP",
        "AddressType": 2,
        "CompanyName": "ZAMIL GROUP TRADE & SERVICES CO. LTD.",
        "OrgFullName": "ZAMIL GROUP TRADE & SERVICES CO. LTD.",
        "OrgPayables": false,
        "ZipPostCode": "31414",
        "OrgReceivables": false,
        "OrgActiveClient": true,
        "AddressShortCode": "FRO00009576",
        "OrgOrganizationCode": "FRC00008918"
    }'
where
    id = 81642 and tenant_id = 482;

update
    shipment_details
set
    client_dps_address_id = 103611,
    updated_at = NOW()
where
    id = 81642 and tenant_id = 482;