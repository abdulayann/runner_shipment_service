-- Query 1: Update for FRC00041902 / FRO00043760
-- Shipment ID : (98996)
-- Update Parties

update
    parties
set
    org_code = 'FRC00041902',
    address_code = 'FRO00043760',
    org_id = '159036',
    address_id = '268162',
    updated_at = NOW(),
    org_data = '{
                    "Id": 159036,
                    "City": "Bangalore",
                    "Guid": "70ffb1ed-9f48-47b0-bfa1-b56fbf342edc",
                    "Email": "operations@vipraindia.com",
                    "State": "KA",
                    "label": "VIPRA INDUSTRIAL PROJECTS LLP",
                    "value": "FRC00041902",
                    "Country": "IND",
                    "Address1": "34, Lotus Towers, Devraj Urs Road Race Course High Grounds",
                    "Address2": "",
                    "FullName": "VIPRA INDUSTRIAL PROJECTS LLP",
                    "Payables": false,
                    "CompanyId": 372,
                    "Receivables": true,
                    "ZipPostCode": "560001",
                    "ForworderAgent": false,
                    "OrganizationCode": "FRC00041902",
                    "InsertUserIdUsername": "p100serviceaccountegypt@dpworld.com",
                    "UpdateUserIdUsername": "p100serviceaccountegypt@dpworld.com"
                }',
    address_data = '{
                        "Id": 268162,
                        "City": "Bangalore",
                        "Guid": "64935833-fa52-4810-9ada-6975ef6372e7",
                        "Email": "operations@vipraindia.com",
                        "State": "KA",
                        "Country": "IND",
                        "OrgGuid": "70ffb1ed-9f48-47b0-bfa1-b56fbf342edc",
                        "Address1": "34, Lotus Towers, Devraj Urs Road Race Course High Grounds",
                        "Address2": "",
                        "OrgSource": "CRP",
                        "AddressType": 1,
                        "CompanyName": "VIPRA INDUSTRIAL PROJECTS LLP",
                        "OrgFullName": "VIPRA INDUSTRIAL PROJECTS LLP",
                        "OrgPayables": false,
                        "ZipPostCode": "560001",
                        "TaxRegNumber": "29AABFV1557E1Z6",
                        "KnownConsignor": false,
                        "OrgReceivables": true,
                        "RegulatedAgent": false,
                        "SiteIdentifier": "1964095_001_FF-HIPL_001_B",
                        "OrgActiveClient": true,
                        "AddressShortCode": "FRO00043760",
                        "OrgOrganizationCode": "FRC00041902"
                    }'
where
    id = 1576833 and tenant_id = 536;

-- Update Shipments
UPDATE shipment_details
SET
    client_dps_address_id = 110652,
    updated_at = NOW()
WHERE
    id = 98996
    AND tenant_id = 536;



