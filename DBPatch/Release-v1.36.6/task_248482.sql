-- Query 1: Update for FRC00045617 / FRO00047693
-- Shipment ID : (93484, 93489)
-- Update Parties

update
    parties
set
    org_code = 'FRC00045617',
    address_code = 'FRO00047693',
    org_id = '162757',
    address_id = '277199',
    updated_at = NOW(),
    org_data = '{
                    "Id": 162757,
                    "City": "Shau Kei Wan",
                    "Guid": "76450752-aa76-406e-be5f-b86fdf07ec35",
                    "Email": "",
                    "label": "HONG KONG RELASTEC LTD",
                    "value": "FRC00045617",
                    "Country": "HKG",
                    "Address1": "15/F Eastern Central Plaza, 83 Nam On Street, 3 Yiu Hing Road, Hong Kong Island",
                    "Address2": "",
                    "FullName": "HONG KONG RELASTEC LTD",
                    "Payables": false,
                    "CompanyId": 372,
                    "Receivables": false,
                    "ForworderAgent": false,
                    "OrganizationCode": "FRC00045617",
                    "InsertUserIdUsername": "p100serviceaccountegypt@dpworld.com",
                    "UpdateUserIdUsername": "p100serviceaccountegypt@dpworld.com"
                }',
    address_data = '{
                        "Id": 277199,
                        "City": "Shau Kei Wan",
                        "Guid": "30c975a2-b06d-4285-9588-4919fe69a93f",
                        "Email": "",
                        "Country": "HKG",
                        "OrgGuid": "76450752-aa76-406e-be5f-b86fdf07ec35",
                        "Address1": "15/F Eastern Central Plaza, 83 Nam On Street, 3 Yiu Hing Road, Hong Kong Island",
                        "Address2": "",
                        "OrgSource": "CRP",
                        "AddressType": 2,
                        "CompanyName": "HONG KONG RELASTEC LTD",
                        "OrgFullName": "HONG KONG RELASTEC LTD",
                        "OrgPayables": false,
                        "KnownConsignor": false,
                        "OrgReceivables": false,
                        "RegulatedAgent": false,
                        "OrgActiveClient": true,
                        "AddressShortCode": "FRO00047693",
                        "OrgOrganizationCode": "FRC00045617"
                    }'
where
    id in (1481304, 1481189) and tenant_id = 474;

-- Update Shipments
UPDATE shipment_details
SET
    client_dps_address_id = 106714,
    updated_at = NOW()
WHERE
    id IN (93484, 93489)
    AND tenant_id = 474;



