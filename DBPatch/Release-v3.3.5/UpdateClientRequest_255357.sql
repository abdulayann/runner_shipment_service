-- Shipment ID : (105032)
-- Update Parties
update parties
    set
        org_code = 'FRC00047352',
        address_code = 'FRO00049824',
        org_id = '164310',
        address_id = '282235',
        updated_at = NOW(),
        org_data = '{
                "Id": 164310,
                "City": "JINHUA",
                "Guid": "3c586396-4beb-4596-b2a7-9004eaf09df0",
                "Email": "nily@conbapharm.com",
                "State": "ZJ",
                "label": "  ZHEJIANG JINHUA CONBA BIO-PHARM. CO., LTD.",
                "value": "FRC00047352",
                "Country": "CHN",
                "Address1": "288 Jinqu Road",
                "Address2": "",
                "FullName": "  ZHEJIANG JINHUA CONBA BIO-PHARM. CO., LTD.",
                "Payables": false,
                "CompanyId": 372,
                "Receivables": true,
                "ZipPostCode": "321016",
                "ForworderAgent": false,
                "OrganizationCode": "FRC00047352",
                "InsertUserIdUsername": "p100serviceaccountegypt@dpworld.com",
                "UpdateUserIdUsername": "p100serviceaccountegypt@dpworld.com"
            }',
    address_data = '{
                "Id": 282235,
                "City": "JINHUA",
                "Guid": "34cfa67e-0208-46bf-bbe1-a80bfb4d95b6",
                "Email": "nily@conbapharm.com",
                "State": "ZJ",
                "Country": "CHN",
                "OrgGuid": "3c586396-4beb-4596-b2a7-9004eaf09df0",
                "Address1": "288 Jinqu Road",
                "Address2": "",
                "OrgSource": "CRP",
                "AddressType": 2,
                "CompanyName": "  ZHEJIANG JINHUA CONBA BIO-PHARM. CO., LTD.",
                "OrgFullName": "  ZHEJIANG JINHUA CONBA BIO-PHARM. CO., LTD.",
                "OrgPayables": false,
                "ZipPostCode": "321016",
                "TaxRegNumber": "913307011472897859",
                "KnownConsignor": false,
                "OrgReceivables": true,
                "RegulatedAgent": false,
                "OrgActiveClient": true,
                "AddressShortCode": "FRO00049824",
                "OrgOrganizationCode": "FRC00047352"
            }'
    where
        id = 1682592 and tenant_id = 579;
-- Update Shipments
UPDATE shipment_details
    SET
        client_dps_address_id = 115140,
        updated_at = NOW()
    WHERE
        id = 105032
        AND tenant_id = 579;