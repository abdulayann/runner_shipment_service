
-- Update Parties
update
    parties
set
    org_code = 'FRC00047352',
    address_code = 'FRO00049824',
    org_id = '164310',
    address_id = '282235',
    updated_at = NOW(),
    address_data = '{
            "Id": 282235,
            "Guid": "34cfa67e-0208-46bf-bbe1-a80bfb4d95b6",
            "AddressShortCode": "FRO00049824",
            "CompanyName": "  ZHEJIANG JINHUA CONBA BIO-PHARM. CO., LTD.",
            "AddressType": 1,
            "SiteIdentifier": "2065419_001_CNSGH_001_B",
            "Address1": "288 Jinqu Road",
            "Address2": "",
            "Country": "CHN",
            "City": "JINHUA",
            "State": "ZJ",
            "ZipPostCode": "321016",
            "Email": "nily@conbapharm.com",
            "OrgGuid": "3c586396-4beb-4596-b2a7-9004eaf09df0",
            "OrgOrganizationCode": "FRC00047352",
            "OrgSource": "CRP",
            "OrgFullName": "  ZHEJIANG JINHUA CONBA BIO-PHARM. CO., LTD.",
            "OrgActiveClient": true,
            "OrgReceivables": true,
            "OrgPayables": false,
            "TaxRegNumber": "913307011472897859",
            "RegulatedAgent": false,
            "KnownConsignor": false
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