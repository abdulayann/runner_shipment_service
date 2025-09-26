-- Shipment ID : (93937)

-- Update Parties
update
    parties
set
    org_code = 'FRC00030620',
    address_code = 'FRO00031628',
    org_id = '147121',
    address_id = '238867',
    updated_at = NOW(),
    org_data = '{
                    "Id": 147121,
                    "City": "Wurzburg",
                    "Guid": "5d22cfe1-0211-441b-9d27-41ea35e6df5a",
                    "Email": "import@xxxlutz.at",
                    "IsGSA": false,
                    "Phone": "1199999999",
                    "label": "BDSK Handels GmbH & Co KG",
                    "value": "FRC00030620",
                    "Broker": false,
                    "Source": "CRP",
                    "Carrier": false,
                    "Country": "DEU",
                    "Address1": "Mergentheimerstrabe 59",
                    "Address2": "",
                    "FullName": "BDSK Handels GmbH & Co KG",
                    "IsActive": 1,
                    "Payables": false,
                    "Services": false,
                    "TenantId": 442,
                    "CompanyId": 372,
                    "Consignee": true,
                    "Consigner": true,
                    "IsParnter": false,
                    "PANNumber": "FRC00030620",
                    "TaxVendor": false,
                    "WareHouse": false,
                    "AirCarrier": false,
                    "ExtraParam": "",
                    "InsertDate": "2025-03-28T19:48:21.000",
                    "SeaCarrier": false,
                    "UpdateDate": "2025-03-28T19:48:21.000",
                    "IsSuspended": false,
                    "RailCarrier": false,
                    "Receivables": false,
                    "RoadCarrier": false,
                    "ZipPostCode": "97084",
                    "ActiveClient": true,
                    "CurrencyCode": "EUR",
                    "EmailInvoice": false,
                    "InsertUserId": 1712,
                    "UpdateUserId": 1712,
                    "ForworderAgent": false,
                    "IsCreditEnabled": false,
                    "IsV2PaymentTerm": false,
                    "TransportClient": false,
                    "IsWalkInCustomer": false,
                    "OrganizationCode": "FRC00030620",
                    "EnableBulkInvoice": false,
                    "InsertUserIdUsername": "p100serviceaccountegypt@dpworld.com",
                    "UpdateUserIdUsername": "p100serviceaccountegypt@dpworld.com",
                    "IsClientEInvoiceEnabled": false,
                    "ImportExportClearanceLock": false,
                    "FusionCreditlimitOverrideApproved": false
                }',
    address_data = '{
                        "Id": 238867,
                        "City": "Wurzburg",
                        "Email": "import@xxxlutz.at",
                        "label": "FRO00031628",
                        "value": "FRO00031628",
                        "Country": "DEU",
                        "OrgGuid": "5d22cfe1-0211-441b-9d27-41ea35e6df5a",
                        "Address1": "Mergentheimerstrabe 59",
                        "Address2": "",
                        "OrgSource": "CRP",
                        "AddressType": 2,
                        "CompanyName": "BDSK Handels GmbH & Co KG",
                        "OrgFullName": "BDSK Handels GmbH & Co KG",
                        "OrgPayables": false,
                        "ZipPostCode": "97084",
                        "OrgReceivables": false,
                        "OrgActiveClient": true,
                        "AddressShortCode": "FRO00031628",
                        "OrgOrganizationCode": "FRC00030620"
                    }'
where
    id = 1488929 and tenant_id = 706;

-- Update Shipments
UPDATE shipment_details
SET
    client_dps_address_id = 106656,
    updated_at = NOW()
WHERE
    id = 93937
    AND tenant_id = 706;


-- Shipment ID : (80953)

-- Update Parties
update
    parties
set
    org_code = 'FRC00047516',
    address_code = 'FRO00049993',
    org_id = '164482',
    address_id = '282876',
    updated_at = NOW(),
    org_data = '{
                    "Id": 164482,
                    "City": "Jeddah",
                    "Guid": "30174fa3-36f3-4b97-9a44-b4a250cef148",
                    "Email": "operations@apl.com.sa",
                    "State": "02",
                    "label": "Establishment Advanced Power Logistics Services Est",
                    "value": "FRC00047516",
                    "Country": "SAU",
                    "Address1": "Building no. 8150 Al Madinah Al Munawwarah Road Ar Ruwais Dist",
                    "Address2": "",
                    "FullName": "Establishment Advanced Power Logistics Services Est",
                    "Payables": false,
                    "CompanyId": 372,
                    "Receivables": true,
                    "ZipPostCode": "23215",
                    "ForworderAgent": false,
                    "OrganizationCode": "FRC00047516",
                    "InsertUserIdUsername": "p100serviceaccountegypt@dpworld.com",
                    "UpdateUserIdUsername": "p100serviceaccountegypt@dpworld.com"
                }',
    address_data = '{
                        "Id": 282876,
                        "City": "Jeddah",
                        "Guid": "da67cc77-bd58-4a27-854d-2402a63d19df",
                        "Email": "operations@apl.com.sa",
                        "State": "02",
                        "Country": "SAU",
                        "OrgGuid": "30174fa3-36f3-4b97-9a44-b4a250cef148",
                        "Address1": "Building no. 8150 Al Madinah Al Munawwarah Road Ar Ruwais Dist",
                        "Address2": "",
                        "OrgSource": "CRP",
                        "AddressType": 2,
                        "CompanyName": "Establishment Advanced Power Logistics Services Est",
                        "OrgFullName": "Establishment Advanced Power Logistics Services Est",
                        "OrgPayables": false,
                        "ZipPostCode": "23215",
                        "TaxRegNumber": "301209562800003",
                        "KnownConsignor": false,
                        "OrgReceivables": true,
                        "RegulatedAgent": false,
                        "OrgActiveClient": true,
                        "AddressShortCode": "FRO00049993",
                        "OrgOrganizationCode": "FRC00047516"
                    }'
where
    id = 1516484 and tenant_id = 709;

-- Update Shipments
UPDATE shipment_details
SET
    client_dps_address_id = 112467,
    updated_at = NOW()
WHERE
    id = 95492
    AND tenant_id = 709;