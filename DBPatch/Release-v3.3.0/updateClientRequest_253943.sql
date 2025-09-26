-- Shipment ID : (95144)
-- Update Parties
update
    parties
set
    org_code = 'FRC00030219',
    address_code = 'FRO00031203',
    org_id = '146854',
    address_id = '238408',
    updated_at = NOW(),
    org_data = '{
                    "Id": 146854,
                    "City": "Jeddah",
                    "Guid": "9ccdf4f6-aed7-4d6c-95ff-e12c541f4c1f",
                    "Email": "jeddah@ALJAMEELFOOD.COM",
                    "IsGSA": false,
                    "Phone": "558106662",
                    "label": "Al Jameel International Co Ltd",
                    "value": "FRC00030219",
                    "Broker": false,
                    "Source": "CRP",
                    "Carrier": false,
                    "Country": "SAU",
                    "Address1": "Yousif Mohammed Yousif Al Thaqafi St",
                    "FullName": "Al Jameel International Co Ltd",
                    "IsActive": 1,
                    "Payables": false,
                    "Services": false,
                    "TenantId": 442,
                    "CompanyId": 372,
                    "Consignee": true,
                    "Consigner": true,
                    "IsParnter": false,
                    "PANNumber": "4030285280",
                    "TaxVendor": false,
                    "WareHouse": false,
                    "AirCarrier": false,
                    "ExtraParam": "",
                    "InsertDate": "2025-03-26T20:14:34.000",
                    "SeaCarrier": false,
                    "UpdateDate": "2025-07-14T19:42:11.000",
                    "IsSuspended": false,
                    "RailCarrier": false,
                    "Receivables": true,
                    "RoadCarrier": false,
                    "ZipPostCode": "22766",
                    "ActiveClient": true,
                    "CurrencyCode": "SAR",
                    "EmailInvoice": false,
                    "InsertUserId": 1712,
                    "UpdateUserId": 1712,
                    "VatRegNumber": "310153411900003",
                    "ForworderAgent": false,
                    "IsCreditEnabled": false,
                    "IsV2PaymentTerm": false,
                    "TransportClient": false,
                    "IsWalkInCustomer": false,
                    "OrganizationCode": "FRC00030219",
                    "EnableBulkInvoice": false,
                    "CustomerIdentifier": "1827373_001",
                    "ReceivableTermsDate": 2,
                    "InsertUserIdUsername": "p100serviceaccountegypt@dpworld.com",
                    "UpdateUserIdUsername": "p100serviceaccountegypt@dpworld.com",
                    "IsClientEInvoiceEnabled": false,
                    "ImportExportClearanceLock": false,
                    "FusionCreditlimitOverrideApproved": false
                }',
    address_data = '{
                        "Id": 238408,
                        "City": "Jeddah",
                        "Email": "jeddah@ALJAMEELFOOD.COM",
                        "label": "FRO00031203",
                        "value": "FRO00031203",
                        "Country": "SAU",
                        "OrgGuid": "9ccdf4f6-aed7-4d6c-95ff-e12c541f4c1f",
                        "Address1": "Yousif Mohammed Yousif Al Thaqafi St",
                        "OrgSource": "CRP",
                        "AddressType": 2,
                        "CompanyName": "Al Jameel International Co Ltd",
                        "OrgFullName": "Al Jameel International Co Ltd",
                        "OrgPayables": false,
                        "ZipPostCode": "22766",
                        "TaxRegNumber": "310153411900003",
                        "OrgReceivables": true,
                        "OrgActiveClient": true,
                        "AddressShortCode": "FRO00031203",
                        "OrgOrganizationCode": "FRC00030219"
                    }'
where
    id = 1510227 and tenant_id = 709;
-- Update Shipments
UPDATE shipment_details
SET
    client_dps_address_id = 107608,
    updated_at = NOW()
WHERE
    id = 95144
    AND tenant_id = 709;