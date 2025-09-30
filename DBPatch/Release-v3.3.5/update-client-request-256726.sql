-- Shipment ID : (99575)
-- Update Parties
update
    parties
set
    org_code = 'FRC00031322',
    address_code = 'FRO00032358',
    org_id = '153536',
    address_id = '255705',
    updated_at = NOW(),
    org_data = '{
                    "Id": 153536,
                    "City": "Oakville",
                    "Guid": "e17fc929-151b-439e-800e-b206c6f3ead4",
                    "Email": "lucia.lawrence@acibrands.com",
                    "IsGSA": false,
                    "Phone": "9058291566",
                    "State": "ON",
                    "label": "ACI Brands Inc.",
                    "value": "FRC00031322",
                    "Broker": false,
                    "Source": "CRP",
                    "Carrier": false,
                    "Country": "CAN",
                    "Address1": "2616 Sheridan Garden Dr.",
                    "FullName": "ACI Brands Inc.",
                    "IsActive": 1,
                    "Payables": false,
                    "Services": false,
                    "TenantId": 442,
                    "CompanyId": 372,
                    "Consignee": true,
                    "Consigner": true,
                    "IsParnter": false,
                    "PANNumber": "121381677",
                    "TaxVendor": false,
                    "WareHouse": false,
                    "AirCarrier": false,
                    "ExtraParam": "",
                    "InsertDate": "2025-06-03T13:22:56.000",
                    "SeaCarrier": false,
                    "UpdateDate": "2025-06-10T16:35:19.000",
                    "IsSuspended": false,
                    "RailCarrier": false,
                    "Receivables": true,
                    "RoadCarrier": false,
                    "ZipPostCode": "L6J 7Z2",
                    "ActiveClient": true,
                    "CurrencyCode": "CAD",
                    "EmailInvoice": false,
                    "InsertUserId": 1712,
                    "UpdateUserId": 1712,
                    "VatRegNumber": "121381677",
                    "ForworderAgent": false,
                    "IsCreditEnabled": false,
                    "IsV2PaymentTerm": false,
                    "TransportClient": false,
                    "IsWalkInCustomer": false,
                    "OrganizationCode": "FRC00031322",
                    "EnableBulkInvoice": false,
                    "CustomerIdentifier": "1900080_001",
                    "ReceivableTermsDate": 2,
                    "InsertUserIdUsername": "p100serviceaccountegypt@dpworld.com",
                    "UpdateUserIdUsername": "p100serviceaccountegypt@dpworld.com",
                    "IsClientEInvoiceEnabled": false,
                    "ImportExportClearanceLock": false,
                    "FusionCreditlimitOverrideApproved": false
                }',
    address_data = '{
                        "Id": 255705,
                        "City": "Oakville",
                        "Email": "lucia.lawrence@acibrands.com",
                        "State": "ON",
                        "label": "FRO00032358",
                        "value": "FRO00032358",
                        "Country": "CAN",
                        "OrgGuid": "e17fc929-151b-439e-800e-b206c6f3ead4",
                        "Address1": "2616 Sheridan Garden Dr.",
                        "OrgSource": "CRP",
                        "AddressType": 1,
                        "CompanyName": "ACI Brands Inc.",
                        "OrgFullName": "ACI Brands Inc.",
                        "OrgPayables": false,
                        "ZipPostCode": "L6J 7Z2",
                        "TaxRegNumber": "121381677",
                        "OrgReceivables": true,
                        "SiteIdentifier": "1900080_001_CAVAN_001_B",
                        "OrgActiveClient": true,
                        "AddressShortCode": "FRO00032358",
                        "OrgOrganizationCode": "FRC00031322"
                    }'
where
    id in (1586874,1695973) and tenant_id = 706;

-- Update Shipments
UPDATE shipment_details
SET
    client_dps_address_id = 116803,
    updated_at = NOW()
WHERE
    id = 99575
    AND tenant_id = 706;