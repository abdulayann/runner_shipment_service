-- Query 1: Update for FRC00039841 / FRO00041585
-- Shipment: JKTS25070709
-- Update Parties
update
    parties
set
    org_code = 'FRC00039841',
    address_code = 'FRO00041585',
    org_id = '157275',
    address_id = '262371',
    updated_at = NOW(),
    org_data = '{
                    "Id": 157275,
                    "City": "Dottingen",
                    "Guid": "52634fc2-8c10-4015-8e66-dfd8a62f2b3f",
                    "Email": "Nal@sara-transporte.ch",
                    "IsGSA": false,
                    "Phone": "0041562819884",
                    "State": "Aargau",
                    "label": "Sara Transporte AG",
                    "value": "FRC00039841",
                    "Broker": false,
                    "Source": "CRP",
                    "Carrier": false,
                    "Country": "CHE",
                    "Address1": "Vorhard 15",
                    "FullName": "Sara Transporte AG",
                    "IsActive": 1,
                    "Payables": false,
                    "Services": false,
                    "TenantId": 442,
                    "CompanyId": 372,
                    "Consignee": true,
                    "Consigner": true,
                    "IsParnter": false,
                    "PANNumber": "CHE-264.842.468",
                    "TaxVendor": false,
                    "WareHouse": false,
                    "AirCarrier": false,
                    "ExtraParam": "",
                    "InsertDate": "2025-07-06T21:11:39.000",
                    "SeaCarrier": false,
                    "UpdateDate": "2025-07-06T23:10:16.000",
                    "IsSuspended": false,
                    "RailCarrier": false,
                    "Receivables": true,
                    "RoadCarrier": false,
                    "ZipPostCode": "5312",
                    "ActiveClient": true,
                    "CurrencyCode": "CHF",
                    "EmailInvoice": false,
                    "InsertUserId": 1712,
                    "UpdateUserId": 1712,
                    "VatRegNumber": "CHE264842468",
                    "ForworderAgent": false,
                    "IsCreditEnabled": false,
                    "IsV2PaymentTerm": false,
                    "TransportClient": false,
                    "IsWalkInCustomer": false,
                    "OrganizationCode": "FRC00039841",
                    "EnableBulkInvoice": false,
                    "CustomerIdentifier": "1937295_001",
                    "ReceivableTermsDate": 2,
                    "InsertUserIdUsername": "p100serviceaccountegypt@dpworld.com",
                    "UpdateUserIdUsername": "p100serviceaccountegypt@dpworld.com",
                    "IsClientEInvoiceEnabled": false,
                    "ImportExportClearanceLock": false,
                    "FusionCreditlimitOverrideApproved": false
                }',
    address_data = '{
                        "Id": 262371,
                        "City": "Dottingen",
                        "Email": "Nal@sara-transporte.ch",
                        "State": "Aargau",
                        "label": "Sara Transporte AG, Vorhard 15, Dottingen, Aargau, CHE",
                        "value": "FRO00041585",
                        "Country": "CHE",
                        "OrgGuid": "52634fc2-8c10-4015-8e66-dfd8a62f2b3f",
                        "Address1": "Vorhard 15",
                        "OrgSource": "CRP",
                        "AddressType": 2,
                        "CompanyName": "Sara Transporte AG",
                        "OrgFullName": "Sara Transporte AG",
                        "OrgPayables": false,
                        "ZipPostCode": "5312",
                        "TaxRegNumber": "CHE264842468",
                        "OrgReceivables": true,
                        "OrgActiveClient": true,
                        "AddressShortCode": "FRO00041585",
                        "OrgOrganizationCode": "FRC00039841"
                    }'
where
    id = 1221287 and tenant_id = 486;

-- Update Shipment
update
    shipment_details
set
    client_dps_address_id = 105623,
    updated_at = NOW()
where
    id = 78599 and tenant_id = 486;
