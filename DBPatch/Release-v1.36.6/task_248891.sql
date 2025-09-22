-- Query 1: Update for FRC00043897 / FRO00045882
-- Shipment id : ( Id = 87545 )
-- Update Parties, Party ID : =  1378791
update
    parties
set
    org_code = 'FRC00043897',
    address_code = 'FRO00045882',
    org_id = '161081',
    address_id = '272827',
    updated_at = NOW(),
    org_data = '{
                    "Id": 161081,
                    "City": "PRATO",
                    "Guid": "d21f5d65-8707-40b1-9d58-81c19da35770",
                    "Email": "",
                    "IsGSA": false,
                    "Phone": "",
                    "label": "WAY S.R.L",
                    "value": "FRC00043897",
                    "Broker": false,
                    "Source": "CRP",
                    "Carrier": false,
                    "Country": "ITA",
                    "Address1": "VIA ARRIGO DA SETTIMELLO 20/22",
                    "Address2": "",
                    "FullName": "WAY S.R.L",
                    "IsActive": 1,
                    "Payables": false,
                    "Services": false,
                    "TenantId": 442,
                    "CompanyId": 372,
                    "Consignee": true,
                    "Consigner": true,
                    "IsParnter": false,
                    "PANNumber": "FRC00043897",
                    "TaxVendor": false,
                    "WareHouse": false,
                    "AirCarrier": false,
                    "ExtraParam": "",
                    "InsertDate": "2025-08-12T09:23:40.000",
                    "SeaCarrier": false,
                    "UpdateDate": "2025-08-12T09:23:40.000",
                    "IsSuspended": false,
                    "RailCarrier": false,
                    "Receivables": false,
                    "RoadCarrier": false,
                    "ZipPostCode": "59100",
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
                    "OrganizationCode": "FRC00043897",
                    "EnableBulkInvoice": false,
                    "InsertUserIdUsername": "p100serviceaccountegypt@dpworld.com",
                    "UpdateUserIdUsername": "p100serviceaccountegypt@dpworld.com",
                    "IsClientEInvoiceEnabled": false,
                    "ImportExportClearanceLock": false,
                    "FusionCreditlimitOverrideApproved": false
                }',
    address_data = '{
                        "Id": 272827,
                        "City": "PRATO",
                        "Email": "",
                        "label": "FRO00045882",
                        "value": "FRO00045882",
                        "Country": "ITA",
                        "OrgGuid": "d21f5d65-8707-40b1-9d58-81c19da35770",
                        "Address1": "VIA ARRIGO DA SETTIMELLO 20/22",
                        "Address2": "",
                        "OrgSource": "CRP",
                        "AddressType": 2,
                        "CompanyName": "WAY S.R.L",
                        "OrgFullName": "WAY S.R.L",
                        "OrgPayables": false,
                        "ZipPostCode": "59100",
                        "OrgReceivables": false,
                        "OrgActiveClient": true,
                        "AddressShortCode": "FRO00045882",
                        "OrgOrganizationCode": "FRC00043897"
                    }'
where
    id = 1378791 and tenant_id = 709;

-- Update Shipment
update
    shipment_details
set
    client_dps_address_id = 107170,
    updated_at = NOW()
where
    id = 87545 and tenant_id = 709;
