-- Shipment ID : (100866)
-- Update Parties
update
    parties
set
    org_code = 'FRC00047570',
    address_code = 'FRO00050051',
    org_id = '164540',
    address_id = '283004',
    updated_at = NOW(),
    org_data = '{
                    "Id": 164540,
                    "City": "Poricei nad Saezavou",
                    "Guid": "aaa2805e-35ea-4eea-a50f-89163b8597fd",
                    "Email": "",
                    "IsGSA": false,
                    "Phone": "",
                    "label": "PRORATIO SRO",
                    "value": "FRC00047570",
                    "Broker": false,
                    "Source": "CRP",
                    "Carrier": false,
                    "Country": "CZE",
                    "Address1": "V Uličkách 4",
                    "Address2": "",
                    "FullName": "PRORATIO SRO",
                    "IsActive": 1,
                    "Payables": false,
                    "Services": false,
                    "TenantId": 442,
                    "CompanyId": 372,
                    "Consignee": true,
                    "Consigner": true,
                    "IsParnter": false,
                    "PANNumber": "FRC00047570",
                    "TaxVendor": false,
                    "WareHouse": false,
                    "AirCarrier": false,
                    "ExtraParam": "",
                    "InsertDate": "2025-09-15T10:36:43.000",
                    "SeaCarrier": false,
                    "UpdateDate": "2025-09-15T10:36:43.000",
                    "IsSuspended": false,
                    "RailCarrier": false,
                    "Receivables": false,
                    "RoadCarrier": false,
                    "ZipPostCode": "25721",
                    "ActiveClient": true,
                    "CurrencyCode": "CZK",
                    "EmailInvoice": false,
                    "InsertUserId": 1712,
                    "UpdateUserId": 1712,
                    "ForworderAgent": false,
                    "IsCreditEnabled": false,
                    "IsV2PaymentTerm": false,
                    "TransportClient": false,
                    "IsWalkInCustomer": false,
                    "OrganizationCode": "FRC00047570",
                    "EnableBulkInvoice": false,
                    "InsertUserIdUsername": "p100serviceaccountegypt@dpworld.com",
                    "UpdateUserIdUsername": "p100serviceaccountegypt@dpworld.com",
                    "IsClientEInvoiceEnabled": false,
                    "ImportExportClearanceLock": false,
                    "FusionCreditlimitOverrideApproved": false
                }',
    address_data = '{
                        "Id": 283004,
                        "City": "Poricei nad Saezavou",
                        "Email": "",
                        "label": "FRO00050051",
                        "value": "FRO00050051",
                        "Country": "CZE",
                        "OrgGuid": "aaa2805e-35ea-4eea-a50f-89163b8597fd",
                        "Address1": "V Uličkách 4",
                        "Address2": "",
                        "OrgSource": "CRP",
                        "AddressType": 2,
                        "CompanyName": "PRORATIO SRO",
                        "OrgFullName": "PRORATIO SRO",
                        "OrgPayables": false,
                        "ZipPostCode": "25721",
                        "OrgReceivables": false,
                        "OrgActiveClient": true,
                        "AddressShortCode": "FRO00050051",
                        "OrgOrganizationCode": "FRC00047570"
                    }'
where
    id = 1609565 and tenant_id = 699;
-- Update Shipments
UPDATE shipment_details
SET
    client_dps_address_id = 112041,
    updated_at = NOW()
WHERE
    id = 100866
    AND tenant_id = 699;