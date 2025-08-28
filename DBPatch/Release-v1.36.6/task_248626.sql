-- Query 1: Update for FRC00023811 / FRO00024547
-- Shipment id : ( Id = 81604 )

-- Update Parties
update
    parties
set
    org_code = 'FRC00023811',
    address_code = 'FRO00024547',
    org_id = '142952',
    address_id = '227107',
    updated_at = NOW(),
    org_data = '{
                    "Id": 142952,
                    "City": "Durrenasch",
                    "Guid": "dc24e25a-012d-4b2a-b3ea-7e4b7f837cbf",
                    "Email": "gregoire.paul@bertschi.com",
                    "IsGSA": false,
                    "Phone": "796065761",
                    "State": "Aargau",
                    "label": "Bertschi Global AG",
                    "value": "FRC00023811",
                    "Broker": false,
                    "Source": "CRP",
                    "Carrier": false,
                    "Country": "CHE",
                    "Address1": "Hutmatt Street 22",
                    "FullName": "Bertschi Global AG",
                    "IsActive": 1,
                    "Payables": false,
                    "Services": false,
                    "TenantId": 442,
                    "CompanyId": 372,
                    "Consignee": true,
                    "Consigner": true,
                    "IsParnter": false,
                    "PANNumber": "CHE105778633",
                    "TaxVendor": false,
                    "WareHouse": false,
                    "AirCarrier": false,
                    "ExtraParam": "",
                    "InsertDate": "2025-02-12T11:24:56.000",
                    "SeaCarrier": false,
                    "UpdateDate": "2025-02-18T19:09:56.000",
                    "IsSuspended": false,
                    "RailCarrier": false,
                    "Receivables": true,
                    "RoadCarrier": false,
                    "ZipPostCode": "CH 5724",
                    "ActiveClient": true,
                    "CurrencyCode": "CHF",
                    "EmailInvoice": false,
                    "InsertUserId": 1712,
                    "UpdateUserId": 1712,
                    "VatRegNumber": "CHE105778633",
                    "ForworderAgent": false,
                    "IsCreditEnabled": false,
                    "IsV2PaymentTerm": false,
                    "TransportClient": false,
                    "IsWalkInCustomer": false,
                    "OrganizationCode": "FRC00023811",
                    "EnableBulkInvoice": false,
                    "CustomerIdentifier": "1772467_001",
                    "ReceivableTermsDate": 2,
                    "InsertUserIdUsername": "p100serviceaccountegypt@dpworld.com",
                    "OrgQuoteValidityDays": 0,
                    "UpdateUserIdUsername": "p100serviceaccountegypt@dpworld.com",
                    "CreditLimitOfConfigRow": "",
                    "CreditTermsOfConfigRow": "",
                    "IsClientEInvoiceEnabled": false,
                    "ImportExportClearanceLock": false,
                    "FusionCreditlimitOverrideApproved": false
                }',
    address_data = '{
                        "Id": 227107,
                        "City": "Durrenasch",
                        "Email": "gregoire.paul@bertschi.com",
                        "State": "Aargau",
                        "label": "FRO00024547",
                        "value": "FRO00024547",
                        "Country": "CHE",
                        "OrgGuid": "dc24e25a-012d-4b2a-b3ea-7e4b7f837cbf",
                        "Address1": "Hutmatt Street 22",
                        "OrgSource": "CRP",
                        "AddressType": 2,
                        "CompanyName": "Bertschi Global AG",
                        "OrgFullName": "Bertschi Global AG",
                        "OrgPayables": false,
                        "ZipPostCode": "CH 5724",
                        "TaxRegNumber": "CHE105778633",
                        "OrgReceivables": true,
                        "OrgActiveClient": true,
                        "AddressShortCode": "FRO00024547",
                        "OrgOrganizationCode": "FRC00023811"
                    }'
where
    id = 1272590 and tenant_id = 468;

-- Update Shipment
UPDATE shipment_details
SET
    client_dps_address_id = 104884,
    updated_at = NOW()
WHERE
    id = 81604
    AND tenant_id = 468;



