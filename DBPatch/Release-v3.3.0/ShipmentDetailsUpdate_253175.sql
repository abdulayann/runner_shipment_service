-- Shipment ID : (91291)

-- Update Parties
update
    parties
set
    org_code = 'FRC00030636',
    address_code = 'FRO00031643',
    org_id = '147134',
    address_id = '238880',
    updated_at = NOW(),
    org_data = '{
                    "Id": 147134,
                    "City": "Morges",
                    "Guid": "cd42a8f5-cf26-4416-a5e5-a673fa61d991",
                    "Email": "invoice@conforama.ch",
                    "IsGSA": false,
                    "Phone": "1199999999",
                    "label": "CONFORAMA SUISSE S.A.",
                    "value": "FRC00030636",
                    "Broker": false,
                    "Source": "CRP",
                    "Carrier": false,
                    "Country": "CHE",
                    "Address1": "Attn: 221-176 Logistics department / Pascal FELIX Rue de la Gare 32,",
                    "Address2": "",
                    "FullName": "CONFORAMA SUISSE S.A.",
                    "IsActive": 1,
                    "Payables": false,
                    "Services": false,
                    "TenantId": 442,
                    "CompanyId": 372,
                    "Consignee": true,
                    "Consigner": true,
                    "IsParnter": false,
                    "PANNumber": "FRC00030636",
                    "TaxVendor": false,
                    "WareHouse": false,
                    "AirCarrier": false,
                    "ExtraParam": "",
                    "InsertDate": "2025-03-28T20:23:32.000",
                    "SeaCarrier": false,
                    "UpdateDate": "2025-03-28T20:23:32.000",
                    "IsSuspended": false,
                    "RailCarrier": false,
                    "Receivables": false,
                    "RoadCarrier": false,
                    "ZipPostCode": "CH-1110",
                    "ActiveClient": true,
                    "CurrencyCode": "CHF",
                    "EmailInvoice": false,
                    "InsertUserId": 1712,
                    "UpdateUserId": 1712,
                    "ForworderAgent": false,
                    "IsCreditEnabled": false,
                    "IsV2PaymentTerm": false,
                    "TransportClient": false,
                    "IsWalkInCustomer": false,
                    "OrganizationCode": "FRC00030636",
                    "EnableBulkInvoice": false,
                    "InsertUserIdUsername": "p100serviceaccountegypt@dpworld.com",
                    "UpdateUserIdUsername": "p100serviceaccountegypt@dpworld.com",
                    "IsClientEInvoiceEnabled": false,
                    "ImportExportClearanceLock": false,
                    "FusionCreditlimitOverrideApproved": false
                }',
    address_data = '{
                        "Id": 238880,
                        "City": "Morges",
                        "Email": "invoice@conforama.ch",
                        "label": "CONFORAMA SUISSE S.A., Attn: 221-176 Logistics department / Pascal FELIX Rue de la Gare 32,, Morges, CHE",
                        "value": "FRO00031643",
                        "Country": "CHE",
                        "OrgGuid": "cd42a8f5-cf26-4416-a5e5-a673fa61d991",
                        "Address1": "Attn: 221-176 Logistics department / Pascal FELIX Rue de la Gare 32,",
                        "Address2": "",
                        "OrgSource": "CRP",
                        "AddressType": 2,
                        "CompanyName": "CONFORAMA SUISSE S.A.",
                        "OrgFullName": "CONFORAMA SUISSE S.A.",
                        "OrgPayables": false,
                        "ZipPostCode": "CH-1110",
                        "OrgReceivables": false,
                        "OrgActiveClient": true,
                        "AddressShortCode": "FRO00031643",
                        "OrgOrganizationCode": "FRC00030636"
                    }'
where
    id = 1442895 and tenant_id = 706;

-- Update Shipments
UPDATE shipment_details
SET
    client_dps_address_id = 112537,
    updated_at = NOW()
WHERE
    id = 91291
    AND tenant_id = 706;
