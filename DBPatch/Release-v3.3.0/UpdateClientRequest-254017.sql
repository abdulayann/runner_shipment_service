-- Shipment ID : (91288)
-- Update Parties
update
    parties
set
    org_code = 'FRC00031284',
    address_code = 'FRO00032319',
    org_id = '147825',
    address_id = '240083',
    updated_at = NOW(),
    org_data = '{
                    "Id": 147825,
                    "City": "LOGNES",
                    "Guid": "f329cced-a215-4e9f-bb46-3b12671d3084",
                    "Email": "lravololoarison@conforama.fr",
                    "IsGSA": false,
                    "Phone": "0170771480",
                    "label": "CONFORAMA FRANCE SASU",
                    "value": "FRC00031284",
                    "Broker": false,
                    "Source": "CRP",
                    "Carrier": false,
                    "Country": "FRA",
                    "Address1": "80 Boulevard du Mandinet ",
                    "Address2": "",
                    "FullName": "CONFORAMA FRANCE SASU",
                    "IsActive": 1,
                    "Payables": false,
                    "Services": false,
                    "TenantId": 442,
                    "CompanyId": 372,
                    "Consignee": true,
                    "Consigner": true,
                    "IsParnter": false,
                    "PANNumber": "FRC00031284",
                    "TaxVendor": false,
                    "WareHouse": false,
                    "AirCarrier": false,
                    "ExtraParam": "",
                    "InsertDate": "2025-04-04T18:16:15.000",
                    "SeaCarrier": false,
                    "UpdateDate": "2025-04-04T18:16:15.000",
                    "IsSuspended": false,
                    "RailCarrier": false,
                    "Receivables": false,
                    "RoadCarrier": false,
                    "ZipPostCode": "77432",
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
                    "OrganizationCode": "FRC00031284",
                    "EnableBulkInvoice": false,
                    "InsertUserIdUsername": "p100serviceaccountegypt@dpworld.com",
                    "UpdateUserIdUsername": "p100serviceaccountegypt@dpworld.com",
                    "IsClientEInvoiceEnabled": false,
                    "ImportExportClearanceLock": false,
                    "FusionCreditlimitOverrideApproved": false
                }',
    address_data = '{
                        "Id": 240083,
                        "City": "LOGNES",
                        "Email": "lravololoarison@conforama.fr",
                        "label": "CONFORAMA FRANCE SASU, 80 Boulevard du Mandinet , LOGNES, FRA",
                        "value": "FRO00032319",
                        "Country": "FRA",
                        "OrgGuid": "f329cced-a215-4e9f-bb46-3b12671d3084",
                        "Address1": "80 Boulevard du Mandinet ",
                        "Address2": "",
                        "OrgSource": "CRP",
                        "AddressType": 2,
                        "CompanyName": "CONFORAMA FRANCE SASU",
                        "OrgFullName": "CONFORAMA FRANCE SASU",
                        "OrgPayables": false,
                        "ZipPostCode": "77432",
                        "OrgReceivables": false,
                        "OrgActiveClient": true,
                        "AddressShortCode": "FRO00032319",
                        "OrgOrganizationCode": "FRC00031284"
                    }'
where
    id = 1626249 and tenant_id = 706;
-- Update Shipments
UPDATE shipment_details
SET
    client_dps_address_id = 240083,
    updated_at = NOW()
WHERE
    id = 91288
    AND tenant_id = 706;

-- Shipment ID : (91291)
update
    parties
set
    org_code = 'FRC00031284',
    address_code = 'FRO00032319',
    org_id = '147825',
    address_id = '240083',
    updated_at = NOW(),
    org_data = '{
                    "Id": 147825,
                    "City": "LOGNES",
                    "Guid": "f329cced-a215-4e9f-bb46-3b12671d3084",
                    "Email": "lravololoarison@conforama.fr",
                    "IsGSA": false,
                    "Phone": "0170771480",
                    "label": "CONFORAMA FRANCE SASU",
                    "value": "FRC00031284",
                    "Broker": false,
                    "Source": "CRP",
                    "Carrier": false,
                    "Country": "FRA",
                    "Address1": "80 Boulevard du Mandinet ",
                    "Address2": "",
                    "FullName": "CONFORAMA FRANCE SASU",
                    "IsActive": 1,
                    "Payables": false,
                    "Services": false,
                    "TenantId": 442,
                    "CompanyId": 372,
                    "Consignee": true,
                    "Consigner": true,
                    "IsParnter": false,
                    "PANNumber": "FRC00031284",
                    "TaxVendor": false,
                    "WareHouse": false,
                    "AirCarrier": false,
                    "ExtraParam": "",
                    "InsertDate": "2025-04-04T18:16:15.000",
                    "SeaCarrier": false,
                    "UpdateDate": "2025-04-04T18:16:15.000",
                    "IsSuspended": false,
                    "RailCarrier": false,
                    "Receivables": false,
                    "RoadCarrier": false,
                    "ZipPostCode": "77432",
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
                    "OrganizationCode": "FRC00031284",
                    "EnableBulkInvoice": false,
                    "InsertUserIdUsername": "p100serviceaccountegypt@dpworld.com",
                    "UpdateUserIdUsername": "p100serviceaccountegypt@dpworld.com",
                    "IsClientEInvoiceEnabled": false,
                    "ImportExportClearanceLock": false,
                    "FusionCreditlimitOverrideApproved": false
                }',
    address_data = '{
                        "Id": 240083,
                        "City": "LOGNES",
                        "Email": "lravololoarison@conforama.fr",
                        "label": "CONFORAMA FRANCE SASU, 80 Boulevard du Mandinet , LOGNES, FRA",
                        "value": "FRO00032319",
                        "Country": "FRA",
                        "OrgGuid": "f329cced-a215-4e9f-bb46-3b12671d3084",
                        "Address1": "80 Boulevard du Mandinet ",
                        "Address2": "",
                        "OrgSource": "CRP",
                        "AddressType": 2,
                        "CompanyName": "CONFORAMA FRANCE SASU",
                        "OrgFullName": "CONFORAMA FRANCE SASU",
                        "OrgPayables": false,
                        "ZipPostCode": "77432",
                        "OrgReceivables": false,
                        "OrgActiveClient": true,
                        "AddressShortCode": "FRO00032319",
                        "OrgOrganizationCode": "FRC00031284"
                    }'
where
    id = 1626249 and tenant_id = 706;
-- Update Shipments
UPDATE shipment_details
SET
    client_dps_address_id = 240083,
    updated_at = NOW()
WHERE
    id = 91291
    AND tenant_id = 706;