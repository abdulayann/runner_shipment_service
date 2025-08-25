-- Query 1: Update for FRC00036694 / FRO00038195 (FRANKE ASIA SOURCING LTD.)
-- Shipment: SNZCSS25081285
-- Reason: User requested to update client details due to incorrect addition by origin

update
    parties
set
    org_code = 'FRC00036694',
    address_code = 'FRO00038195',
    org_id = '153728',
    address_id = '256002',
    updated_at = NOW(),
    org_data = '{
    "Id": 153728,
    "City": "Hong Kong",
    "Guid": "33585c43-0a81-4346-8b0b-e7a0af06233c",
    "Email": "Wayne.Lv@franke.com",
    "IsGSA": false,
    "Phone": "7508415251",
    "label": "FRANKE ASIA SOURCING LTD.",
    "value": "FRC00036694",
    "Broker": false,
    "Source": "CRP",
    "Carrier": false,
    "Country": "CHN",
    "Address1": "Suite 701,1063 Kings Road,Quarry Bay",
    "FullName": "FRANKE ASIA SOURCING LTD.",
    "IsActive": 1,
    "Payables": false,
    "Services": false,
    "TenantId": 442,
    "CompanyId": 372,
    "Consignee": true,
    "Consigner": true,
    "IsParnter": false,
    "PANNumber": "32454647-000-03-25-4",
    "TaxVendor": false,
    "WareHouse": false,
    "AirCarrier": false,
    "ExtraParam": "",
    "InsertDate": "2025-06-05T04:08:10.000",
    "SeaCarrier": false,
    "UpdateDate": "2025-06-19T17:56:26.000",
    "IsSuspended": false,
    "RailCarrier": false,
    "Receivables": true,
    "RoadCarrier": false,
    "ZipPostCode": "999077",
    "ActiveClient": true,
    "CurrencyCode": "CNY",
    "EmailInvoice": false,
    "InsertUserId": 1712,
    "UpdateUserId": 3515,
    "VatRegNumber": "3245464700003254",
    "ForworderAgent": false,
    "CustomerTaxType": 2,
    "IsCreditEnabled": false,
    "IsV2PaymentTerm": false,
    "TransportClient": false,
    "IsWalkInCustomer": false,
    "OrganizationCode": "FRC00036694",
    "EnableBulkInvoice": false,
    "CustomerIdentifier": "1911289_001",
    "InsertUserIdUsername": "p100serviceaccountegypt@dpworld.com",
    "UpdateUserIdUsername": "243647",
    "IsClientEInvoiceEnabled": false,
    "ImportExportClearanceLock": false,
    "FusionCreditlimitOverrideApproved": false
}',
    address_data = '{
    "Id": 256002,
    "City": "Hong Kong",
    "Email": "Wayne.Lv@franke.com",
    "label": "FRANKE ASIA SOURCING LTD., Suite 701,1063 Kings Road,Quarry Bay, Hong Kong, CHN",
    "value": "FRO00038195",
    "Country": "CHN",
    "OrgGuid": "33585c43-0a81-4346-8b0b-e7a0af06233c",
    "Address1": "Suite 701,1063 Kings Road,Quarry Bay",
    "OrgSource": "CRP",
    "AddressType": 1,
    "CompanyName": "FRANKE ASIA SOURCING LTD.",
    "OrgFullName": "FRANKE ASIA SOURCING LTD.",
    "OrgPayables": false,
    "ZipPostCode": "999077",
    "TaxRegNumber": "3245464700003254",
    "OrgReceivables": true,
    "SiteIdentifier": "1911289_001_CNSGH_002_B",
    "OrgActiveClient": true,
    "AddressShortCode": "FRO00038195",
    "OrgOrganizationCode": "FRC00036694"
}'
where
    id = 1318684 and tenant_id = 713;

update
    shipment_details
set
    client_dps_address_id = 103347,
    updated_at = NOW()
where
    id = 84112 and tenant_id = 713;

-- Query 2: Update for FRC00036694 / FRO00038195 (FRANKE ASIA SOURCING LTD.)
-- Shipment: SNZCSS25081441
-- Reason: User requested to update client details due to incorrect addition by origin

update
    parties
set
    org_code = 'FRC00036694',
    address_code = 'FRO00038195',
    org_id = '153728',
    address_id = '256002',
    updated_at = NOW(),
    org_data = '{
    "Id": 153728,
    "City": "Hong Kong",
    "Guid": "33585c43-0a81-4346-8b0b-e7a0af06233c",
    "Email": "Wayne.Lv@franke.com",
    "IsGSA": false,
    "Phone": "7508415251",
    "label": "FRANKE ASIA SOURCING LTD.",
    "value": "FRC00036694",
    "Broker": false,
    "Source": "CRP",
    "Carrier": false,
    "Country": "CHN",
    "Address1": "Suite 701,1063 Kings Road,Quarry Bay",
    "FullName": "FRANKE ASIA SOURCING LTD.",
    "IsActive": 1,
    "Payables": false,
    "Services": false,
    "TenantId": 442,
    "CompanyId": 372,
    "Consignee": true,
    "Consigner": true,
    "IsParnter": false,
    "PANNumber": "32454647-000-03-25-4",
    "TaxVendor": false,
    "WareHouse": false,
    "AirCarrier": false,
    "ExtraParam": "",
    "InsertDate": "2025-06-05T04:08:10.000",
    "SeaCarrier": false,
    "UpdateDate": "2025-06-19T17:56:26.000",
    "IsSuspended": false,
    "RailCarrier": false,
    "Receivables": true,
    "RoadCarrier": false,
    "ZipPostCode": "999077",
    "ActiveClient": true,
    "CurrencyCode": "CNY",
    "EmailInvoice": false,
    "InsertUserId": 1712,
    "UpdateUserId": 3515,
    "VatRegNumber": "3245464700003254",
    "ForworderAgent": false,
    "CustomerTaxType": 2,
    "IsCreditEnabled": false,
    "IsV2PaymentTerm": false,
    "TransportClient": false,
    "IsWalkInCustomer": false,
    "OrganizationCode": "FRC00036694",
    "EnableBulkInvoice": false,
    "CustomerIdentifier": "1911289_001",
    "InsertUserIdUsername": "p100serviceaccountegypt@dpworld.com",
    "UpdateUserIdUsername": "243647",
    "IsClientEInvoiceEnabled": false,
    "ImportExportClearanceLock": false,
    "FusionCreditlimitOverrideApproved": false
}',
    address_data = '{
    "Id": 256002,
    "City": "Hong Kong",
    "Email": "Wayne.Lv@franke.com",
    "label": "FRANKE ASIA SOURCING LTD., Suite 701,1063 Kings Road,Quarry Bay, Hong Kong, CHN",
    "value": "FRO00038195",
    "Country": "CHN",
    "OrgGuid": "33585c43-0a81-4346-8b0b-e7a0af06233c",
    "Address1": "Suite 701,1063 Kings Road,Quarry Bay",
    "OrgSource": "CRP",
    "AddressType": 1,
    "CompanyName": "FRANKE ASIA SOURCING LTD.",
    "OrgFullName": "FRANKE ASIA SOURCING LTD.",
    "OrgPayables": false,
    "ZipPostCode": "999077",
    "TaxRegNumber": "3245464700003254",
    "OrgReceivables": true,
    "SiteIdentifier": "1911289_001_CNSGH_002_B",
    "OrgActiveClient": true,
    "AddressShortCode": "FRO00038195",
    "OrgOrganizationCode": "FRC00036694"
}'
where
    id = 1378311 and tenant_id = 713;

update
    shipment_details
set
    client_dps_address_id = 103347,
    updated_at = NOW()
where
    id = 87518 and tenant_id = 713;

-- Query 3: Update for FRC00036697 / FRO00038198 (Franke (China) Kitchen System Co., Ltd.)
-- Shipment: SNZCSS25081286
-- Reason: User requested to update client details due to incorrect addition by origin

update
    parties
set
    org_code = 'FRC00036697',
    address_code = 'FRO00038198',
    org_id = '153730',
    address_id = '256004',
    updated_at = NOW(),
    org_data = '{
    "Id": 153730,
    "City": "Heshan City",
    "Guid": "b5c40877-7921-4e5f-83d1-abcd49ef8329",
    "Email": "Emily.Feng@franke.com",
    "IsGSA": false,
    "Phone": "7508412832",
    "State": "Guangdong",
    "label": "Franke (China) Kitchen System Co., Ltd.",
    "value": "FRC00036697",
    "Broker": false,
    "Source": "CRP",
    "Carrier": false,
    "Country": "CHN",
    "Address1": "318 Yinglang Industrial Zone Shaping Town ",
    "FullName": "Franke (China) Kitchen System Co., Ltd.",
    "IsActive": 1,
    "Payables": false,
    "Services": false,
    "TenantId": 442,
    "CompanyId": 372,
    "Consignee": true,
    "Consigner": true,
    "IsParnter": false,
    "PANNumber": "91440700617701181Y",
    "TaxVendor": false,
    "WareHouse": false,
    "AirCarrier": false,
    "ExtraParam": "",
    "InsertDate": "2025-06-05T04:20:47.000",
    "SeaCarrier": false,
    "UpdateDate": "2025-06-12T19:22:37.000",
    "IsSuspended": false,
    "RailCarrier": false,
    "Receivables": true,
    "RoadCarrier": false,
    "ZipPostCode": "529700",
    "ActiveClient": true,
    "CurrencyCode": "CNY",
    "EmailInvoice": false,
    "InsertUserId": 1712,
    "UpdateUserId": 4444,
    "VatRegNumber": "91440700617701181Y",
    "ForworderAgent": false,
    "CustomerTaxType": 2,
    "IsCreditEnabled": false,
    "IsV2PaymentTerm": false,
    "TransportClient": false,
    "IsWalkInCustomer": false,
    "OrganizationCode": "FRC00036697",
    "EnableBulkInvoice": false,
    "CustomerIdentifier": "1906717_001",
    "InsertUserIdUsername": "p100serviceaccountegypt@dpworld.com",
    "UpdateUserIdUsername": "manohar.tumbre",
    "IsClientEInvoiceEnabled": false,
    "ImportExportClearanceLock": false,
    "FusionCreditlimitOverrideApproved": false
}',
    address_data = '{
    "Id": 256004,
    "City": "Heshan City",
    "Email": "Emily.Feng@franke.com",
    "State": "Guangdong",
    "label": "FRO00038198",
    "value": "FRO00038198",
    "Country": "CHN",
    "OrgGuid": "b5c40877-7921-4e5f-83d1-abcd49ef8329",
    "Address1": "318 Yinglang Industrial Zone Shaping Town ",
    "OrgSource": "CRP",
    "AddressType": 1,
    "CompanyName": "Franke (China) Kitchen System Co., Ltd.",
    "OrgFullName": "Franke (China) Kitchen System Co., Ltd.",
    "OrgPayables": false,
    "ZipPostCode": "529700",
    "TaxRegNumber": "91440700617701181Y",
    "OrgReceivables": true,
    "SiteIdentifier": "1906717_001_CNSGH_002_B",
    "OrgActiveClient": true,
    "AddressShortCode": "FRO00038198",
    "OrgOrganizationCode": "FRC00036697"
}'
where
    id = 1318722 and tenant_id = 713;

update
    shipment_details
set
    client_dps_address_id = 102119,
    updated_at = NOW()
where
    id = 84114 and tenant_id = 713;

-- Query 4: Update for FRC00036697 / FRO00038198 (Franke (China) Kitchen System Co., Ltd.)
-- Shipment: SNZCSS25081443
-- Reason: User requested to update client details due to incorrect addition by origin

update
    parties
set
    org_code = 'FRC00036697',
    address_code = 'FRO00038198',
    org_id = '153730',
    address_id = '256004',
    updated_at = NOW(),
    org_data = '{
    "Id": 153730,
    "City": "Heshan City",
    "Guid": "b5c40877-7921-4e5f-83d1-abcd49ef8329",
    "Email": "Emily.Feng@franke.com",
    "IsGSA": false,
    "Phone": "7508412832",
    "State": "Guangdong",
    "label": "Franke (China) Kitchen System Co., Ltd.",
    "value": "FRC00036697",
    "Broker": false,
    "Source": "CRP",
    "Carrier": false,
    "Country": "CHN",
    "Address1": "318 Yinglang Industrial Zone Shaping Town ",
    "FullName": "Franke (China) Kitchen System Co., Ltd.",
    "IsActive": 1,
    "Payables": false,
    "Services": false,
    "TenantId": 442,
    "CompanyId": 372,
    "Consignee": true,
    "Consigner": true,
    "IsParnter": false,
    "PANNumber": "91440700617701181Y",
    "TaxVendor": false,
    "WareHouse": false,
    "AirCarrier": false,
    "ExtraParam": "",
    "InsertDate": "2025-06-05T04:20:47.000",
    "SeaCarrier": false,
    "UpdateDate": "2025-06-12T19:22:37.000",
    "IsSuspended": false,
    "RailCarrier": false,
    "Receivables": true,
    "RoadCarrier": false,
    "ZipPostCode": "529700",
    "ActiveClient": true,
    "CurrencyCode": "CNY",
    "EmailInvoice": false,
    "InsertUserId": 1712,
    "UpdateUserId": 4444,
    "VatRegNumber": "91440700617701181Y",
    "ForworderAgent": false,
    "CustomerTaxType": 2,
    "IsCreditEnabled": false,
    "IsV2PaymentTerm": false,
    "TransportClient": false,
    "IsWalkInCustomer": false,
    "OrganizationCode": "FRC00036697",
    "EnableBulkInvoice": false,
    "CustomerIdentifier": "1906717_001",
    "InsertUserIdUsername": "p100serviceaccountegypt@dpworld.com",
    "UpdateUserIdUsername": "manohar.tumbre",
    "IsClientEInvoiceEnabled": false,
    "ImportExportClearanceLock": false,
    "FusionCreditlimitOverrideApproved": false
}',
    address_data = '{
    "Id": 256004,
    "City": "Heshan City",
    "Email": "Emily.Feng@franke.com",
    "State": "Guangdong",
    "label": "FRO00038198",
    "value": "FRO00038198",
    "Country": "CHN",
    "OrgGuid": "b5c40877-7921-4e5f-83d1-abcd49ef8329",
    "Address1": "318 Yinglang Industrial Zone Shaping Town ",
    "OrgSource": "CRP",
    "AddressType": 1,
    "CompanyName": "Franke (China) Kitchen System Co., Ltd.",
    "OrgFullName": "Franke (China) Kitchen System Co., Ltd.",
    "OrgPayables": false,
    "ZipPostCode": "529700",
    "TaxRegNumber": "91440700617701181Y",
    "OrgReceivables": true,
    "SiteIdentifier": "1906717_001_CNSGH_002_B",
    "OrgActiveClient": true,
    "AddressShortCode": "FRO00038198",
    "OrgOrganizationCode": "FRC00036697"
}'
where
    id = 1378567 and tenant_id = 713;

update
    shipment_details
set
    client_dps_address_id = 102119,
    updated_at = NOW()
where
    id = 87532 and tenant_id = 713;