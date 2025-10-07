
-- 1. FRC00005003 (Al Sharif Group & Kec Ltd Co.)
-- Shipment: BOMS24082106

UPDATE parties
SET
    org_code = 'FRC00005003',
    address_code = 'FRO00004477',
    org_id = '63104',
    address_id = '79603',
    updated_at = NOW(),
    org_data = '{
        "Id": 63104,
        "City": "Jeddah",
        "Guid": "79328390-2b0d-43b5-be7e-87ee3d98e3e2",
        "Email": "khanf1@kecrpg.com",
        "IsGSA": false,
        "Phone": "568173182",
        "label": "Al Sharif Group & Kec Ltd Co.",
        "value": "FRC00005003",
        "Broker": false,
        "Source": "CRP",
        "Carrier": false,
        "Country": "SAU",
        "Address1": "Al Khalidiyah District",
        "FullName": "Al Sharif Group & Kec Ltd Co.",
        "IsActive": 1,
        "Payables": false,
        "Services": false,
        "TenantId": 442,
        "CompanyId": 372,
        "Consignee": true,
        "Consigner": true,
        "IsParnter": false,
        "PANNumber": "4030131490",
        "TaxVendor": false,
        "WareHouse": false,
        "AirCarrier": false,
        "ExtraParam": "",
        "InsertDate": "2024-02-20T12:49:10.000",
        "SeaCarrier": false,
        "UpdateDate": "2025-07-14T13:53:29.000",
        "IsSuspended": false,
        "RailCarrier": false,
        "Receivables": true,
        "RoadCarrier": false,
        "ZipPostCode": "23422",
        "ActiveClient": true,
        "CurrencyCode": "SAR",
        "EmailInvoice": false,
        "InsertUserId": 1712,
        "UpdateUserId": 1712,
        "VatRegNumber": "300187976400003",
        "ForworderAgent": false,
        "PaymentTermsId": 21,
        "IsCreditEnabled": false,
        "IsV2PaymentTerm": false,
        "TransportClient": false,
        "IsWalkInCustomer": false,
        "OrganizationCode": "FRC00005003",
        "PaymentTermsCode": "C0",
        "EnableBulkInvoice": false,
        "CustomerIdentifier": "1376922_001",
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
        "Id": 79603,
        "City": "Jeddah",
        "Email": "khanf1@kecrpg.com",
        "label": "FRO00004477",
        "value": "FRO00004477",
        "Country": "SAU",
        "OrgGuid": "79328390-2b0d-43b5-be7e-87ee3d98e3e2",
        "Address1": "Al Khalidiyah District",
        "OrgSource": "CRP",
        "AddressType": 1,
        "CompanyName": "Al Sharif Group & Kec Ltd Co.",
        "OrgFullName": "Al Sharif Group & Kec Ltd Co.",
        "OrgPayables": false,
        "ZipPostCode": "23422",
        "TaxRegNumber": "300187976400003",
        "OrgReceivables": true,
        "SiteIdentifier": "1376922_001_FF-SA_001_B",
        "OrgActiveClient": true,
        "AddressShortCode": "FRO00004477",
        "OrgOrganizationCode": "FRC00005003"
    }'
WHERE
    id = 905647
    AND tenant_id = 473;

UPDATE shipment_details
SET
    client_dps_address_id = 114112,
    updated_at = NOW()
WHERE
    id = 60938
    AND tenant_id = 473;

-- ========================================
-- 2. FRC00021052 (DP WORLD MIDDLE EAST LIMITED)
-- Shipments: KULA25050699, JHBA25050152, KULA25050754
-- ========================================

-- Shipment: KULA25050699
UPDATE parties
SET
    org_code = 'FRC00021052',
    address_code = 'FRO00021683',
    org_id = '136607',
    address_id = '202166',
    updated_at = NOW(),
    org_data = '{
        "Id": 136607,
        "City": "Jeddah",
        "Guid": "4a65d0e3-71c2-44c5-b1de-e81691ff2255",
        "Email": "dpwjed.customerservice@dpworld.com",
        "IsGSA": false,
        "Phone": "126270080",
        "State": "Mekka",
        "label": "DP WORLD MIDDLE EAST LIMITED",
        "value": "FRC00021052",
        "Source": "CRP",
        "Country": "SAU",
        "Address1": "South Container Terminal, 21474 Jeddah - Hindawiyah",
        "FullName": "DP WORLD MIDDLE EAST LIMITED",
        "IsActive": 1,
        "Payables": false,
        "TenantId": 442,
        "CompanyId": 372,
        "Consignee": true,
        "Consigner": true,
        "TaxVendor": false,
        "ExtraParam": "{}",
        "InsertDate": "2024-11-15T14:23:44.000",
        "UpdateDate": "2025-02-05T18:33:01.000",
        "IsSuspended": false,
        "Receivables": false,
        "ZipPostCode": "21474",
        "ActiveClient": true,
        "CurrencyCode": "SAR",
        "EmailInvoice": false,
        "InsertUserId": 1712,
        "ForworderAgent": false,
        "IsCreditEnabled": false,
        "IsV2PaymentTerm": false,
        "OrganizationCode": "FRC00021052",
        "EnableBulkInvoice": false,
        "InsertUserIdUsername": "p100serviceaccountegypt@dpworld.com",
        "OrgQuoteValidityDays": 0,
        "CreditLimitOfConfigRow": "",
        "CreditTermsOfConfigRow": "",
        "IsClientEInvoiceEnabled": false,
        "ImportExportClearanceLock": false
    }',
    address_data = '{
        "Id": 202166,
        "City": "Jeddah",
        "Email": "dpwjed.customerservice@dpworld.com",
        "State": "Mekka",
        "label": "DP WORLD MIDDLE EAST LIMITED, South Container Terminal, 21474 Jeddah - Hindawiyah, Jeddah, Mekka, SAU",
        "value": "FRO00021683",
        "Country": "SAU",
        "OrgGuid": "4a65d0e3-71c2-44c5-b1de-e81691ff2255",
        "Address1": "South Container Terminal, 21474 Jeddah - Hindawiyah",
        "OrgSource": "CRP",
        "AddressType": 2,
        "CompanyName": "DP WORLD MIDDLE EAST LIMITED",
        "OrgFullName": "DP WORLD MIDDLE EAST LIMITED",
        "OrgPayables": false,
        "ZipPostCode": "21474",
        "OrgReceivables": false,
        "OrgActiveClient": true,
        "AddressShortCode": "FRO00021683",
        "OrgOrganizationCode": "FRC00021052"
    }'
WHERE
    id = 823391
    AND tenant_id = 473;

UPDATE shipment_details
SET
    client_dps_address_id = 112492,
    updated_at = NOW()
WHERE
    id = 56346
    AND tenant_id = 473;

-- Shipment: JHBA25050152
UPDATE parties
SET
    org_code = 'FRC00021052',
    address_code = 'FRO00021683',
    org_id = '136607',
    address_id = '202166',
    updated_at = NOW(),
    org_data = '{
        "Id": 136607,
        "City": "Jeddah",
        "Guid": "4a65d0e3-71c2-44c5-b1de-e81691ff2255",
        "Email": "dpwjed.customerservice@dpworld.com",
        "IsGSA": false,
        "Phone": "126270080",
        "State": "Mekka",
        "label": "DP WORLD MIDDLE EAST LIMITED",
        "value": "FRC00021052",
        "Source": "CRP",
        "Country": "SAU",
        "Address1": "South Container Terminal, 21474 Jeddah - Hindawiyah",
        "FullName": "DP WORLD MIDDLE EAST LIMITED",
        "IsActive": 1,
        "Payables": false,
        "TenantId": 442,
        "CompanyId": 372,
        "Consignee": true,
        "Consigner": true,
        "TaxVendor": false,
        "ExtraParam": "{}",
        "InsertDate": "2024-11-15T14:23:44.000",
        "UpdateDate": "2025-02-05T18:33:01.000",
        "IsSuspended": false,
        "Receivables": false,
        "ZipPostCode": "21474",
        "ActiveClient": true,
        "CurrencyCode": "SAR",
        "EmailInvoice": false,
        "InsertUserId": 1712,
        "ForworderAgent": false,
        "IsCreditEnabled": false,
        "IsV2PaymentTerm": false,
        "OrganizationCode": "FRC00021052",
        "EnableBulkInvoice": false,
        "InsertUserIdUsername": "p100serviceaccountegypt@dpworld.com",
        "OrgQuoteValidityDays": 0,
        "CreditLimitOfConfigRow": "",
        "CreditTermsOfConfigRow": "",
        "IsClientEInvoiceEnabled": false,
        "ImportExportClearanceLock": false
    }',
    address_data = '{
        "Id": 202166,
        "City": "Jeddah",
        "Email": "dpwjed.customerservice@dpworld.com",
        "State": "Mekka",
        "label": "DP WORLD MIDDLE EAST LIMITED, South Container Terminal, 21474 Jeddah - Hindawiyah, Jeddah, Mekka, SAU",
        "value": "FRO00021683",
        "Country": "SAU",
        "OrgGuid": "4a65d0e3-71c2-44c5-b1de-e81691ff2255",
        "Address1": "South Container Terminal, 21474 Jeddah - Hindawiyah",
        "OrgSource": "CRP",
        "AddressType": 2,
        "CompanyName": "DP WORLD MIDDLE EAST LIMITED",
        "OrgFullName": "DP WORLD MIDDLE EAST LIMITED",
        "OrgPayables": false,
        "ZipPostCode": "21474",
        "OrgReceivables": false,
        "OrgActiveClient": true,
        "AddressShortCode": "FRO00021683",
        "OrgOrganizationCode": "FRC00021052"
    }'
WHERE
    id = 794030
    AND tenant_id = 473;

UPDATE shipment_details
SET
    client_dps_address_id = 112492,
    updated_at = NOW()
WHERE
    id = 54767
    AND tenant_id = 473;

-- Shipment: KULA25050754
UPDATE parties
SET
    org_code = 'FRC00021052',
    address_code = 'FRO00021683',
    org_id = '136607',
    address_id = '202166',
    updated_at = NOW(),
    org_data = '{
        "Id": 136607,
        "City": "Jeddah",
        "Guid": "4a65d0e3-71c2-44c5-b1de-e81691ff2255",
        "Email": "dpwjed.customerservice@dpworld.com",
        "IsGSA": false,
        "Phone": "126270080",
        "State": "Mekka",
        "label": "DP WORLD MIDDLE EAST LIMITED",
        "value": "FRC00021052",
        "Source": "CRP",
        "Country": "SAU",
        "Address1": "South Container Terminal, 21474 Jeddah - Hindawiyah",
        "FullName": "DP WORLD MIDDLE EAST LIMITED",
        "IsActive": 1,
        "Payables": false,
        "TenantId": 442,
        "CompanyId": 372,
        "Consignee": true,
        "Consigner": true,
        "TaxVendor": false,
        "ExtraParam": "{}",
        "InsertDate": "2024-11-15T14:23:44.000",
        "UpdateDate": "2025-02-05T18:33:01.000",
        "IsSuspended": false,
        "Receivables": false,
        "ZipPostCode": "21474",
        "ActiveClient": true,
        "CurrencyCode": "SAR",
        "EmailInvoice": false,
        "InsertUserId": 1712,
        "ForworderAgent": false,
        "IsCreditEnabled": false,
        "IsV2PaymentTerm": false,
        "OrganizationCode": "FRC00021052",
        "EnableBulkInvoice": false,
        "InsertUserIdUsername": "p100serviceaccountegypt@dpworld.com",
        "OrgQuoteValidityDays": 0,
        "CreditLimitOfConfigRow": "",
        "CreditTermsOfConfigRow": "",
        "IsClientEInvoiceEnabled": false,
        "ImportExportClearanceLock": false
    }',
    address_data = '{
        "Id": 202166,
        "City": "Jeddah",
        "Email": "dpwjed.customerservice@dpworld.com",
        "State": "Mekka",
        "label": "DP WORLD MIDDLE EAST LIMITED, South Container Terminal, 21474 Jeddah - Hindawiyah, Jeddah, Mekka, SAU",
        "value": "FRO00021683",
        "Country": "SAU",
        "OrgGuid": "4a65d0e3-71c2-44c5-b1de-e81691ff2255",
        "Address1": "South Container Terminal, 21474 Jeddah - Hindawiyah",
        "OrgSource": "CRP",
        "AddressType": 2,
        "CompanyName": "DP WORLD MIDDLE EAST LIMITED",
        "OrgFullName": "DP WORLD MIDDLE EAST LIMITED",
        "OrgPayables": false,
        "ZipPostCode": "21474",
        "OrgReceivables": false,
        "OrgActiveClient": true,
        "AddressShortCode": "FRO00021683",
        "OrgOrganizationCode": "FRC00021052"
    }'
WHERE
    id = 926127
    AND tenant_id = 473;

UPDATE shipment_details
SET
    client_dps_address_id = 112492,
    updated_at = NOW()
WHERE
    id = 62010
    AND tenant_id = 473;

-- ========================================
-- 3. FRC00004659 (JEDDAH CABLES COMPANY)
-- Shipments: BOMA25026395, LNZA25063516
-- ========================================

-- Shipment: BOMA25026395
UPDATE parties
SET
    org_code = 'FRC00004659',
    address_code = 'FRO00003875',
    org_id = '63738',
    address_id = '83264',
    updated_at = NOW(),
    org_data = '{
        "Id": 63738,
        "City": "Jeddah",
        "Guid": "114190e1-43ff-49db-9390-e445e37782fc",
        "Email": "ghada.bashmail@cables.energya.com",
        "IsGSA": false,
        "Phone": "126360770",
        "State": "Mekka",
        "label": "JEDDAH CABLES COMPANY",
        "value": "FRC00004659",
        "Source": "CRP",
        "Country": "SAU",
        "Address1": "Industrial City Phase III P.O.BOX 31248",
        "FullName": "JEDDAH CABLES COMPANY",
        "IsActive": 1,
        "Payables": false,
        "TenantId": 442,
        "CompanyId": 372,
        "Consignee": true,
        "Consigner": true,
        "PANNumber": "300192367900003",
        "TaxVendor": false,
        "ExtraParam": "{}",
        "InsertDate": "2024-02-29T14:14:03.000",
        "UpdateDate": "2024-08-29T20:58:36.000",
        "IsSuspended": false,
        "Receivables": true,
        "ZipPostCode": "21497",
        "ActiveClient": true,
        "CurrencyCode": "SAR",
        "EmailInvoice": false,
        "InsertUserId": 1712,
        "UpdateUserId": 1712,
        "VatRegNumber": "300192367900003",
        "ForworderAgent": false,
        "IsCreditEnabled": false,
        "IsV2PaymentTerm": false,
        "OrganizationCode": "FRC00004659",
        "CustomerIdentifier": "1402537_001",
        "ReceivableTermsDate": 2,
        "InsertUserIdUsername": "p100serviceaccountegypt@dpworld.com",
        "OrgQuoteValidityDays": 0,
        "UpdateUserIdUsername": "p100serviceaccountegypt@dpworld.com",
        "CreditLimitOfConfigRow": "",
        "CreditTermsOfConfigRow": "",
        "ImportExportClearanceLock": false
    }',
    address_data = '{
        "Id": 83264,
        "City": "Jeddah",
        "Email": "ghada.bashmail@cables.energya.com",
        "State": "Mekka",
        "label": "FRO00003875",
        "value": "FRO00003875",
        "Country": "SAU",
        "OrgGuid": "114190e1-43ff-49db-9390-e445e37782fc",
        "Address1": "Industrial City Phase III P.O.BOX 31248",
        "OrgSource": "CRP",
        "AddressType": 1,
        "CompanyName": "JEDDAH CABLES COMPANY",
        "OrgFullName": "JEDDAH CABLES COMPANY",
        "OrgPayables": false,
        "ZipPostCode": "21497",
        "TaxRegNumber": "300192367900003",
        "OrgReceivables": true,
        "SiteIdentifier": "1402537_001_FF-SA_001_B",
        "OrgActiveClient": true,
        "AddressShortCode": "FRO00003875",
        "OrgOrganizationCode": "FRC00004659"
    }'
WHERE
    id = 489394
    AND tenant_id = 473;

UPDATE shipment_details
SET
    client_dps_address_id = 112604,
    updated_at = NOW()
WHERE
    id = 37190
    AND tenant_id = 473;

-- Shipment: LNZA25063516
UPDATE parties
SET
    org_code = 'FRC00004659',
    address_code = 'FRO00003875',
    org_id = '63738',
    address_id = '83264',
    updated_at = NOW(),
    org_data = '{
        "Id": 63738,
        "City": "Jeddah",
        "Guid": "114190e1-43ff-49db-9390-e445e37782fc",
        "Email": "ghada.bashmail@cables.energya.com",
        "IsGSA": false,
        "Phone": "126360770",
        "State": "Mekka",
        "label": "JEDDAH CABLES COMPANY",
        "value": "FRC00004659",
        "Source": "CRP",
        "Country": "SAU",
        "Address1": "Industrial City Phase III P.O.BOX 31248",
        "FullName": "JEDDAH CABLES COMPANY",
        "IsActive": 1,
        "Payables": false,
        "TenantId": 442,
        "CompanyId": 372,
        "Consignee": true,
        "Consigner": true,
        "PANNumber": "300192367900003",
        "TaxVendor": false,
        "ExtraParam": "{}",
        "InsertDate": "2024-02-29T14:14:03.000",
        "UpdateDate": "2024-08-29T20:58:36.000",
        "IsSuspended": false,
        "Receivables": true,
        "ZipPostCode": "21497",
        "ActiveClient": true,
        "CurrencyCode": "SAR",
        "EmailInvoice": false,
        "InsertUserId": 1712,
        "UpdateUserId": 1712,
        "VatRegNumber": "300192367900003",
        "ForworderAgent": false,
        "IsCreditEnabled": false,
        "IsV2PaymentTerm": false,
        "OrganizationCode": "FRC00004659",
        "CustomerIdentifier": "1402537_001",
        "ReceivableTermsDate": 2,
        "InsertUserIdUsername": "p100serviceaccountegypt@dpworld.com",
        "OrgQuoteValidityDays": 0,
        "UpdateUserIdUsername": "p100serviceaccountegypt@dpworld.com",
        "CreditLimitOfConfigRow": "",
        "CreditTermsOfConfigRow": "",
        "ImportExportClearanceLock": false
    }',
    address_data = '{
        "Id": 83264,
        "City": "Jeddah",
        "Email": "ghada.bashmail@cables.energya.com",
        "State": "Mekka",
        "label": "FRO00003875",
        "value": "FRO00003875",
        "Country": "SAU",
        "OrgGuid": "114190e1-43ff-49db-9390-e445e37782fc",
        "Address1": "Industrial City Phase III P.O.BOX 31248",
        "OrgSource": "CRP",
        "AddressType": 1,
        "CompanyName": "JEDDAH CABLES COMPANY",
        "OrgFullName": "JEDDAH CABLES COMPANY",
        "OrgPayables": false,
        "ZipPostCode": "21497",
        "TaxRegNumber": "300192367900003",
        "OrgReceivables": true,
        "SiteIdentifier": "1402537_001_FF-SA_001_B",
        "OrgActiveClient": true,
        "AddressShortCode": "FRO00003875",
        "OrgOrganizationCode": "FRC00004659"
    }'
WHERE
    id = 1093918
    AND tenant_id = 473;

UPDATE shipment_details
SET
    client_dps_address_id = 112604,
    updated_at = NOW()
WHERE
    id = 71364
    AND tenant_id = 473;

-- ========================================
-- 4. FRC00008837 (Safana Ideal Trading Company)
-- Shipment: BOMS25037685
-- ========================================

UPDATE parties
SET
    org_code = 'FRC00008837',
    address_code = 'FRO00009492',
    org_id = '142213',
    address_id = '224492',
    updated_at = NOW(),
    org_data = '{
        "Id": 142213,
        "City": "Jeddah",
        "Guid": "959d9058-6176-4f00-a833-3625b74550e3",
        "Email": "inquiry@safanaa.com",
        "IsGSA": false,
        "Phone": "561700313",
        "State": "Mekka",
        "label": "Safana Ideal Trading Company",
        "value": "FRC00008837",
        "Source": "CRP",
        "Country": "SAU",
        "Address1": "Al Mahjar Dist prince Majid street",
        "FullName": "Safana Ideal Trading Company",
        "IsActive": 1,
        "Payables": false,
        "TenantId": 442,
        "CompanyId": 372,
        "Consignee": true,
        "Consigner": true,
        "PANNumber": "311091180300003",
        "TaxVendor": false,
        "ExtraParam": "{}",
        "InsertDate": "2025-02-03T04:37:36.000",
        "UpdateDate": "2025-02-03T04:50:27.000",
        "IsSuspended": false,
        "Receivables": true,
        "ZipPostCode": "22511",
        "ActiveClient": true,
        "CurrencyCode": "SAR",
        "EmailInvoice": false,
        "InsertUserId": 1712,
        "UpdateUserId": 1712,
        "VatRegNumber": "311091180300003",
        "ForworderAgent": false,
        "IsCreditEnabled": false,
        "IsV2PaymentTerm": false,
        "OrganizationCode": "FRC00008837",
        "CustomerIdentifier": "1764892_001",
        "ReceivableTermsDate": 2,
        "InsertUserIdUsername": "p100serviceaccountegypt@dpworld.com",
        "OrgQuoteValidityDays": 0,
        "UpdateUserIdUsername": "p100serviceaccountegypt@dpworld.com",
        "CreditLimitOfConfigRow": "",
        "CreditTermsOfConfigRow": "",
        "IsClientEInvoiceEnabled": false,
        "ImportExportClearanceLock": false
    }',
    address_data = '{
        "Id": 224492,
        "City": "Jeddah",
        "Email": "inquiry@safanaa.com",
        "State": "Mekka",
        "label": "Safana Ideal Trading Company, Al Mahjar Dist prince Majid street, Jeddah, Mekka, SAU",
        "value": "FRO00009492",
        "Country": "SAU",
        "OrgGuid": "959d9058-6176-4f00-a833-3625b74550e3",
        "Address1": "Al Mahjar Dist prince Majid street",
        "OrgSource": "CRP",
        "AddressType": 1,
        "CompanyName": "Safana Ideal Trading Company",
        "OrgFullName": "Safana Ideal Trading Company",
        "OrgPayables": false,
        "ZipPostCode": "22511",
        "TaxRegNumber": "311091180300003",
        "OrgReceivables": true,
        "SiteIdentifier": "1764892_001_FF-SA_001_B",
        "OrgActiveClient": true,
        "AddressShortCode": "FRO00009492",
        "OrgOrganizationCode": "FRC00008837"
    }'
WHERE
    id = 683330
    AND tenant_id = 473;

UPDATE shipment_details
SET
    client_dps_address_id = 115008,
    updated_at = NOW()
WHERE
    id = 48601
    AND tenant_id = 473;

-- ========================================
-- 5. FRC00027912 (Modern Supply Trading Company)
-- Shipment: ANRS25060658
-- ========================================

UPDATE parties
SET
    org_code = 'FRC00027912',
    address_code = 'FRO00028809',
    org_id = '155396',
    address_id = '258853',
    updated_at = NOW(),
    org_data = '{
        "Id": 155396,
        "City": "Jeddah",
        "Guid": "407641f9-0fba-400a-a68c-c9d1a778e0f9",
        "Email": "usaid@amscosal.com",
        "label": "Modern Supply Trading Company",
        "value": "FRC00027912",
        "Country": "SAU",
        "Address1": "Al shehri Street , Andalusia Dist , Ibrahim Al-Juffali, Makkah",
        "FullName": "Modern Supply Trading Company",
        "Payables": false,
        "CompanyId": 372,
        "Receivables": true,
        "ZipPostCode": "37748 23326",
        "ForworderAgent": false,
        "OrganizationCode": "FRC00027912",
        "InsertUserIdUsername": "p100serviceaccountegypt@dpworld.com",
        "UpdateUserIdUsername": "p100serviceaccountegypt@dpworld.com"
    }',
    address_data = '{
        "Id": 258853,
        "City": "Jeddah",
        "Guid": "f8993dbb-0031-4e8b-be04-3049b92add73",
        "Email": "usaid@amscosal.com",
        "Country": "SAU",
        "OrgGuid": "407641f9-0fba-400a-a68c-c9d1a778e0f9",
        "Address1": "Al shehri Street , Andalusia Dist , Ibrahim Al-Juffali, Makkah",
        "OrgSource": "CRP",
        "AddressType": 1,
        "CompanyName": "Modern Supply Trading Company",
        "OrgFullName": "Modern Supply Trading Company",
        "OrgPayables": false,
        "ZipPostCode": "37748 23326",
        "TaxRegNumber": "310415735200003",
        "KnownConsignor": false,
        "OrgReceivables": true,
        "RegulatedAgent": false,
        "SiteIdentifier": "1917750_001_FF-SA_001_B",
        "OrgActiveClient": true,
        "AddressShortCode": "FRO00028809",
        "OrgOrganizationCode": "FRC00027912"
    }'
WHERE
    id = 1067099
    AND tenant_id = 473;

UPDATE shipment_details
SET
    client_dps_address_id = 115009,
    updated_at = NOW()
WHERE
    id = 69870
    AND tenant_id = 473;

-- ========================================
-- 6. FRC00008804 (SAID BAWAZIR TRADING COMPANY)
-- Shipments: PNHS25020014, PNHS25030018, PNHS25060052, PNHS25060054
-- ========================================

-- Shipment: PNHS25020014
UPDATE parties
SET
    org_code = 'FRC00008804',
    address_code = 'FRO00009458',
    org_id = '122306',
    address_id = '153817',
    updated_at = NOW(),
    org_data = '{
        "Id": 122306,
        "City": "JEDDAH",
        "Guid": "7b987649-e881-4ea2-bb5f-fc1c5cc4a819",
        "Email": "stock.control@sbtcgroup.com",
        "IsGSA": false,
        "Phone": "26433857336",
        "State": "Mekka",
        "label": "SAID BAWAZIR TRADING COMPANY (L.L.C)",
        "value": "FRC00008804",
        "Source": "CRP",
        "Country": "SAU",
        "Address1": "SBTC BUILDING 1ST FLOOR, P.O BOX : 11625, West Bugdadiyah - 21463",
        "FullName": "SAID BAWAZIR TRADING COMPANY (L.L.C)",
        "IsActive": 1,
        "Payables": false,
        "TenantId": 442,
        "CompanyId": 372,
        "Consignee": true,
        "Consigner": true,
        "PANNumber": "310596981900003",
        "TaxVendor": false,
        "ExtraParam": "{}",
        "InsertDate": "2024-06-02T19:27:22.000",
        "UpdateDate": "2025-02-05T22:33:01.000",
        "IsSuspended": false,
        "Receivables": true,
        "ZipPostCode": "22232",
        "ActiveClient": true,
        "CurrencyCode": "SAR",
        "EmailInvoice": false,
        "InsertUserId": 1712,
        "UpdateUserId": 1712,
        "VatRegNumber": "310596981900003",
        "ForworderAgent": false,
        "IsCreditEnabled": true,
        "IsV2PaymentTerm": false,
        "OrganizationCode": "FRC00008804",
        "EnableBulkInvoice": false,
        "CustomerIdentifier": "1475011_001",
        "ReceivableTermsDate": 2,
        "InsertUserIdUsername": "p100serviceaccountegypt@dpworld.com",
        "OrgQuoteValidityDays": 0,
        "UpdateUserIdUsername": "p100serviceaccountegypt@dpworld.com",
        "CreditLimitOfConfigRow": "0",
        "CreditTermsOfConfigRow": "30 days from Invoice Date",
        "IsClientEInvoiceEnabled": false,
        "ImportExportClearanceLock": false
    }',
    address_data = '{
        "Id": 153817,
        "City": "JEDDAH",
        "Email": "stock.control@sbtcgroup.com",
        "State": "Mekka",
        "label": "FRO00009458",
        "value": "FRO00009458",
        "Country": "SAU",
        "OrgGuid": "7b987649-e881-4ea2-bb5f-fc1c5cc4a819",
        "Address1": "SBTC BUILDING 1ST FLOOR, P.O BOX : 11625, West Bugdadiyah - 21463",
        "OrgSource": "CRP",
        "AddressType": 2,
        "CompanyName": "SAID BAWAZIR TRADING COM",
        "OrgFullName": "SAID BAWAZIR TRADING COMPANY (L.L.C)",
        "OrgPayables": false,
        "ZipPostCode": "22232",
        "TaxRegNumber": "310596981900003",
        "OrgReceivables": true,
        "OrgActiveClient": true,
        "AddressShortCode": "FRO00009458",
        "OrgOrganizationCode": "FRC00008804"
    }'
WHERE
    id = 489379
    AND tenant_id = 473;

UPDATE shipment_details
SET
    client_dps_address_id = 115016,
    updated_at = NOW()
WHERE
    id = 37188
    AND tenant_id = 473;

-- Shipment: PNHS25030018
UPDATE parties
SET
    org_code = 'FRC00008804',
    address_code = 'FRO00009458',
    org_id = '122306',
    address_id = '153817',
    updated_at = NOW(),
    org_data = '{
        "Id": 122306,
        "City": "JEDDAH",
        "Guid": "7b987649-e881-4ea2-bb5f-fc1c5cc4a819",
        "Email": "stock.control@sbtcgroup.com",
        "IsGSA": false,
        "Phone": "26433857336",
        "State": "Mekka",
        "label": "SAID BAWAZIR TRADING COMPANY (L.L.C)",
        "value": "FRC00008804",
        "Source": "CRP",
        "Country": "SAU",
        "Address1": "SBTC BUILDING 1ST FLOOR, P.O BOX : 11625, West Bugdadiyah - 21463",
        "FullName": "SAID BAWAZIR TRADING COMPANY (L.L.C)",
        "IsActive": 1,
        "Payables": false,
        "TenantId": 442,
        "CompanyId": 372,
        "Consignee": true,
        "Consigner": true,
        "PANNumber": "310596981900003",
        "TaxVendor": false,
        "ExtraParam": "{}",
        "InsertDate": "2024-06-02T19:27:22.000",
        "UpdateDate": "2025-02-05T22:33:01.000",
        "IsSuspended": false,
        "Receivables": true,
        "ZipPostCode": "22232",
        "ActiveClient": true,
        "CurrencyCode": "SAR",
        "EmailInvoice": false,
        "InsertUserId": 1712,
        "UpdateUserId": 1712,
        "VatRegNumber": "310596981900003",
        "ForworderAgent": false,
        "IsCreditEnabled": true,
        "IsV2PaymentTerm": false,
        "OrganizationCode": "FRC00008804",
        "EnableBulkInvoice": false,
        "CustomerIdentifier": "1475011_001",
        "ReceivableTermsDate": 2,
        "InsertUserIdUsername": "p100serviceaccountegypt@dpworld.com",
        "OrgQuoteValidityDays": 0,
        "UpdateUserIdUsername": "p100serviceaccountegypt@dpworld.com",
        "CreditLimitOfConfigRow": "0",
        "CreditTermsOfConfigRow": "30 days from Invoice Date",
        "IsClientEInvoiceEnabled": false,
        "ImportExportClearanceLock": false
    }',
    address_data = '{
        "Id": 153817,
        "City": "JEDDAH",
        "Email": "stock.control@sbtcgroup.com",
        "State": "Mekka",
        "label": "FRO00009458",
        "value": "FRO00009458",
        "Country": "SAU",
        "OrgGuid": "7b987649-e881-4ea2-bb5f-fc1c5cc4a819",
        "Address1": "SBTC BUILDING 1ST FLOOR, P.O BOX : 11625, West Bugdadiyah - 21463",
        "OrgSource": "CRP",
        "AddressType": 2,
        "CompanyName": "SAID BAWAZIR TRADING COM",
        "OrgFullName": "SAID BAWAZIR TRADING COMPANY (L.L.C)",
        "OrgPayables": false,
        "ZipPostCode": "22232",
        "TaxRegNumber": "310596981900003",
        "OrgReceivables": true,
        "OrgActiveClient": true,
        "AddressShortCode": "FRO00009458",
        "OrgOrganizationCode": "FRC00008804"
    }'
WHERE
    id = 561045
    AND tenant_id = 473;

UPDATE shipment_details
SET
    client_dps_address_id = 115016,
    updated_at = NOW()
WHERE
    id = 41395
    AND tenant_id = 473;

-- Shipment: PNHS25060052
UPDATE parties
SET
    org_code = 'FRC00008804',
    address_code = 'FRO00009458',
    org_id = '122306',
    address_id = '153817',
    updated_at = NOW(),
    org_data = '{
        "Id": 122306,
        "City": "JEDDAH",
        "Guid": "7b987649-e881-4ea2-bb5f-fc1c5cc4a819",
        "Email": "stock.control@sbtcgroup.com",
        "IsGSA": false,
        "Phone": "26433857336",
        "State": "Mekka",
        "label": "SAID BAWAZIR TRADING COMPANY (L.L.C)",
        "value": "FRC00008804",
        "Source": "CRP",
        "Country": "SAU",
        "Address1": "SBTC BUILDING 1ST FLOOR, P.O BOX : 11625, West Bugdadiyah - 21463",
        "FullName": "SAID BAWAZIR TRADING COMPANY (L.L.C)",
        "IsActive": 1,
        "Payables": false,
        "TenantId": 442,
        "CompanyId": 372,
        "Consignee": true,
        "Consigner": true,
        "PANNumber": "310596981900003",
        "TaxVendor": false,
        "ExtraParam": "{}",
        "InsertDate": "2024-06-02T19:27:22.000",
        "UpdateDate": "2025-02-05T22:33:01.000",
        "IsSuspended": false,
        "Receivables": true,
        "ZipPostCode": "22232",
        "ActiveClient": true,
        "CurrencyCode": "SAR",
        "EmailInvoice": false,
        "InsertUserId": 1712,
        "UpdateUserId": 1712,
        "VatRegNumber": "310596981900003",
        "ForworderAgent": false,
        "IsCreditEnabled": true,
        "IsV2PaymentTerm": false,
        "OrganizationCode": "FRC00008804",
        "EnableBulkInvoice": false,
        "CustomerIdentifier": "1475011_001",
        "ReceivableTermsDate": 2,
        "InsertUserIdUsername": "p100serviceaccountegypt@dpworld.com",
        "OrgQuoteValidityDays": 0,
        "UpdateUserIdUsername": "p100serviceaccountegypt@dpworld.com",
        "CreditLimitOfConfigRow": "0",
        "CreditTermsOfConfigRow": "30 days from Invoice Date",
        "IsClientEInvoiceEnabled": false,
        "ImportExportClearanceLock": false
    }',
    address_data = '{
        "Id": 153817,
        "City": "JEDDAH",
        "Email": "stock.control@sbtcgroup.com",
        "State": "Mekka",
        "label": "FRO00009458",
        "value": "FRO00009458",
        "Country": "SAU",
        "OrgGuid": "7b987649-e881-4ea2-bb5f-fc1c5cc4a819",
        "Address1": "SBTC BUILDING 1ST FLOOR, P.O BOX : 11625, West Bugdadiyah - 21463",
        "OrgSource": "CRP",
        "AddressType": 2,
        "CompanyName": "SAID BAWAZIR TRADING COM",
        "OrgFullName": "SAID BAWAZIR TRADING COMPANY (L.L.C)",
        "OrgPayables": false,
        "ZipPostCode": "22232",
        "TaxRegNumber": "310596981900003",
        "OrgReceivables": true,
        "OrgActiveClient": true,
        "AddressShortCode": "FRO00009458",
        "OrgOrganizationCode": "FRC00008804"
    }'
WHERE
    id = 1066646
    AND tenant_id = 473;

UPDATE shipment_details
SET
    client_dps_address_id = 115016,
    updated_at = NOW()
WHERE
    id = 69842
    AND tenant_id = 473;

-- Shipment: PNHS25060054
UPDATE parties
SET
    org_code = 'FRC00008804',
    address_code = 'FRO00009458',
    org_id = '122306',
    address_id = '153817',
    updated_at = NOW(),
    org_data = '{
        "Id": 122306,
        "City": "JEDDAH",
        "Guid": "7b987649-e881-4ea2-bb5f-fc1c5cc4a819",
        "Email": "stock.control@sbtcgroup.com",
        "IsGSA": false,
        "Phone": "26433857336",
        "State": "Mekka",
        "label": "SAID BAWAZIR TRADING COMPANY (L.L.C)",
        "value": "FRC00008804",
        "Source": "CRP",
        "Country": "SAU",
        "Address1": "SBTC BUILDING 1ST FLOOR, P.O BOX : 11625, West Bugdadiyah - 21463",
        "FullName": "SAID BAWAZIR TRADING COMPANY (L.L.C)",
        "IsActive": 1,
        "Payables": false,
        "TenantId": 442,
        "CompanyId": 372,
        "Consignee": true,
        "Consigner": true,
        "PANNumber": "310596981900003",
        "TaxVendor": false,
        "ExtraParam": "{}",
        "InsertDate": "2024-06-02T19:27:22.000",
        "UpdateDate": "2025-02-05T22:33:01.000",
        "IsSuspended": false,
        "Receivables": true,
        "ZipPostCode": "22232",
        "ActiveClient": true,
        "CurrencyCode": "SAR",
        "EmailInvoice": false,
        "InsertUserId": 1712,
        "UpdateUserId": 1712,
        "VatRegNumber": "310596981900003",
        "ForworderAgent": false,
        "IsCreditEnabled": true,
        "IsV2PaymentTerm": false,
        "OrganizationCode": "FRC00008804",
        "EnableBulkInvoice": false,
        "CustomerIdentifier": "1475011_001",
        "ReceivableTermsDate": 2,
        "InsertUserIdUsername": "p100serviceaccountegypt@dpworld.com",
        "OrgQuoteValidityDays": 0,
        "UpdateUserIdUsername": "p100serviceaccountegypt@dpworld.com",
        "CreditLimitOfConfigRow": "0",
        "CreditTermsOfConfigRow": "30 days from Invoice Date",
        "IsClientEInvoiceEnabled": false,
        "ImportExportClearanceLock": false
    }',
    address_data = '{
        "Id": 153817,
        "City": "JEDDAH",
        "Email": "stock.control@sbtcgroup.com",
        "State": "Mekka",
        "label": "FRO00009458",
        "value": "FRO00009458",
        "Country": "SAU",
        "OrgGuid": "7b987649-e881-4ea2-bb5f-fc1c5cc4a819",
        "Address1": "SBTC BUILDING 1ST FLOOR, P.O BOX : 11625, West Bugdadiyah - 21463",
        "OrgSource": "CRP",
        "AddressType": 2,
        "CompanyName": "SAID BAWAZIR TRADING COM",
        "OrgFullName": "SAID BAWAZIR TRADING COMPANY (L.L.C)",
        "OrgPayables": false,
        "ZipPostCode": "22232",
        "TaxRegNumber": "310596981900003",
        "OrgReceivables": true,
        "OrgActiveClient": true,
        "AddressShortCode": "FRO00009458",
        "OrgOrganizationCode": "FRC00008804"
    }'
WHERE
    id = 1067022
    AND tenant_id = 473;

UPDATE shipment_details
SET
    client_dps_address_id = 115016,
    updated_at = NOW()
WHERE
    id = 69865
    AND tenant_id = 473;

-- ========================================
-- 7. FRC00027628 (SAUDI GOODY PRODUCTS MARKETING COMPANY LTD)
-- Shipment: SUBS25040139
-- ========================================

UPDATE parties
SET
    org_code = 'FRC00027628',
    address_code = 'FRO00028515',
    org_id = '143758',
    address_id = '229754',
    updated_at = NOW(),
    org_data = '{
        "Id": 143758,
        "City": "JEDDAH",
        "Guid": "2f1716f6-213c-471c-8140-f62e32902cb4",
        "Email": "info@saudigoody.com",
        "IsGSA": false,
        "Phone": "119999999999",
        "State": "Mekka",
        "label": "SAUDI GOODY PRODUCTS MARKETING COMPANY LTD",
        "value": "FRC00027628",
        "Broker": false,
        "Source": "CRP",
        "Carrier": false,
        "Country": "SAU",
        "Address1": "UNIT #1 AD DAWSARI AL ANDALUS,PO BOX 3813 JEDDAH",
        "Address2": "",
        "FullName": "SAUDI GOODY PRODUCTS MARKETING COMPANY LTD",
        "IsActive": 1,
        "Payables": false,
        "Services": false,
        "TenantId": 442,
        "CompanyId": 372,
        "Consignee": true,
        "Consigner": true,
        "IsParnter": false,
        "PANNumber": "Dummy123",
        "TaxVendor": false,
        "WareHouse": false,
        "AirCarrier": false,
        "ExtraParam": "",
        "InsertDate": "2025-02-21T11:53:55.000",
        "SeaCarrier": false,
        "UpdateDate": "2025-02-21T11:53:55.000",
        "IsSuspended": false,
        "RailCarrier": false,
        "Receivables": false,
        "RoadCarrier": false,
        "ZipPostCode": "23326",
        "ActiveClient": true,
        "CurrencyCode": "SAR",
        "EmailInvoice": false,
        "InsertUserId": 1712,
        "UpdateUserId": 1712,
        "ForworderAgent": false,
        "IsCreditEnabled": false,
        "IsV2PaymentTerm": false,
        "TransportClient": false,
        "IsWalkInCustomer": false,
        "OrganizationCode": "FRC00027628",
        "EnableBulkInvoice": false,
        "InsertUserIdUsername": "p100serviceaccountegypt@dpworld.com",
        "UpdateUserIdUsername": "p100serviceaccountegypt@dpworld.com",
        "IsClientEInvoiceEnabled": false,
        "ImportExportClearanceLock": false,
        "FusionCreditlimitOverrideApproved": false
    }',
    address_data = '{
        "Id": 229754,
        "City": "JEDDAH",
        "Email": "info@saudigoody.com",
        "State": "Mekka",
        "label": "FRO00028515",
        "value": "FRO00028515",
        "Country": "SAU",
        "OrgGuid": "2f1716f6-213c-471c-8140-f62e32902cb4",
        "Address1": "UNIT #1 AD DAWSARI AL ANDALUS,PO BOX 3813 JEDDAH",
        "OrgSource": "CRP",
        "AddressType": 2,
        "CompanyName": "SAUDI GOODY PRODUCTS MARKETING COMPANY LTD",
        "OrgFullName": "SAUDI GOODY PRODUCTS MARKETING COMPANY LTD",
        "OrgPayables": false,
        "ZipPostCode": "23326",
        "OrgReceivables": false,
        "OrgActiveClient": true,
        "AddressShortCode": "FRO00028515",
        "OrgOrganizationCode": "FRC00027628"
    }'
WHERE
    id = 841435
    AND tenant_id = 473;

UPDATE shipment_details
SET
    client_dps_address_id = 115010,
    updated_at = NOW()
WHERE
    id = 57357
    AND tenant_id = 473;

-- ========================================
-- 8. FRC00008918 (ZAMIL GROUP TRADE & SERVICES CO. LTD.)
-- Shipments: LNZA25032295, LNZA25052861, ANRS25070741, ANRS25070731
-- ========================================

-- Shipment: LNZA25032295
UPDATE parties
SET
    org_code = 'FRC00008918',
    address_code = 'FRO00009576',
    org_id = '122187',
    address_id = '153587',
    updated_at = NOW(),
    org_data = '{
        "Id": 122187,
        "City": "DAMMAM",
        "Guid": "af5b1b24-ba79-4900-856a-e4f152be50f2",
        "Email": "Sayed.Zabiullah@zamilts.com",
        "State": "Eastern Province",
        "label": "ZAMIL GROUP TRADE & SERVICES CO. LTD.",
        "value": "FRC00008918",
        "Country": "SAU",
        "Address1": "P.O.BOX 13793, 1ST INDL. CITY 42ND ST., 15TH CROSS",
        "FullName": "ZAMIL GROUP TRADE & SERVICES CO. LTD.",
        "Payables": false,
        "CompanyId": 372,
        "Receivables": true,
        "ZipPostCode": "31414",
        "ForworderAgent": false,
        "OrganizationCode": "FRC00008918",
        "InsertUserIdUsername": "p100serviceaccountegypt@dpworld.com",
        "UpdateUserIdUsername": "p100serviceaccountegypt@dpworld.com"
    }',
    address_data = '{
        "Id": 153587,
        "City": "DAMMAM",
        "Guid": "3ebd32d2-11fc-436a-b853-c66efb74b102",
        "Email": "Sayed.Zabiullah@zamilts.com",
        "State": "Eastern Province",
        "label": "ZAMIL GROUP TRADE & SERVICES CO. LTD.\nP.O.BOX 13793, 1ST INDL. CITY 42ND ST., 15TH CROSS, DAMMAM, Eastern Province\nSAU, 31414\n",
        "value": "FRO00009576",
        "Country": "SAU",
        "OrgGuid": "af5b1b24-ba79-4900-856a-e4f152be50f2",
        "Address1": "P.O.BOX 13793, 1ST INDL. CITY 42ND ST., 15TH CROSS",
        "OrgSource": "CRP",
        "AddressType": 2,
        "CompanyName": "ZAMIL GROUP TRADE & SERVICES CO. LTD.",
        "OrgFullName": "ZAMIL GROUP TRADE & SERVICES CO. LTD.",
        "OrgPayables": false,
        "ZipPostCode": "31414",
        "TaxRegNumber": "310087999400003",
        "OrgReceivables": true,
        "OrgActiveClient": true,
        "AddressShortCode": "FRO00009576",
        "OrgOrganizationCode": "FRC00008918"
    }'
WHERE
    id = 635206
    AND tenant_id = 473;

UPDATE shipment_details
SET
    client_dps_address_id = 114999,
    updated_at = NOW()
WHERE
    id = 45800
    AND tenant_id = 473;

-- Shipment: LNZA25052861
UPDATE parties
SET
    org_code = 'FRC00008918',
    address_code = 'FRO00009576',
    org_id = '122187',
    address_id = '153587',
    updated_at = NOW(),
    org_data = '{
        "Id": 122187,
        "City": "DAMMAM",
        "Guid": "af5b1b24-ba79-4900-856a-e4f152be50f2",
        "Email": "Sayed.Zabiullah@zamilts.com",
        "State": "Eastern Province",
        "label": "ZAMIL GROUP TRADE & SERVICES CO. LTD.",
        "value": "FRC00008918",
        "Country": "SAU",
        "Address1": "P.O.BOX 13793, 1ST INDL. CITY 42ND ST., 15TH CROSS",
        "FullName": "ZAMIL GROUP TRADE & SERVICES CO. LTD.",
        "Payables": false,
        "CompanyId": 372,
        "Receivables": true,
        "ZipPostCode": "31414",
        "ForworderAgent": false,
        "OrganizationCode": "FRC00008918",
        "InsertUserIdUsername": "p100serviceaccountegypt@dpworld.com",
        "UpdateUserIdUsername": "p100serviceaccountegypt@dpworld.com"
    }',
    address_data = '{
        "Id": 153587,
        "City": "DAMMAM",
        "Guid": "3ebd32d2-11fc-436a-b853-c66efb74b102",
        "Email": "Sayed.Zabiullah@zamilts.com",
        "State": "Eastern Province",
        "label": "ZAMIL GROUP TRADE & SERVICES CO. LTD.\nP.O.BOX 13793, 1ST INDL. CITY 42ND ST., 15TH CROSS, DAMMAM, Eastern Province\nSAU, 31414\n",
        "value": "FRO00009576",
        "Country": "SAU",
        "OrgGuid": "af5b1b24-ba79-4900-856a-e4f152be50f2",
        "Address1": "P.O.BOX 13793, 1ST INDL. CITY 42ND ST., 15TH CROSS",
        "OrgSource": "CRP",
        "AddressType": 2,
        "CompanyName": "ZAMIL GROUP TRADE & SERVICES CO. LTD.",
        "OrgFullName": "ZAMIL GROUP TRADE & SERVICES CO. LTD.",
        "OrgPayables": false,
        "ZipPostCode": "31414",
        "TaxRegNumber": "310087999400003",
        "OrgReceivables": true,
        "OrgActiveClient": true,
        "AddressShortCode": "FRO00009576",
        "OrgOrganizationCode": "FRC00008918"
    }'
WHERE
    id = 841346
    AND tenant_id = 473;

UPDATE shipment_details
SET
    client_dps_address_id = 114999,
    updated_at = NOW()
WHERE
    id = 57352
    AND tenant_id = 473;

-- Shipment: ANRS25070741
UPDATE parties
SET
    org_code = 'FRC00008918',
    address_code = 'FRO00009576',
    org_id = '122187',
    address_id = '153587',
    updated_at = NOW(),
    org_data = '{
        "Id": 122187,
        "City": "DAMMAM",
        "Guid": "af5b1b24-ba79-4900-856a-e4f152be50f2",
        "Email": "Sayed.Zabiullah@zamilts.com",
        "State": "Eastern Province",
        "label": "ZAMIL GROUP TRADE & SERVICES CO. LTD.",
        "value": "FRC00008918",
        "Country": "SAU",
        "Address1": "P.O.BOX 13793, 1ST INDL. CITY 42ND ST., 15TH CROSS",
        "FullName": "ZAMIL GROUP TRADE & SERVICES CO. LTD.",
        "Payables": false,
        "CompanyId": 372,
        "Receivables": true,
        "ZipPostCode": "31414",
        "ForworderAgent": false,
        "OrganizationCode": "FRC00008918",
        "InsertUserIdUsername": "p100serviceaccountegypt@dpworld.com",
        "UpdateUserIdUsername": "p100serviceaccountegypt@dpworld.com"
    }',
    address_data = '{
        "Id": 153587,
        "City": "DAMMAM",
        "Guid": "3ebd32d2-11fc-436a-b853-c66efb74b102",
        "Email": "Sayed.Zabiullah@zamilts.com",
        "State": "Eastern Province",
        "label": "ZAMIL GROUP TRADE & SERVICES CO. LTD.\nP.O.BOX 13793, 1ST INDL. CITY 42ND ST., 15TH CROSS, DAMMAM, Eastern Province\nSAU, 31414\n",
        "value": "FRO00009576",
        "Country": "SAU",
        "OrgGuid": "af5b1b24-ba79-4900-856a-e4f152be50f2",
        "Address1": "P.O.BOX 13793, 1ST INDL. CITY 42ND ST., 15TH CROSS",
        "OrgSource": "CRP",
        "AddressType": 2,
        "CompanyName": "ZAMIL GROUP TRADE & SERVICES CO. LTD.",
        "OrgFullName": "ZAMIL GROUP TRADE & SERVICES CO. LTD.",
        "OrgPayables": false,
        "ZipPostCode": "31414",
        "TaxRegNumber": "310087999400003",
        "OrgReceivables": true,
        "OrgActiveClient": true,
        "AddressShortCode": "FRO00009576",
        "OrgOrganizationCode": "FRC00008918"
    }'
WHERE
    id = 1410500
    AND tenant_id = 473;

UPDATE shipment_details
SET
    client_dps_address_id = 114999,
    updated_at = NOW()
WHERE
    id = 89356
    AND tenant_id = 473;

-- Shipment: ANRS25070731
UPDATE parties
SET
    org_code = 'FRC00008918',
    address_code = 'FRO00009576',
    org_id = '122187',
    address_id = '153587',
    updated_at = NOW(),
    org_data = '{
        "Id": 122187,
        "City": "DAMMAM",
        "Guid": "af5b1b24-ba79-4900-856a-e4f152be50f2",
        "Email": "Sayed.Zabiullah@zamilts.com",
        "State": "Eastern Province",
        "label": "ZAMIL GROUP TRADE & SERVICES CO. LTD.",
        "value": "FRC00008918",
        "Country": "SAU",
        "Address1": "P.O.BOX 13793, 1ST INDL. CITY 42ND ST., 15TH CROSS",
        "FullName": "ZAMIL GROUP TRADE & SERVICES CO. LTD.",
        "Payables": false,
        "CompanyId": 372,
        "Receivables": true,
        "ZipPostCode": "31414",
        "ForworderAgent": false,
        "OrganizationCode": "FRC00008918",
        "InsertUserIdUsername": "p100serviceaccountegypt@dpworld.com",
        "UpdateUserIdUsername": "p100serviceaccountegypt@dpworld.com"
    }',
    address_data = '{
        "Id": 153587,
        "City": "DAMMAM",
        "Guid": "3ebd32d2-11fc-436a-b853-c66efb74b102",
        "Email": "Sayed.Zabiullah@zamilts.com",
        "State": "Eastern Province",
        "label": "ZAMIL GROUP TRADE & SERVICES CO. LTD.\nP.O.BOX 13793, 1ST INDL. CITY 42ND ST., 15TH CROSS, DAMMAM, Eastern Province\nSAU, 31414\n",
        "value": "FRO00009576",
        "Country": "SAU",
        "OrgGuid": "af5b1b24-ba79-4900-856a-e4f152be50f2",
        "Address1": "P.O.BOX 13793, 1ST INDL. CITY 42ND ST., 15TH CROSS",
        "OrgSource": "CRP",
        "AddressType": 2,
        "CompanyName": "ZAMIL GROUP TRADE & SERVICES CO. LTD.",
        "OrgFullName": "ZAMIL GROUP TRADE & SERVICES CO. LTD.",
        "OrgPayables": false,
        "ZipPostCode": "31414",
        "TaxRegNumber": "310087999400003",
        "OrgReceivables": true,
        "OrgActiveClient": true,
        "AddressShortCode": "FRO00009576",
        "OrgOrganizationCode": "FRC00008918"
    }'
WHERE
    id = 1499060
    AND tenant_id = 473;

UPDATE shipment_details
SET
    client_dps_address_id = 114999,
    updated_at = NOW()
WHERE
    id = 94511
    AND tenant_id = 473;

