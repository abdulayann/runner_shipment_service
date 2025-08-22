-- Query 1: Update for FRC00027912 / FRO00028809 (Modern Supply Trading Company)
-- Shipment: ANRS25070759
update
    parties
set
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
      "IsGSA": false,
      "Phone": "0550836060",
      "label": "Modern Supply Trading Company",
      "value": "FRC00027912",
      "Broker": false,
      "Source": "CRP",
      "Carrier": false,
      "Country": "SAU",
      "Address1": "Al shehri Street , Andalusia Dist , Ibrahim Al-Juffali, Makkah",
      "FullName": "Modern Supply Trading Company",
      "IsActive": 1,
      "Payables": false,
      "Services": false,
      "TenantId": 442,
      "CompanyId": 372,
      "Consignee": true,
      "Consigner": true,
      "IsParnter": false,
      "PANNumber": "310415735200003",
      "TaxVendor": false,
      "WareHouse": false,
      "AirCarrier": false,
      "ExtraParam": "",
      "InsertDate": "2025-06-18T17:17:15.000",
      "SeaCarrier": false,
      "UpdateDate": "2025-07-08T10:40:48.000",
      "IsSuspended": false,
      "RailCarrier": false,
      "Receivables": true,
      "RoadCarrier": false,
      "ZipPostCode": "37748 23326",
      "ActiveClient": true,
      "CurrencyCode": "SAR",
      "EmailInvoice": false,
      "InsertUserId": 1712,
      "UpdateUserId": 1712,
      "VatRegNumber": "310415735200003",
      "ForworderAgent": false,
      "IsCreditEnabled": false,
      "IsV2PaymentTerm": false,
      "TransportClient": false,
      "IsWalkInCustomer": false,
      "OrganizationCode": "FRC00027912",
      "EnableBulkInvoice": false,
      "CustomerIdentifier": "1917750_001",
      "ReceivableTermsDate": 2,
      "InsertUserIdUsername": "p100serviceaccountegypt@dpworld.com",
      "UpdateUserIdUsername": "p100serviceaccountegypt@dpworld.com",
      "IsClientEInvoiceEnabled": false,
      "ImportExportClearanceLock": false,
      "FusionCreditlimitOverrideApproved": false
    }',
    address_data = '{
      "Id": 258853,
      "City": "Jeddah",
      "Email": "usaid@amscosal.com",
      "label": "FRO00028809",
      "value": "FRO00028809",
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
      "OrgReceivables": true,
      "SiteIdentifier": "1917750_001_FF-BE_001_B",
      "OrgActiveClient": true,
      "AddressShortCode": "FRO00028809",
      "OrgOrganizationCode": "FRC00027912"
    }'
where
    id = (select client_id from shipment_details  where shipment_id='ANRS25070759' and tenant_id = 442);

update
    shipment_details
set
    client_dps_address_id = 104019,
    updated_at = NOW()
where
    shipment_id='ANRS25070759' and tenant_id = 442; -- Replace with actual shipment details ID

-- Query 2: Update for FRC00008918 / FRO00009576 (ZAMIL GROUP TRADE & SERVICES CO. LTD.)
-- Shipment: ANRS25070741
update
    parties
set
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
      "IsGSA": false,
      "Phone": "138924910",
      "State": "Eastern Province",
      "label": "ZAMIL GROUP TRADE & SERVICES CO. LTD.",
      "value": "FRC00008918",
      "Source": "CRP",
      "Country": "SAU",
      "Address1": "P.O.BOX 13793, 1ST INDL. CITY 42ND ST., 15TH CROSS",
      "FullName": "ZAMIL GROUP TRADE & SERVICES CO. LTD.",
      "IsActive": 1,
      "Payables": false,
      "TenantId": 442,
      "CompanyId": 372,
      "Consignee": true,
      "Consigner": true,
      "PANNumber": "310087999400003",
      "TaxVendor": false,
      "ExtraParam": "{}",
      "InsertDate": "2024-05-30T00:44:39.000",
      "UpdateDate": "2024-09-13T07:56:41.000",
      "IsSuspended": false,
      "Receivables": true,
      "ZipPostCode": "31414",
      "ActiveClient": true,
      "CurrencyCode": "SAR",
      "EmailInvoice": false,
      "InsertUserId": 1712,
      "UpdateUserId": 1712,
      "VatRegNumber": "310087999400003",
      "ForworderAgent": false,
      "IsCreditEnabled": false,
      "IsV2PaymentTerm": false,
      "OrganizationCode": "FRC00008918",
      "CustomerIdentifier": "1551010_001",
      "InsertUserIdUsername": "p100serviceaccountegypt@dpworld.com",
      "OrgQuoteValidityDays": 0,
      "UpdateUserIdUsername": "p100serviceaccountegypt@dpworld.com",
      "CreditLimitOfConfigRow": "",
      "CreditTermsOfConfigRow": "",
      "ImportExportClearanceLock": false
    }',
    address_data = '{
      "Id": 153587,
      "City": "DAMMAM",
      "Email": "Sayed.Zabiullah@zamilts.com",
      "State": "Eastern Province",
      "label": "FRO00009576",
      "value": "FRO00009576",
      "Country": "SAU",
      "OrgGuid": "af5b1b24-ba79-4900-856a-e4f152be50f2",
      "Address1": "P.O.BOX 13793, 1ST INDL. CITY 42ND ST., 15TH CROSS",
      "OrgSource": "CRP",
      "AddressType": 1,
      "CompanyName": "ZAMIL GROUP TRADE & SERVICES CO. LTD.",
      "OrgFullName": "ZAMIL GROUP TRADE & SERVICES CO. LTD.",
      "OrgPayables": false,
      "ZipPostCode": "31414",
      "TaxRegNumber": "310087999400003",
      "OrgReceivables": true,
      "SiteIdentifier": "1551010_001_FF-SA_001_B",
      "OrgActiveClient": true,
      "AddressShortCode": "FRO00009576",
      "OrgOrganizationCode": "FRC00008918"
    }'
where
    id = (select client_id from shipment_details  where shipment_id='ANRS25070741' and tenant_id = 442);

update
    shipment_details
set
    client_dps_address_id = 104020,
    updated_at = NOW()
where
    shipment_id='ANRS25070741' and tenant_id = 442;