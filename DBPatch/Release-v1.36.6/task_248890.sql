-- Query 1: Update for FRC00008918 / FRO00009576
--Party Id : 1252928
-- Shipmen Id : 80434
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
    id = 1252928 and tenant_id = 482;

update
    shipment_details
set
    client_dps_address_id = 103611,
    updated_at = NOW()
where
    id = 80434 and tenant_id = 482;

-- Query 2: Update for FRC00008918 / FRO00009576
-- Party Id : 1273230
-- Shipment Id : 81642
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
    id = 1273230 and tenant_id = 482;

update
    shipment_details
set
    client_dps_address_id = 103611,
    updated_at = NOW()
where
    id = 81642 and tenant_id = 482;