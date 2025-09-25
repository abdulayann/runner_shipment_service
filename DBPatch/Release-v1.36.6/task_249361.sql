UPDATE parties
SET
    org_code = 'FRC00012937',
    address_code = 'FRO00013239',
    org_id = '127952',
    address_id = '181256',
    updated_at = NOW(),
    org_data = '{
                    "Id": 127952,
                    "City": "WEIFANG",
                    "Guid": "be71fc46-ccfe-456e-896b-29a74583b85d",
                    "Email": "hebe@ghlibo.com",
                    "IsGSA": false,
                    "Phone": "155889077371",
                    "State": "Shandong",
                    "label": "Weifang guanghua fine chemical co.,ltd",
                    "value": "FRC00012937",
                    "Source": "CRP",
                    "Country": "CHN",
                    "Address1": "NO.399 XINHAI STREET, BINHAI WEIFANG SHANDONG 261000 CHINA",
                    "FullName": "Weifang guanghua fine chemical co.,ltd",
                    "IsActive": 1,
                    "Payables": false,
                    "TenantId": 442,
                    "CompanyId": 372,
                    "Consignee": true,
                    "Consigner": true,
                    "PANNumber": "91300700MA3BX83Y18",
                    "TaxVendor": false,
                    "ExtraParam": "{}",
                    "InsertDate": "2024-08-13T20:44:25.000",
                    "UpdateDate": "2024-08-13T22:11:39.000",
                    "IsSuspended": false,
                    "Receivables": true,
                    "ZipPostCode": "261000",
                    "ActiveClient": true,
                    "CurrencyCode": "CNY",
                    "EmailInvoice": false,
                    "InsertUserId": 1712,
                    "UpdateUserId": 1712,
                    "VatRegNumber": "91300700MA3BX83Y18",
                    "ForworderAgent": false,
                    "IsCreditEnabled": false,
                    "IsV2PaymentTerm": false,
                    "OrganizationCode": "FRC00012937",
                    "CustomerIdentifier": "1537118_001",
                    "ReceivableTermsDate": 2,
                    "InsertUserIdUsername": "p100serviceaccountegypt@dpworld.com",
                    "OrgQuoteValidityDays": 0,
                    "UpdateUserIdUsername": "p100serviceaccountegypt@dpworld.com",
                    "CreditLimitOfConfigRow": "",
                    "CreditTermsOfConfigRow": "",
                    "ImportExportClearanceLock": false
    }',
    address_data = '{
        "Id": 181256,
        "City": "WEIFANG",
        "Email": "hebe@ghlibo.com",
        "State": "Shandong",
        "label": "FRO00013239",
        "value": "FRO00013239",
        "Country": "CHN",
        "OrgGuid": "be71fc46-ccfe-456e-896b-29a74583b85d",
        "Address1": "NO.399 XINHAI STREET, BINHAI WEIFANG SHANDONG 261000 CHINA",
        "OrgSource": "CRP",
        "AddressType": 1,
        "CompanyName": "Weifang guanghua fine chemical co.,ltd",
        "OrgFullName": "Weifang guanghua fine chemical co.,ltd",
        "OrgPayables": false,
        "ZipPostCode": "261000",
        "TaxRegNumber": "91300700MA3BX83Y18",
        "OrgReceivables": true,
        "SiteIdentifier": "1537118_001_CNSGH_001_B",
        "OrgActiveClient": true,
        "AddressShortCode": "FRO00013239",
        "OrgOrganizationCode": "FRC00012937"
    }'
where
    id = 1515255 and tenant_id = 602;

-- Update Shipment
update
    shipment_details
set
    client_dps_address_id = 107634,
    updated_at = NOW()
where
    id = 95427 and tenant_id = 602;
