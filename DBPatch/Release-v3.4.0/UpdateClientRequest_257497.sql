
-- Shipment ID: 108963
-- Party ID: 1751188
-- Update Parties
UPDATE parties
SET
    org_code = 'FRC00022115',
    address_code = 'FRO00022786',
    org_id = '138117',
    address_id = '209608',
    updated_at = NOW(),
    org_data = '{
        "Id": 138117,
        "City": "OSIMO",
        "Guid": "2b011d24-2882-4f8e-93ca-403efe7a086b",
        "Email": "Luca.Biondi@enedopower.com",
        "IsGSA": false,
        "Phone": "0717214611",
        "State": "Marche",
        "label": "ENEDO S.P.A",
        "value": "FRC00022115",
        "Source": "CRP",
        "Country": "ITA",
        "Address1": "VIA ANCONA 59",
        "FullName": "ENEDO S.P.A",
        "IsActive": 1,
        "Payables": false,
        "TenantId": 442,
        "CompanyId": 372,
        "Consignee": true,
        "Consigner": true,
        "PANNumber": "IT02085400428",
        "TaxVendor": false,
        "ExtraParam": "{}",
        "InsertDate": "2024-12-09T21:06:09.000",
        "UpdateDate": "2025-02-05T16:33:01.000",
        "IsSuspended": false,
        "Receivables": true,
        "ZipPostCode": "60027",
        "ActiveClient": true,
        "CurrencyCode": "EUR",
        "EmailInvoice": false,
        "InsertUserId": 1712,
        "UpdateUserId": 1712,
        "VatRegNumber": "IT02085400428",
        "ForworderAgent": false,
        "IsCreditEnabled": false,
        "IsV2PaymentTerm": false,
        "OrganizationCode": "FRC00022115",
        "EnableBulkInvoice": false,
        "CustomerIdentifier": "1703168_001",
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
        "Id": 209608,
        "City": "OSIMO",
        "Email": "Luca.Biondi@enedopower.com",
        "State": "Marche",
        "label": "FRO00022786",
        "value": "FRO00022786",
        "Country": "ITA",
        "OrgGuid": "2b011d24-2882-4f8e-93ca-403efe7a086b",
        "Address1": "VIA ANCONA 59",
        "OrgSource": "CRP",
        "AddressType": 1,
        "CompanyName": "ENEDO S.P.A",
        "OrgFullName": "ENEDO S.P.A",
        "OrgPayables": false,
        "ZipPostCode": "60027",
        "TaxRegNumber": "IT02085400428",
        "OrgReceivables": true,
        "SiteIdentifier": "1703168_001_ITMIL_001_B",
        "OrgActiveClient": true,
        "AddressShortCode": "FRO00022786",
        "OrgOrganizationCode": "FRC00022115"
    }'
WHERE
    id = 1751188
    AND tenant_id = 676;

-- Update Shipments
UPDATE shipment_details
SET
    client_dps_address_id = 117202,
    updated_at = NOW()
WHERE
    id = 108963
    AND tenant_id = 676;

-- ========================================
-- END OF UPDATE
-- ========================================