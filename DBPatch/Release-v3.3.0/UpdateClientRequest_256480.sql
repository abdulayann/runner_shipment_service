-- Shipment ID : (106110)
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
    "Id": 141676,
    "City": "CASTEL MAGGIORE",
    "Guid": "5f7baf9a-9972-488b-8c20-ef12f8436a71",
    "Email": "Patrizia.Meluzzi@bonfiglioli.com",
    "IsGSA": false,
    "Phone": "0516387271",
    "State": "Emilia-Romagna",
    "label": "SELCOM GROUP S.P.A.",
    "value": "FRC00024878",
    "Source": "CRP",
    "Country": "ITA",
    "Address1": "VIA GRANDI 5",
    "FullName": "SELCOM GROUP S.P.A.",
    "IsActive": 1,
    "Payables": false,
    "TenantId": 442,
    "CompanyId": 372,
    "Consignee": true,
    "Consigner": true,
    "PANNumber": "IT09865430962",
    "TaxVendor": false,
    "ExtraParam": "{}",
    "InsertDate": "2025-01-27T11:26:40.000",
    "UpdateDate": "2025-02-05T16:33:01.000",
    "IsSuspended": false,
    "Receivables": true,
    "ZipPostCode": "40013",
    "ActiveClient": true,
    "CurrencyCode": "EUR",
    "EmailInvoice": false,
    "InsertUserId": 1712,
    "UpdateUserId": 1712,
    "VatRegNumber": "IT09865430962",
    "ForworderAgent": false,
    "IsCreditEnabled": false,
    "IsV2PaymentTerm": false,
    "OrganizationCode": "FRC00024878",
    "EnableBulkInvoice": false,
    "CustomerIdentifier": "1759728_001",
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
    "Id": 222963,
    "City": "CASTEL MAGGIORE",
    "Email": "Patrizia.Meluzzi@bonfiglioli.com",
    "State": "Emilia-Romagna",
    "label": "SELCOM GROUP S.P.A., VIA GRANDI 5, CASTEL MAGGIORE, Emilia-Romagna, ITA",
    "value": "FRO00025659",
    "Country": "ITA",
    "OrgGuid": "5f7baf9a-9972-488b-8c20-ef12f8436a71",
    "Address1": "VIA GRANDI 5",
    "OrgSource": "CRP",
    "AddressType": 1,
    "CompanyName": "SELCOM GROUP S.P.A.",
    "OrgFullName": "SELCOM GROUP S.P.A.",
    "OrgPayables": false,
    "ZipPostCode": "40013",
    "TaxRegNumber": "IT09865430962",
    "OrgReceivables": true,
    "SiteIdentifier": "1759728_001_ITMIL_001_B",
    "OrgActiveClient": true,
    "AddressShortCode": "FRO00025659",
    "OrgOrganizationCode": "FRC00024878"
}'
where
    id in (106110) and tenant_id = 676;
-- Update Shipments
UPDATE shipment_details
SET
    client_dps_address_id = 222963,
    updated_at = NOW()
WHERE
    id in (106110)
    AND tenant_id = 676;