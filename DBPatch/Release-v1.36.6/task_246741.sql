-- Query for FRC00024878 / FRO00025659 (SELCOM GROUP S.P.A.)
-- Branch: Bologna
-- Shipment: BLQA25081352
-- Reason: As per CSO, Client should be consider as SELCOM GROUP S.P.A.

update
    parties
set
    org_code = 'FRC00024878',
    address_code = 'FRO00025659',
    org_id = '141676',
    address_id = '222963',
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
        "Broker": false,
        "Source": "CRP",
        "Carrier": false,
        "Country": "ITA",
        "Address1": "VIA GRANDI 5",
        "FullName": "SELCOM GROUP S.P.A.",
        "IsActive": 1,
        "Payables": false,
        "Services": false,
        "TenantId": 442,
        "CompanyId": 372,
        "Consignee": true,
        "Consigner": true,
        "IsParnter": false,
        "PANNumber": "IT09865430962",
        "TaxVendor": false,
        "WareHouse": false,
        "AirCarrier": false,
        "ExtraParam": "",
        "InsertDate": "2025-01-27T11:26:40.000",
        "SeaCarrier": false,
        "UpdateDate": "2025-04-10T10:05:52.000",
        "IsSuspended": false,
        "RailCarrier": false,
        "Receivables": true,
        "RoadCarrier": false,
        "ZipPostCode": "40013",
        "ActiveClient": true,
        "CurrencyCode": "EUR",
        "EmailInvoice": false,
        "InsertUserId": 1712,
        "UpdateUserId": 1712,
        "VatRegNumber": "IT09865430962",
        "ForworderAgent": false,
        "IsCreditEnabled": true,
        "IsV2PaymentTerm": false,
        "TransportClient": false,
        "IsWalkInCustomer": false,
        "OrganizationCode": "FRC00024878",
        "EnableBulkInvoice": false,
        "CustomerIdentifier": "1759728_001",
        "ReceivableTermsDate": 2,
        "InsertUserIdUsername": "p100serviceaccountegypt@dpworld.com",
        "OrgQuoteValidityDays": 0,
        "UpdateUserIdUsername": "p100serviceaccountegypt@dpworld.com",
        "IsClientEInvoiceEnabled": false,
        "ImportExportClearanceLock": false,
        "FusionCreditlimitOverrideApproved": false
    }',
    address_data = '{
        "Id": 222963,
        "City": "CASTEL MAGGIORE",
        "Email": "Patrizia.Meluzzi@bonfiglioli.com",
        "State": "Emilia-Romagna",
        "label": "FRO00025659",
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
    id = 89622 and tenant_id = 676;

update
    shipment_details
set
    client_dps_address_id = 102310,
    updated_at = NOW()
where
   id = 89622 and tenant_id = 676;