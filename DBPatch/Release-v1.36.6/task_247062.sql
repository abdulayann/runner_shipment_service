-- Query 1: Update for FRC00036589 / FRO00045652 (Zero Motorcycles B.V.)
-- Shipment: RTMS25082123
-- Reason: User requested to update client details in shipment due to incorrect client added

update
    parties
set
    org_code = 'FRC00036589',
    address_code = 'FRO00045652',
    org_id = '153845',
    address_id = '272400',
    updated_at = NOW(),
    org_data = '{
    "Id": 153845,
    "City": "Noord-Scharwoude",
    "Guid": "c40022a7-4306-4362-97de-6b8a34be48e1",
    "Email": "eu.ap@zeromotorcycles.com",
    "IsGSA": false,
    "Phone": "0725112014",
    "label": "Zero Motorcycles B.V. ",
    "value": "FRC00036589",
    "Broker": false,
    "Source": "CRP",
    "Carrier": false,
    "Country": "NLD",
    "Address1": "Oester 12, Noord-Holland ",
    "FullName": "Zero Motorcycles B.V. ",
    "IsActive": 1,
    "Payables": false,
    "Services": false,
    "TenantId": 442,
    "CompanyId": 372,
    "Consignee": true,
    "Consigner": true,
    "IsParnter": false,
    "PANNumber": "37162492",
    "TaxVendor": false,
    "WareHouse": false,
    "AirCarrier": false,
    "ExtraParam": "",
    "InsertDate": "2025-06-05T17:00:59.000",
    "SeaCarrier": false,
    "UpdateDate": "2025-06-05T17:45:55.000",
    "IsSuspended": false,
    "RailCarrier": false,
    "Receivables": true,
    "RoadCarrier": false,
    "ZipPostCode": "1723 HW",
    "ActiveClient": true,
    "CurrencyCode": "EUR",
    "EmailInvoice": false,
    "InsertUserId": 1712,
    "UpdateUserId": 1712,
    "VatRegNumber": "NL822137434B02",
    "ForworderAgent": false,
    "IsCreditEnabled": false,
    "IsV2PaymentTerm": false,
    "TransportClient": false,
    "IsWalkInCustomer": false,
    "OrganizationCode": "FRC00036589",
    "EnableBulkInvoice": false,
    "CustomerIdentifier": "1902903_001",
    "ReceivableTermsDate": 2,
    "InsertUserIdUsername": "p100serviceaccountegypt@dpworld.com",
    "UpdateUserIdUsername": "p100serviceaccountegypt@dpworld.com",
    "IsClientEInvoiceEnabled": false,
    "ImportExportClearanceLock": false,
    "FusionCreditlimitOverrideApproved": false
}',
    address_data = '{
    "Id": 272400,
    "City": "Tiel",
    "Guid": "ffa42ca1-6620-4b47-8c61-89e4fd2cf9ac",
    "Email": "",
    "label": "Zero Motorcycles B.V. \\nMOL CARGO HET EEK 9,Gelderland, Tiel\\nNLD, 4004 LM\\n",
    "value": "FRO00045652",
    "Country": "NLD",
    "OrgGuid": "c40022a7-4306-4362-97de-6b8a34be48e1",
    "Address1": "MOL CARGO HET EEK 9,Gelderland",
    "Address2": "",
    "OrgSource": "CRP",
    "AddressType": 1,
    "CompanyName": "Zero Motorcycles B.V. ",
    "OrgFullName": "Zero Motorcycles B.V. ",
    "OrgPayables": false,
    "ZipPostCode": "4004 LM",
    "TaxRegNumber": "NL822137434B02",
    "KnownConsignor": false,
    "OrgReceivables": true,
    "RegulatedAgent": false,
    "OrgActiveClient": true,
    "AddressShortCode": "FRO00045652",
    "OrgOrganizationCode": "FRC00036589"
}'
where
    id = (select client_id from shipment_details where shipment_id='RTMS25082123' and tenant_id = 442);

update
    shipment_details
set
    client_dps_address_id = 101431,
    updated_at = NOW()
where
    shipment_id='RTMS25082123' and tenant_id = 442;

-- Query 2: Update for FRC00036589 / FRO00045652 (Zero Motorcycles B.V.)
-- Shipment: LAXS25071049
-- Reason: User requested to update client details in shipment due to incorrect client added

update
    parties
set
    org_code = 'FRC00036589',
    address_code = 'FRO00045652',
    org_id = '153845',
    address_id = '272400',
    updated_at = NOW(),
    org_data = '{
    "Id": 153845,
    "City": "Noord-Scharwoude",
    "Guid": "c40022a7-4306-4362-97de-6b8a34be48e1",
    "Email": "eu.ap@zeromotorcycles.com",
    "IsGSA": false,
    "Phone": "0725112014",
    "label": "Zero Motorcycles B.V. ",
    "value": "FRC00036589",
    "Broker": false,
    "Source": "CRP",
    "Carrier": false,
    "Country": "NLD",
    "Address1": "Oester 12, Noord-Holland ",
    "FullName": "Zero Motorcycles B.V. ",
    "IsActive": 1,
    "Payables": false,
    "Services": false,
    "TenantId": 442,
    "CompanyId": 372,
    "Consignee": true,
    "Consigner": true,
    "IsParnter": false,
    "PANNumber": "37162492",
    "TaxVendor": false,
    "WareHouse": false,
    "AirCarrier": false,
    "ExtraParam": "",
    "InsertDate": "2025-06-05T17:00:59.000",
    "SeaCarrier": false,
    "UpdateDate": "2025-06-05T17:45:55.000",
    "IsSuspended": false,
    "RailCarrier": false,
    "Receivables": true,
    "RoadCarrier": false,
    "ZipPostCode": "1723 HW",
    "ActiveClient": true,
    "CurrencyCode": "EUR",
    "EmailInvoice": false,
    "InsertUserId": 1712,
    "UpdateUserId": 1712,
    "VatRegNumber": "NL822137434B02",
    "ForworderAgent": false,
    "IsCreditEnabled": false,
    "IsV2PaymentTerm": false,
    "TransportClient": false,
    "IsWalkInCustomer": false,
    "OrganizationCode": "FRC00036589",
    "EnableBulkInvoice": false,
    "CustomerIdentifier": "1902903_001",
    "ReceivableTermsDate": 2,
    "InsertUserIdUsername": "p100serviceaccountegypt@dpworld.com",
    "UpdateUserIdUsername": "p100serviceaccountegypt@dpworld.com",
    "IsClientEInvoiceEnabled": false,
    "ImportExportClearanceLock": false,
    "FusionCreditlimitOverrideApproved": false
}',
    address_data = '{
    "Id": 272400,
    "City": "Tiel",
    "Guid": "ffa42ca1-6620-4b47-8c61-89e4fd2cf9ac",
    "Email": "",
    "label": "Zero Motorcycles B.V. \\nMOL CARGO HET EEK 9,Gelderland, Tiel\\nNLD, 4004 LM\\n",
    "value": "FRO00045652",
    "Country": "NLD",
    "OrgGuid": "c40022a7-4306-4362-97de-6b8a34be48e1",
    "Address1": "MOL CARGO HET EEK 9,Gelderland",
    "Address2": "",
    "OrgSource": "CRP",
    "AddressType": 1,
    "CompanyName": "Zero Motorcycles B.V. ",
    "OrgFullName": "Zero Motorcycles B.V. ",
    "OrgPayables": false,
    "ZipPostCode": "4004 LM",
    "TaxRegNumber": "NL822137434B02",
    "KnownConsignor": false,
    "OrgReceivables": true,
    "RegulatedAgent": false,
    "OrgActiveClient": true,
    "AddressShortCode": "FRO00045652",
    "OrgOrganizationCode": "FRC00036589"
}'
where
    id = (select client_id from shipment_details where shipment_id='LAXS25071049' and tenant_id = 442);

update
    shipment_details
set
    client_dps_address_id = 101431,
    updated_at = NOW()
where
    shipment_id='LAXS25071049' and tenant_id = 442;