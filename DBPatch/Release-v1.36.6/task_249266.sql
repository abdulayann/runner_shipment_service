-- Shipment: GVAA25070210
update
    parties
set
    org_code = 'FRC00039932',
    address_code = 'FRO00041680',
    org_id = '157243',
    address_id = '262336',
    updated_at = NOW(),
    org_data = '{
                    "Id": 157243,
                    "City": "Basel",
                    "Guid": "97e3783c-86a2-4435-b01d-632b28536de1",
                    "Email": "",
                    "IsGSA": false,
                    "Phone": "",
                    "State": "Basel-Stadt",
                    "label": "BRENNTAG SCHWEIZERHALL AG",
                    "value": "FRC00039932",
                    "Broker": false,
                    "Source": "CRP",
                    "Carrier": false,
                    "Country": "CHE",
                    "Address1": "ELSASSERSTRASSE 231",
                    "Address2": "",
                    "FullName": "BRENNTAG SCHWEIZERHALL AG",
                    "IsActive": 1,
                    "Payables": false,
                    "Services": false,
                    "TenantId": 442,
                    "CompanyId": 372,
                    "Consignee": true,
                    "Consigner": true,
                    "IsParnter": false,
                    "PANNumber": "43w4tfrf",
                    "TaxVendor": false,
                    "WareHouse": false,
                    "AirCarrier": false,
                    "ExtraParam": "",
                    "InsertDate": "2025-07-05T02:28:15.000",
                    "SeaCarrier": false,
                    "UpdateDate": "2025-07-05T02:28:15.000",
                    "IsSuspended": false,
                    "RailCarrier": false,
                    "Receivables": false,
                    "RoadCarrier": false,
                    "ZipPostCode": "4056",
                    "ActiveClient": true,
                    "CurrencyCode": "CHF",
                    "EmailInvoice": false,
                    "InsertUserId": 1712,
                    "UpdateUserId": 1712,
                    "ForworderAgent": false,
                    "IsCreditEnabled": false,
                    "IsV2PaymentTerm": false,
                    "TransportClient": false,
                    "IsWalkInCustomer": false,
                    "OrganizationCode": "FRC00039932",
                    "EnableBulkInvoice": false,
                    "InsertUserIdUsername": "p100serviceaccountegypt@dpworld.com",
                    "UpdateUserIdUsername": "p100serviceaccountegypt@dpworld.com",
                    "IsClientEInvoiceEnabled": false,
                    "ImportExportClearanceLock": false,
                    "FusionCreditlimitOverrideApproved": false
                }',

    address_data = '{
                        "Id": 262336,
                        "City": "Basel",
                        "Email": "",
                        "State": "Basel-Stadt",
                        "label": "FRO00041680",
                        "value": "FRO00041680",
                        "Country": "CHE",
                        "OrgGuid": "97e3783c-86a2-4435-b01d-632b28536de1",
                        "Address1": "ELSASSERSTRASSE 231",
                        "Address2": "",
                        "OrgSource": "CRP",
                        "AddressType": 2,
                        "CompanyName": "BRENNTAG SCHWEIZERHALL AG",
                        "OrgFullName": "BRENNTAG SCHWEIZERHALL AG",
                        "OrgPayables": false,
                        "ZipPostCode": "4056",
                        "OrgReceivables": false,
                        "OrgActiveClient": true,
                        "AddressShortCode": "FRO00041680",
                        "OrgOrganizationCode": "FRC00039932"
                    }'

where
    id = 1316621;

update
    shipment_details
set
    client_dps_address_id = 108718,
    updated_at = NOW()
where
    id=83994;




-- Shipment: GVAA25070210 , Branch - Basel
update
    parties
set
    org_code = 'FRC00039841',
    address_code = 'FRO00041585',
    org_id = '157275',
    address_id = '262371',
    updated_at = NOW(),
    org_data = '{
                    "Id": 157275,
                    "City": "Dottingen",
                    "Guid": "52634fc2-8c10-4015-8e66-dfd8a62f2b3f",
                    "Email": "Nal@sara-transporte.ch",
                    "IsGSA": false,
                    "Phone": "0041562819884",
                    "State": "Aargau",
                    "label": "Sara Transporte AG",
                    "value": "FRC00039841",
                    "Broker": false,
                    "Source": "CRP",
                    "Carrier": false,
                    "Country": "CHE",
                    "Address1": "Vorhard 15",
                    "FullName": "Sara Transporte AG",
                    "IsActive": 1,
                    "Payables": false,
                    "Services": false,
                    "TenantId": 442,
                    "CompanyId": 372,
                    "Consignee": true,
                    "Consigner": true,
                    "IsParnter": false,
                    "PANNumber": "CHE-264.842.468",
                    "TaxVendor": false,
                    "WareHouse": false,
                    "AirCarrier": false,
                    "ExtraParam": "",
                    "InsertDate": "2025-07-06T21:11:39.000",
                    "SeaCarrier": false,
                    "UpdateDate": "2025-07-06T23:10:16.000",
                    "IsSuspended": false,
                    "RailCarrier": false,
                    "Receivables": true,
                    "RoadCarrier": false,
                    "ZipPostCode": "5312",
                    "ActiveClient": true,
                    "CurrencyCode": "CHF",
                    "EmailInvoice": false,
                    "InsertUserId": 1712,
                    "UpdateUserId": 1712,
                    "VatRegNumber": "CHE264842468",
                    "ForworderAgent": false,
                    "IsCreditEnabled": false,
                    "IsV2PaymentTerm": false,
                    "TransportClient": false,
                    "IsWalkInCustomer": false,
                    "OrganizationCode": "FRC00039841",
                    "EnableBulkInvoice": false,
                    "CustomerIdentifier": "1937295_001",
                    "ReceivableTermsDate": 2,
                    "InsertUserIdUsername": "p100serviceaccountegypt@dpworld.com",
                    "UpdateUserIdUsername": "p100serviceaccountegypt@dpworld.com",
                    "IsClientEInvoiceEnabled": false,
                    "ImportExportClearanceLock": false,
                    "FusionCreditlimitOverrideApproved": false
                }',
    address_data = '{
                        "Id": 262371,
                        "City": "Dottingen",
                        "Email": "Nal@sara-transporte.ch",
                        "State": "Aargau",
                        "label": "FRO00041585",
                        "value": "FRO00041585",
                        "Country": "CHE",
                        "OrgGuid": "52634fc2-8c10-4015-8e66-dfd8a62f2b3f",
                        "Address1": "Vorhard 15",
                        "OrgSource": "CRP",
                        "AddressType": 2,
                        "CompanyName": "Sara Transporte AG",
                        "OrgFullName": "Sara Transporte AG",
                        "OrgPayables": false,
                        "ZipPostCode": "5312",
                        "TaxRegNumber": "CHE264842468",
                        "OrgReceivables": true,
                        "OrgActiveClient": true,
                        "AddressShortCode": "FRO00041585",
                        "OrgOrganizationCode": "FRC00039841"
                    }'
where
    id = 1509573;

update
    shipment_details
set
    client_dps_address_id = 107687,
    updated_at = NOW()
where
    id=95109;