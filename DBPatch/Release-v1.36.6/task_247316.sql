-- Ticket 247316
-- Query 1: 'FRC00035108'/ 'FRO00036534',
-- Shipment: SGHA25086343
update
    parties
set
    org_code = 'FRC00035108',
    address_code = 'FRO00036534',
    org_id = '152038',
    address_id = '252562',
    updated_at = NOW(),
    org_data = '{
                    "Id": 152038,
                    "City": "Zurich",
                    "Guid": "11cfaf5c-3901-441f-a378-c0871753ff22",
                    "Email": "invoices@nexxiot.com",
                    "IsGSA": false,
                    "Phone": "442755151",
                    "label": "Nexxiot AG",
                    "value": "FRC00035108",
                    "Broker": false,
                    "Source": "CRP",
                    "Carrier": false,
                    "Country": "CHE",
                    "Address1": "Nordstrasse 15",
                    "FullName": "Nexxiot AG",
                    "IsActive": 1,
                    "Payables": false,
                    "Services": false,
                    "TenantId": 442,
                    "CompanyId": 372,
                    "Consignee": true,
                    "Consigner": true,
                    "IsParnter": false,
                    "PANNumber": "CHE-429226969",
                    "TaxVendor": false,
                    "WareHouse": false,
                    "AirCarrier": false,
                    "ExtraParam": "",
                    "InsertDate": "2025-05-20T23:56:51.000",
                    "SeaCarrier": false,
                    "UpdateDate": "2025-07-21T10:35:24.000",
                    "IsSuspended": false,
                    "RailCarrier": false,
                    "Receivables": true,
                    "RoadCarrier": false,
                    "ZipPostCode": "8006",
                    "ActiveClient": true,
                    "CurrencyCode": "CHF",
                    "EmailInvoice": false,
                    "InsertUserId": 1712,
                    "UpdateUserId": 1712,
                    "VatRegNumber": "429226969",
                    "ForworderAgent": false,
                    "IsCreditEnabled": false,
                    "IsV2PaymentTerm": false,
                    "TransportClient": false,
                    "IsWalkInCustomer": false,
                    "OrganizationCode": "FRC00035108",
                    "EnableBulkInvoice": false,
                    "CustomerIdentifier": "1885254_001",
                    "ReceivableTermsDate": 2,
                    "InsertUserIdUsername": "p100serviceaccountegypt@dpworld.com",
                    "UpdateUserIdUsername": "p100serviceaccountegypt@dpworld.com",
                    "IsClientEInvoiceEnabled": false,
                    "ImportExportClearanceLock": false,
                    "FusionCreditlimitOverrideApproved": false

                }',
    address_data = '{
                        "Id": 252562,
                        "City": "Zurich",
                        "Email": "invoices@nexxiot.com",
                        "label": "FRO00036534",
                        "value": "FRO00036534",
                        "Country": "CHE",
                        "OrgGuid": "11cfaf5c-3901-441f-a378-c0871753ff22",
                        "Address1": "Nordstrasse 15",
                        "OrgSource": "CRP",
                        "AddressType": 1,
                        "CompanyName": "Nexxiot AG",
                        "OrgFullName": "Nexxiot AG",
                        "OrgPayables": false,
                        "ZipPostCode": "8006",
                        "TaxRegNumber": "429226969",
                        "OrgReceivables": true,
                        "SiteIdentifier": "1885254_001_CHTWL_001_B",
                        "OrgActiveClient": true,
                        "AddressShortCode": "FRO00036534",
                        "OrgOrganizationCode": "FRC00035108"

                    }'
where
     id = 1422110 and tenant_id = 685;

update
    shipment_details
set
    client_dps_address_id = 105255,
    updated_at = NOW()
where
   id = 90065 and tenant_id = 685;