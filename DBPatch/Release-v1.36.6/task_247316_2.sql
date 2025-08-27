-- BKKS25062249

update
    parties
set
    org_code = 'FRC00011599',
    address_code = 'FRO00011930',
    org_id = '132498',
    address_id = '189530',
    updated_at = NOW(),
    org_data = '{
                    "Id": 132498,
                    "City": "Abu Dhabi",
                    "Guid": "a6ca4900-5a89-4404-a3e3-273b4d6c27ba",
                    "Email": "unitco@eim.ae",
                    "IsGSA": false,
                    "Phone": "26777701",
                    "label": "UNITED ELECTRONICS COMPANY - L L C",
                    "value": "FRC00011599",
                    "Source": "CRP",
                    "Country": "ARE",
                    "Address1": "SAIF BIN DARWISH - PRP12990, HAMDAN STREET, ZIYANI,",
                    "FullName": "UNITED ELECTRONICS COMPANY - L L C",
                    "IsActive": 1,
                    "Payables": false,
                    "TenantId": 442,
                    "CompanyId": 372,
                    "Consignee": true,
                    "Consigner": true,
                    "PANNumber": "100011562400003",
                    "TaxVendor": false,
                    "ExtraParam": "{}",
                    "InsertDate": "2024-09-26T19:20:03.000",
                    "UpdateDate": "2024-10-15T20:10:54.000",
                    "IsSuspended": false,
                    "Receivables": true,
                    "ActiveClient": true,
                    "CurrencyCode": "AED",
                    "EmailInvoice": false,
                    "InsertUserId": 1712,
                    "UpdateUserId": 1712,
                    "VatRegNumber": "100011562400003",
                    "ForworderAgent": false,
                    "IsCreditEnabled": false,
                    "IsV2PaymentTerm": false,
                    "OrganizationCode": "FRC00011599",
                    "CustomerIdentifier": "1591585_001",
                    "ReceivableTermsDate": 2,
                    "InsertUserIdUsername": "p100serviceaccountegypt@dpworld.com",
                    "OrgQuoteValidityDays": 0,
                    "UpdateUserIdUsername": "p100serviceaccountegypt@dpworld.com",
                    "CreditLimitOfConfigRow": "",
                    "CreditTermsOfConfigRow": "",
                    "ImportExportClearanceLock": false
                }',
    address_data = '{
                        "Id": 189530,
                        "City": "Abu Dhabi",
                        "Email": "unitco@eim.ae",
                        "label": "FRO00011930",
                        "value": "FRO00011930",
                        "Country": "ARE",
                        "OrgGuid": "a6ca4900-5a89-4404-a3e3-273b4d6c27ba",
                        "Address1": "SAIF BIN DARWISH - PRP12990, HAMDAN STREET, ZIYANI,",
                        "OrgSource": "CRP",
                        "AddressType": 1,
                        "CompanyName": "UNITED ELECTRONICS COMPANY - L L C",
                        "OrgFullName": "UNITED ELECTRONICS COMPANY - L L C",
                        "OrgPayables": false,
                        "TaxRegNumber": "100011562400003",
                        "OrgReceivables": true,
                        "SiteIdentifier": "1591585_001_FF-AUH_001_B",
                        "OrgActiveClient": true,
                        "AddressShortCode": "FRO00011930",
                        "OrgOrganizationCode": "FRC00011599"
                    }'
where
    id = (select client_id from shipment_details where shipment_id='BKKS25062249' and tenant_id = 506);
update
    shipment_details
set
    client_dps_address_id = 105263,
    updated_at = NOW()
where
   id = 72160 and tenant_id = 506;


--BKKS25062259

update
    parties
set
    org_code = 'FRC00011599',
    address_code = 'FRO00011930',
    org_id = '132498',
    address_id = '189530',
    updated_at = NOW(),
    org_data = '{
                    "Id": 132498,
                    "City": "Abu Dhabi",
                    "Guid": "a6ca4900-5a89-4404-a3e3-273b4d6c27ba",
                    "Email": "unitco@eim.ae",
                    "IsGSA": false,
                    "Phone": "26777701",
                    "label": "UNITED ELECTRONICS COMPANY - L L C",
                    "value": "FRC00011599",
                    "Source": "CRP",
                    "Country": "ARE",
                    "Address1": "SAIF BIN DARWISH - PRP12990, HAMDAN STREET, ZIYANI,",
                    "FullName": "UNITED ELECTRONICS COMPANY - L L C",
                    "IsActive": 1,
                    "Payables": false,
                    "TenantId": 442,
                    "CompanyId": 372,
                    "Consignee": true,
                    "Consigner": true,
                    "PANNumber": "100011562400003",
                    "TaxVendor": false,
                    "ExtraParam": "{}",
                    "InsertDate": "2024-09-26T19:20:03.000",
                    "UpdateDate": "2024-10-15T20:10:54.000",
                    "IsSuspended": false,
                    "Receivables": true,
                    "ActiveClient": true,
                    "CurrencyCode": "AED",
                    "EmailInvoice": false,
                    "InsertUserId": 1712,
                    "UpdateUserId": 1712,
                    "VatRegNumber": "100011562400003",
                    "ForworderAgent": false,
                    "IsCreditEnabled": false,
                    "IsV2PaymentTerm": false,
                    "OrganizationCode": "FRC00011599",
                    "CustomerIdentifier": "1591585_001",
                    "ReceivableTermsDate": 2,
                    "InsertUserIdUsername": "p100serviceaccountegypt@dpworld.com",
                    "OrgQuoteValidityDays": 0,
                    "UpdateUserIdUsername": "p100serviceaccountegypt@dpworld.com",
                    "CreditLimitOfConfigRow": "",
                    "CreditTermsOfConfigRow": "",
                    "ImportExportClearanceLock": false
                }',
    address_data = '{
                        "Id": 189530,
                        "City": "Abu Dhabi",
                        "Email": "unitco@eim.ae",
                        "label": "FRO00011930",
                        "value": "FRO00011930",
                        "Country": "ARE",
                        "OrgGuid": "a6ca4900-5a89-4404-a3e3-273b4d6c27ba",
                        "Address1": "SAIF BIN DARWISH - PRP12990, HAMDAN STREET, ZIYANI,",
                        "OrgSource": "CRP",
                        "AddressType": 1,
                        "CompanyName": "UNITED ELECTRONICS COMPANY - L L C",
                        "OrgFullName": "UNITED ELECTRONICS COMPANY - L L C",
                        "OrgPayables": false,
                        "TaxRegNumber": "100011562400003",
                        "OrgReceivables": true,
                        "SiteIdentifier": "1591585_001_FF-AUH_001_B",
                        "OrgActiveClient": true,
                        "AddressShortCode": "FRO00011930",
                        "OrgOrganizationCode": "FRC00011599"
                    }'
where
    id = (select client_id from shipment_details where shipment_id='BKKS25062259' and tenant_id = 506);
update
    shipment_details
set
    client_dps_address_id = 105263,
    updated_at = NOW()
where
   id = 72202 and tenant_id = 506;

--NBOCSS25082428

update
    parties
set
    org_code = 'FRC00043253',
    address_code = 'FRO00045199',
    org_id = '160312',
    address_id = '271603',
    updated_at = NOW(),
    org_data = '{
                    "Id": 160312,
                    "City": "HANGZHOU",
                    "Guid": "341e8d6f-a770-4acd-9276-183c221c7da0",
                    "Email": "yelili@sunkiss.org.cn",
                    "IsGSA": false,
                    "Phone": "013357112156",
                    "State": "ZJ",
                    "label": "SUNKISS HEALTHCARE (ZHEJIANG) CO.,LTD",
                    "value": "FRC00043253",
                    "Broker": false,
                    "Source": "CRP",
                    "Carrier": false,
                    "Country": "CHN",
                    "Address1": "NO.6 PANLONGSHAN ROAD, CHANGKOU NEW ZONE, ",
                    "Address2": "FUYANG ECONOMIC DEVELOPMENT ZONE",
                    "FullName": "SUNKISS HEALTHCARE (ZHEJIANG) CO.,LTD",
                    "IsActive": 1,
                    "Payables": false,
                    "Services": false,
                    "TenantId": 442,
                    "CompanyId": 372,
                    "Consignee": true,
                    "Consigner": true,
                    "IsParnter": false,
                    "PANNumber": "913301005551720254",
                    "StateCode": "ZJ",
                    "TaxVendor": false,
                    "WareHouse": false,
                    "AirCarrier": false,
                    "ExtraParam": "",
                    "InsertDate": "2025-08-05T21:36:54.000",
                    "SeaCarrier": false,
                    "UpdateDate": "2025-08-05T22:31:19.000",
                    "IsSuspended": false,
                    "RailCarrier": false,
                    "Receivables": true,
                    "RoadCarrier": false,
                    "ZipPostCode": "311422",
                    "ActiveClient": true,
                    "CurrencyCode": "CNY",
                    "EmailInvoice": false,
                    "InsertUserId": 1712,
                    "UpdateUserId": 1712,
                    "VatRegNumber": "913301005551720254",
                    "ForworderAgent": false,
                    "CustomerTaxType": 1,
                    "IsCreditEnabled": false,
                    "IsV2PaymentTerm": false,
                    "TransportClient": false,
                    "IsWalkInCustomer": false,
                    "OrganizationCode": "FRC00043253",
                    "EnableBulkInvoice": false,
                    "CustomerIdentifier": "1980658_001",
                    "ReceivableTermsDate": 2,
                    "InsertUserIdUsername": "p100serviceaccountegypt@dpworld.com",
                    "UpdateUserIdUsername": "p100serviceaccountegypt@dpworld.com",
                    "IsClientEInvoiceEnabled": false,
                    "ImportExportClearanceLock": false,
                    "FusionCreditlimitOverrideApproved": false
                }',
    address_data = '{
                        "Id": 271603,
                        "City": "HANGZHOU",
                        "Email": "yelili@sunkiss.org.cn",
                        "State": "ZJ",
                        "label": "FRO00045199",
                        "value": "FRO00045199",
                        "Country": "CHN",
                        "OrgGuid": "341e8d6f-a770-4acd-9276-183c221c7da0",
                        "Address1": "NO.6 PANLONGSHAN ROAD, CHANGKOU NEW ZONE, ",
                        "Address2": "FUYANG ECONOMIC DEVELOPMENT ZONE",
                        "OrgSource": "CRP",
                        "AddressType": 1,
                        "CompanyName": "SUNKISS HEALTHCARE (ZHEJIANG) CO.,LTD",
                        "OrgFullName": "SUNKISS HEALTHCARE (ZHEJIANG) CO.,LTD",
                        "OrgPayables": false,
                        "ZipPostCode": "311422",
                        "TaxRegNumber": "913301005551720254",
                        "OrgReceivables": true,
                        "SiteIdentifier": "1980658_001_CNSGH_002_B",
                        "OrgActiveClient": true,
                        "AddressShortCode": "FRO00045199",
                        "OrgOrganizationCode": "FRC00043253"
                    }'
where
    id = (select client_id from shipment_details where shipment_id='NBOCSS25082428' and tenant_id = 706);

update
    shipment_details
set
    client_dps_address_id = 105624,
    updated_at = NOW()
where
   id = 85303 and tenant_id = 706;