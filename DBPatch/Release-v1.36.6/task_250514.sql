UPDATE parties
SET
    org_code    = 'FRC00014041',
    address_code= 'FRO00014378',
    org_id      = '129218',
    address_id  = '184063',
    updated_at  = NOW(),
    org_data    = '{
                       "Id": 129218,
                       "City": "SINGEN HOHENTWIEL",
                       "Guid": "9da68249-99ca-441a-833d-3c3fc7ba24d4",
                       "Email": "na@na.com",
                       "IsGSA": false,
                       "Phone": "119999999999",
                       "State": "Baden-Württemberg",
                       "label": "AMCOR FLEXIBLES SINGEN GMBH",
                       "value": "FRC00014041",
                       "Source": "CRP",
                       "Country": "DEU",
                       "Address1": "ALUSINGENPLATZ 1",
                       "FullName": "AMCOR FLEXIBLES SINGEN GMBH",
                       "IsActive": 1,
                       "Payables": false,
                       "TenantId": 442,
                       "CompanyId": 372,
                       "Consignee": true,
                       "Consigner": true,
                       "PANNumber": "FRC00014041",
                       "TaxVendor": false,
                       "ExtraParam": "{}",
                       "InsertDate": "2024-08-29T15:44:21.000",
                       "UpdateDate": "2024-08-29T15:44:21.000",
                       "IsSuspended": false,
                       "Receivables": false,
                       "ZipPostCode": "78224",
                       "ActiveClient": true,
                       "CurrencyCode": "EUR",
                       "EmailInvoice": false,
                       "InsertUserId": 1712,
                       "ForworderAgent": false,
                       "IsCreditEnabled": false,
                       "IsV2PaymentTerm": false,
                       "OrganizationCode": "FRC00014041",
                       "InsertUserIdUsername": "p100serviceaccountegypt@dpworld.com",
                       "OrgQuoteValidityDays": 0,
                       "CreditLimitOfConfigRow": "",
                       "CreditTermsOfConfigRow": "",
                       "ImportExportClearanceLock": false
                   }',
    address_data = '{
                        "Id": 184063,
                        "City": "SINGEN HOHENTWIEL",
                        "Email": "na@na.com",
                        "State": "Baden-Württemberg",
                        "label": "FRO00014378",
                        "value": "FRO00014378",
                        "Country": "DEU",
                        "OrgGuid": "9da68249-99ca-441a-833d-3c3fc7ba24d4",
                        "Address1": "ALUSINGENPLATZ 1",
                        "OrgSource": "CRP",
                        "AddressType": 2,
                        "CompanyName": "AMCOR FLEXIBLES SINGEN GMBH",
                        "OrgFullName": "AMCOR FLEXIBLES SINGEN GMBH",
                        "OrgPayables": false,
                        "ZipPostCode": "78224",
                        "OrgReceivables": false,
                        "OrgActiveClient": true,
                        "AddressShortCode": "FRO00014378",
                        "OrgOrganizationCode": "FRC00014041"
                    }'
WHERE tenant_id = 474
  AND id IN (
    1518814,1520317,1520390,1520944,1521181,1521255,
    1526987,1527857,1527908,1528114,1529096,1529126,
    1529601,1535665);

UPDATE shipment_details
SET
    client_dps_address_id = 105097,
    updated_at = NOW()
WHERE tenant_id = 474
  AND id IN (
    95646,95730,95736,95771,95778,95782,
    96133,96184,96186,96197,96260,96262,
    96294,96649);

