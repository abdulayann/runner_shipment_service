update parties
set
org_code = 'FRC00030167',
org_id = '147025',
org_data = '{"Id": 147025, "City": "Ostrava", "Guid": "0247d8f1-2b63-40c3-8077-9164e115a964", "Email": "petr.revaj@dhl.com", "label": "DHL Express Czech Republic s.r.o.", "value": "FRC00030167", "Country": "CZE", "Address1": "Nadrazni 2967/93, Moravska Ostrava", "FullName": "DHL Express Czech Republic s.r.o.", "Payables": false, "CompanyId": 372, "Receivables": true, "ZipPostCode": "70200", "ForworderAgent": false, "OrganizationCode": "FRC00030167", "InsertUserIdUsername": "p100serviceaccountegypt@dpworld.com", "UpdateUserIdUsername": "p100serviceaccountegypt@dpworld.com"}',
address_id = '238682',
address_code = 'FRO00031144',
address_data = '{"Id": 238682, "City": "Ostrava", "Guid": "5dcbae88-c893-47fd-8ddb-48c79921747b", "Email": "petr.revaj@dhl.com", "Country": "CZE", "OrgGuid": "0247d8f1-2b63-40c3-8077-9164e115a964", "Address1": "Nadrazni 2967/93, Moravska Ostrava", "OrgSource": "CRP", "AddressType": 1, "CompanyName": "DHL Express Czech Republic s.r.o.", "OrgFullName": "DHL Express Czech Republic s.r.o.", "OrgPayables": false, "ZipPostCode": "70200", "TaxRegNumber": "CZ25683446", "KnownConsignor": false, "OrgReceivables": true, "RegulatedAgent": false, "SiteIdentifier": "1829790_001_CZLOU_001_B", "OrgActiveClient": true, "AddressShortCode": "FRO00031144", "OrgOrganizationCode": "FRC00030167"}'
where id = 870189;

update shipment_details
set client_dps_address_id = 81255
where id = 58961;