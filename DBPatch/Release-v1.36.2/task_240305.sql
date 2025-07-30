update
	parties
set
	org_code = 'FRC00027912',
	address_code = 'FRO00028809',
	org_id = '155396',
	address_id = '258853',
	updated_at = NOW(),
	org_data = '{
    "Id": 155396,
    "City": "Jeddah",
    "Guid": "407641f9-0fba-400a-a68c-c9d1a778e0f9",
    "Email": "usaid@amscosal.com",
    "label": "Modern Supply Trading Company",
    "value": "FRC00027912",
    "Country": "SAU",
    "Address1": "Al shehri Street , Andalusia Dist , Ibrahim Al-Juffali, Makkah",
    "FullName": "Modern Supply Trading Company",
    "Payables": false,
    "CompanyId": 372,
    "Receivables": true,
    "ZipPostCode": "37748 23326",
    "ForworderAgent": false,
    "OrganizationCode": "FRC00027912",
    "InsertUserIdUsername": "p100serviceaccountegypt@dpworld.com",
    "UpdateUserIdUsername": "p100serviceaccountegypt@dpworld.com"
}',
	address_data = '{
    "Id": 258853,
    "City": "Jeddah",
    "Guid": "f8993dbb-0031-4e8b-be04-3049b92add73",
    "Email": "usaid@amscosal.com",
    "Country": "SAU",
    "OrgGuid": "407641f9-0fba-400a-a68c-c9d1a778e0f9",
    "Address1": "Al shehri Street , Andalusia Dist , Ibrahim Al-Juffali, Makkah",
    "OrgSource": "CRP",
    "AddressType": 1,
    "CompanyName": "Modern Supply Trading Company",
    "OrgFullName": "Modern Supply Trading Company",
    "OrgPayables": false,
    "ZipPostCode": "37748 23326",
    "TaxRegNumber": "310415735200003",
    "KnownConsignor": false,
    "OrgReceivables": true,
    "RegulatedAgent": false,
    "SiteIdentifier": "1917750_001_FF-SA_001_B",
    "OrgActiveClient": true,
    "AddressShortCode": "FRO00028809",
    "OrgOrganizationCode": "FRC00027912"
}'
where
	id = 1067099;

update
	shipment_details
set
	client_dps_address_id = 98522,
	updated_at = NOW()
where
	id = 69870;