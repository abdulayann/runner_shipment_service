update
	parties
set
	org_code = 'FRC00020807',
	address_code = 'FRO00021432',
	org_id = '150518',
	address_id = '247904',
	updated_at = NOW(),
	org_data = '{
    "Id": 150518,
    "City": "Boenen",
    "Guid": "61a517ce-84c9-401e-8899-9a9c0848ef6f",
    "Email": "Patrick.Lamke@kik.de",
    "State": "North Rhine-Westphalia",
    "label": "Kik textilien und non-food gmbh",
    "value": "FRC00020807",
    "Country": "DEU",
    "Address1": "Siemensstrasse 21",
    "FullName": "Kik textilien und non-food gmbh",
    "Payables": false,
    "CompanyId": 372,
    "Receivables": true,
    "ZipPostCode": "59199",
    "ForworderAgent": false,
    "OrganizationCode": "FRC00020807",
    "InsertUserIdUsername": "p100serviceaccountegypt@dpworld.com",
    "UpdateUserIdUsername": "p100serviceaccountegypt@dpworld.com"
}',
	address_data = '{
    "Id": 247904,
    "City": "Boenen",
    "Guid": "ddf48250-c028-4819-b43d-c67dc1d2df04",
    "Email": "Patrick.Lamke@kik.de",
    "State": "North Rhine-Westphalia",
    "Country": "DEU",
    "OrgGuid": "61a517ce-84c9-401e-8899-9a9c0848ef6f",
    "Address1": "Siemensstrasse 21",
    "OrgSource": "CRP",
    "AddressType": 2,
    "CompanyName": "Kik textilien und non-food gmbh",
    "OrgFullName": "Kik textilien und non-food gmbh",
    "OrgPayables": false,
    "ZipPostCode": "59199",
    "TaxRegNumber": "DE811671397",
    "KnownConsignor": false,
    "OrgReceivables": true,
    "RegulatedAgent": false,
    "OrgActiveClient": true,
    "AddressShortCode": "FRO00021432",
    "OrgOrganizationCode": "FRC00020807"
}'
where
	id in (select client_id from shipment_details where id in (72644,72652,74079,83065,80401,81271,81009,83073,74810,79111,74084,72658,77979,76189,72666,75632,76184,76198,76199,75832,
76202,76208,81017,81014,76204,82321,74591,76182,76193,82111,72839,80397,72647,72668,82127,82131,80365,80353,80393,77984,80398,78685,77712,77713));


update
	shipment_details
set
	client_dps_address_id = 84289,
	updated_at = NOW()
where
	id (72644,72652,74079,83065,80401,81271,81009,83073,74810,79111,74084,72658,77979,76189,72666,75632,76184,76198,76199,75832,
76202,76208,81017,81014,76204,82321,74591,76182,76193,82111,72839,80397,72647,72668,82127,82131,80365,80353,80393,77984,80398,78685,77712,77713));