-- Shipment ID : (80952)

-- Update Parties
update
    parties
set
    org_code = 'FRC00027378',
    address_code = 'FRO00028254',
    org_id = '164339',
    address_id = '282270',
    updated_at = NOW(),
    org_data = '{
                   "Id":164339,
                   "Guid":"3d7aad33-b1c4-45b1-a801-66a9ea636e2d",
                   "OrganizationCode":"FRC00027378",
                   "FullName":"Marchese Spedition + Transporte GmbH",
                   "Address1":"Messeplatz 10",
                   "Country":"CHE",
                   "City":"Basel",
                   "State":"BS",
                   "ZipPostCode":"4058",
                   "Email":"liborio@msped.ch",
                   "ForworderAgent":false,
                   "Receivables":true,
                   "Payables":false,
                   "CompanyId":372,
                   "InsertUserIdUsername":"p100serviceaccountegypt@dpworld.com",
                   "UpdateUserIdUsername":"p100serviceaccountegypt@dpworld.com"
                }',
    address_data = '{
                       "Id":282270,
                       "Guid":"7a31eb2e-85a1-41e7-b750-bddeac53cd8e",
                       "AddressShortCode":"FRO00028254",
                       "CompanyName":"Marchese Spedition + Transporte GmbH",
                       "AddressType":1,
                       "SiteIdentifier":"2050708_001_CHTWL_001_B",
                       "Address1":"Messeplatz 10",
                       "Country":"CHE",
                       "City":"Basel",
                       "State":"BS",
                       "ZipPostCode":"4058",
                       "Email":"liborio@msped.ch",
                       "OrgGuid":"3d7aad33-b1c4-45b1-a801-66a9ea636e2d",
                       "OrgOrganizationCode":"FRC00027378",
                       "OrgSource":"CRP",
                       "OrgFullName":"Marchese Spedition + Transporte GmbH",
                       "OrgActiveClient":true,
                       "OrgReceivables":true,
                       "OrgPayables":false,
                       "TaxRegNumber":"CHE497141646MWST",
                       "RegulatedAgent":false,
                       "KnownConsignor":false
                    }'
where
    id = 1261638 and tenant_id = 685;

-- Update Shipments
UPDATE shipment_details
SET
    client_dps_address_id = 111524,
    updated_at = NOW()
WHERE
    id = 80952
    AND tenant_id = 685;


-- Shipment ID : (80953)

-- Update Parties
update
    parties
set
    org_code = 'FRC00027378',
    address_code = 'FRO00028254',
    org_id = '164339',
    address_id = '282270',
    updated_at = NOW(),
    org_data = '{
                   "Id":164339,
                   "Guid":"3d7aad33-b1c4-45b1-a801-66a9ea636e2d",
                   "OrganizationCode":"FRC00027378",
                   "FullName":"Marchese Spedition + Transporte GmbH",
                   "Address1":"Messeplatz 10",
                   "Country":"CHE",
                   "City":"Basel",
                   "State":"BS",
                   "ZipPostCode":"4058",
                   "Email":"liborio@msped.ch",
                   "ForworderAgent":false,
                   "Receivables":true,
                   "Payables":false,
                   "CompanyId":372,
                   "InsertUserIdUsername":"p100serviceaccountegypt@dpworld.com",
                   "UpdateUserIdUsername":"p100serviceaccountegypt@dpworld.com"
                }',
    address_data = '{
                       "Id":282270,
                       "Guid":"7a31eb2e-85a1-41e7-b750-bddeac53cd8e",
                       "AddressShortCode":"FRO00028254",
                       "CompanyName":"Marchese Spedition + Transporte GmbH",
                       "AddressType":1,
                       "SiteIdentifier":"2050708_001_CHTWL_001_B",
                       "Address1":"Messeplatz 10",
                       "Country":"CHE",
                       "City":"Basel",
                       "State":"BS",
                       "ZipPostCode":"4058",
                       "Email":"liborio@msped.ch",
                       "OrgGuid":"3d7aad33-b1c4-45b1-a801-66a9ea636e2d",
                       "OrgOrganizationCode":"FRC00027378",
                       "OrgSource":"CRP",
                       "OrgFullName":"Marchese Spedition + Transporte GmbH",
                       "OrgActiveClient":true,
                       "OrgReceivables":true,
                       "OrgPayables":false,
                       "TaxRegNumber":"CHE497141646MWST",
                       "RegulatedAgent":false,
                       "KnownConsignor":false
                    }'
where
    id = 1261648 and tenant_id = 685;

-- Update Shipments
UPDATE shipment_details
SET
    client_dps_address_id = 111524,
    updated_at = NOW()
WHERE
    id = 80953
    AND tenant_id = 685;