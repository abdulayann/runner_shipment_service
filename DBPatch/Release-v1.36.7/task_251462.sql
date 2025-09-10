-- Query 1: Update for FRC00018279 / FRO00018814
-- Shipment ID : (98555)
-- Update Parties

update
    parties
set
    org_code = 'FRC00018279',
    address_code = 'FRO00018814',
    org_id = '133669',
    address_id = '195049',
    updated_at = NOW(),
    org_data = '{
                    "Id": 133669,
                    "City": "TEKKEKOY",
                    "Guid": "818ab7e8-c8aa-449e-832b-908abcaaea98",
                    "Email": "na@na.com",
                    "State": "Samsun",
                    "label": "SAMPA OTOMOTIV SANAYI VE TICARET A.S",
                    "value": "FRC00018279",
                    "Country": "TUR",
                    "Address1": "ORGANIZE SANAYI BOLGESI,ORGANIZE SANAYI,BULVARI NO.31,",
                    "FullName": "SAMPA OTOMOTIV SANAYI VE TICARET A.S",
                    "Payables": false,
                    "CompanyId": 372,
                    "Receivables": false,
                    "ZipPostCode": "55300",
                    "ForworderAgent": false,
                    "OrganizationCode": "FRC00018279",
                    "InsertUserIdUsername": "p100serviceaccountegypt@dpworld.com"
                }',
    address_data = '{
                        "Id": 195049,
                        "City": "TEKKEKOY",
                        "Guid": "4efc2e4b-0575-4f05-8c9d-e596e5cc94d3",
                        "Email": "na@na.com",
                        "State": "Samsun",
                        "Country": "TUR",
                        "OrgGuid": "818ab7e8-c8aa-449e-832b-908abcaaea98",
                        "Address1": "ORGANIZE SANAYI BOLGESI,ORGANIZE SANAYI,BULVARI NO.31,",
                        "OrgSource": "CRP",
                        "AddressType": 2,
                        "CompanyName": "SAMPA OTOMOTIV SANAYI VE TICARET A.S",
                        "OrgFullName": "SAMPA OTOMOTIV SANAYI VE TICARET A.S",
                        "OrgPayables": false,
                        "ZipPostCode": "55300",
                        "OrgReceivables": false,
                        "OrgActiveClient": true,
                        "AddressShortCode": "FRO00018814",
                        "OrgOrganizationCode": "FRC00018279"
                    }'
where
    id = 1569051 and tenant_id = 474;

-- Update Shipments
UPDATE shipment_details
SET
    client_dps_address_id = 110471,
    updated_at = NOW()
WHERE
    id = 98555
    AND tenant_id = 474;



