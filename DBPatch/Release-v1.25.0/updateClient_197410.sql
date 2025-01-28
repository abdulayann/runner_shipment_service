update parties set
org_code = 'FRC00020282',
address_code = 'FRO00020896',
org_data = '{"Id":135952,"OrganizationCode":"FRC00020282","Receivables":true,"Email":"markus.fruehauf@cli.ag","ZipPostCode":"50827","Payables":false,"City":"Cologne","Guid":"e4a7a241-c9a0-43f9-b664-537572188ed3","Address1":"Koehlstrasse 10","ForworderAgent":false,"State":"NW","FullName":"CLI Transport GmbH","Country":"DEU"}',
address_data = '{"AddressShortCode":"FRO00020896","Email":"kontakt@cli.ag","ZipPostCode":"50827","SiteIdentifier":"3817036_B","City":"Cologne","Guid":"bfb9b162-6f97-41ca-8afc-40a2b52060fb","Address1":"Koehlstrasse 10","CompanyName":"CLI Transport GmbH","TaxRegNumber":"DE329858128","State":"Nordrhein-Westfalen","OrgOrganizationCode":"FRC00020282","Country":"DEU","Id":200522,"AddressType":"1"}',
org_id = '135952',
address_id = '200522'
where id in (select client_id from shipment_details where id in ('32015') and tenant_id = '474');
