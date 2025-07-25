UPDATE parties
SET
  org_id = '126322',
  org_code = 'FRC00011276',
  address_id = '165181',
  address_code = 'FRO00011591',
  org_data ='{"Id":126322,"City":"Fullerton","Guid":"181dd9b6-c2aa-4b5a-93b0-3d3754c910e6","Email":"c.kruger@proplas.com","IsGSA":false,"Phone":"7144466500","State":"CA","label":"Professional Plastics","value":"FRC00011276","Source":"CRP","Country":"USA","Address1":"1810 E Valencia DR","FullName":"Professional Plastics","IsActive":1,"Payables":false,"TenantId":442,"CompanyId":372,"Consignee":true,"Consigner":true,"PANNumber":"330108234","TaxVendor":false,"ExtraParam":"{}","InsertDate":"2024-07-18T14:29:40.000","UpdateDate":"2024-12-19T06:43:12.000","IsSuspended":false,"Receivables":true,"ZipPostCode":"92831","ActiveClient":true,"CurrencyCode":"USD","EmailInvoice":false,"InsertUserId":1712,"UpdateUserId":1712,"VatRegNumber":"330108234","ForworderAgent":false,"IsCreditEnabled":false,"IsV2PaymentTerm":false,"OrganizationCode":"FRC00011276","CustomerIdentifier":"1517433_001","ReceivableTermsDate":2,"InsertUserIdUsername":"p100serviceaccountegypt@dpworld.com","OrgQuoteValidityDays":0,"UpdateUserIdUsername":"p100serviceaccountegypt@dpworld.com","CreditLimitOfConfigRow":"","CreditTermsOfConfigRow":"","IsClientEInvoiceEnabled":false,"ImportExportClearanceLock":false}',


  address_data = '{"Id":165181,"City":"Fullerton","Email":"c.kruger@proplas.com","State":"California","label":"Professional Plastics, 1810 E Valencia DR, Fullerton, California, USA","value":"FRO00011591","Country":"USA","OrgGuid":"181dd9b6-c2aa-4b5a-93b0-3d3754c910e6","Address1":"1810 E Valencia DR","OrgSource":"CRP","AddressType":1,"CompanyName":"Professional Plastics","OrgFullName":"Professional Plastics","OrgPayables":false,"ZipPostCode":"92831","TaxRegNumber":"330108234","OrgReceivables":true,"SiteIdentifier":"1517433_001_FF-US_001_B","OrgActiveClient":true,"AddressShortCode":"FRO00011591","OrgOrganizationCode":"FRC00011276"}'
WHERE id = '1245263';



update shipment_details
set client_dps_address_id = 96373
where id = 80011;

