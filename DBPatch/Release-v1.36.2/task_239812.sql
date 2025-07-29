update
	parties
set
	org_code = 'FRC00040454',
	address_code = 'FRO00042235',
	org_id = '158373',
	address_id = '267038',
	updated_at = NOW(),
	org_data =
 '{
      "Id": 158373,
      "City": "Bologna",
      "Guid": "d89e7be7-2750-4af6-9ed5-31199a3d862f",
      "Email": "sales@mylog.it",
      "IsGSA": false,
      "Phone": "3351990892",
      "Source": "CRP",
      "Country": "ITA",
      "Address1": "Interporto di Bologna Blocco 4.4",
      "FullName": "Mylog Srl",
      "IsActive": 1,
      "Payables": false,
      "TenantId": 442,
      "Consignee": true,
      "Consigner": true,
      "PANNumber": "IT02624321200",
      "TaxVendor": false,
      "ExtraParam": "{}",
      "InsertDate": "2025-07-16T15:19:12.000",
      "UpdateDate": "2025-07-16T16:55:53.417",
      "IsSuspended": false,
      "Receivables": true,
      "ZipPostCode": "40010",
      "ActiveClient": true,
      "CurrencyCode": "EUR",
      "EmailInvoice": false,
      "InsertUserId": 1712,
      "VatRegNumber": "IT02624321200",
      "ForworderAgent": false,
      "IsCreditEnabled": false,
      "IsV2PaymentTerm": false,
      "OrganizationCode": "FRC00040454",
      "ReceivableTermsDate": 2,
      "InsertUserIdUsername": "p100serviceaccountegypt@dpworld.com",
      "OrgQuoteValidityDays": 0,
      "IsClientEInvoiceEnabled": false,
      "ImportExportClearanceLock": false
  }',
	address_data =
 '{
      "Id": 267038,
      "City": "Bologna",
      "Email": "sales@mylog.it",
      "Country": "ITA",
      "OrgGuid": "d89e7be7-2750-4af6-9ed5-31199a3d862f",
      "Address1": "Interporto di Bologna Blocco 4.4",
      "OrgSource": "CRP",
      "AddressType": 1,
      "CompanyName": "Mylog Srl",
      "OrgFullName": "Mylog Srl",
      "OrgPayables": false,
      "ZipPostCode": "40010",
      "TaxRegNumber": "IT02624321200",
      "OrgReceivables": true,
      "OrgActiveClient": true,
      "AddressShortCode": "FRO00042235",
      "OrgOrganizationCode": "FRC00040454"
  }'
where
	id = 1273501;


update
	shipment_details
set
	client_dps_address_id = 93638,
	updated_at = NOW()
where
	id = 81651;