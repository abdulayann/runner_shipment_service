package com.dpw.runner.shipment.services.entitytransfer.dto;

import com.dpw.runner.shipment.services.entitytransfer.common.request.IEntityTranferBaseEntity;
import com.dpw.runner.shipment.services.entitytransfer.enums.RegStatus;
import lombok.*;

import java.util.List;

@Data
@ToString
@Builder
@AllArgsConstructor
@NoArgsConstructor
public class EntityTransferOrganizations implements IEntityTranferBaseEntity {
    public long Id;
    public String OrganizationCode;
    public String OrganizationCategory;
    public String RemoteId;
    public String RemoteIdType;
    public String FullName;
    public String LocalName;
    public String Address1;
    public String Address2;
    public String Country;
    public String City;
    public String State;
    public String ZipPostCode;
    public int Unloco;
    public String UnlocoName;
    public String Phone;
    public String Mobile;
    public String Fax;
    public String Email;
    public Boolean IsParnter;
    public String WebsiteUrl;
    public String Language;
    public String CurrencyCode;
    public Long HomePort;
    public Long HomeAirport;
    public String HomeAirPortName;
    public String HomePortName;
    public String HomePortCountry;
    public String CustomerIdentifier;
    public String PartyIdentifier;
    public String WhitelistedTenantGUID;
    public String PANNumber;
    public Long SalesAgent;
    public String CreatedByAndDate;
    public String AgentIATACode;
    public Long OrgQuoteValidityDays;
    public String AgentCASSCode;
    public String VatRegNumber;
    public RegStatus RegistrationStatus;
    public Boolean ActiveClient;
    public Boolean Consigner;
    public Boolean Consignee;
    public Boolean TransportClient;
    public Boolean IsWalkInCustomer;
    public Boolean IsGSA;
    public Boolean WareHouse;
    public Boolean Carrier;
    public Boolean ForworderAgent;
    public Boolean Broker;
    public Boolean Services;
    public int AgentType;
    public Boolean ImportExportClearanceLock;
    public Boolean Receivables;
    public Boolean Payables;
    public Boolean TaxVendor;
    public String CostInvoiceCurrency;
    public String RevenueInvoiceCurrency;
    public String TaxTreatment;
    public String TaxRegistrationNumber;
    public String PlaceOfSupply;
    public String TaxEmail;
    public String TaxGoods;
    public String TaxKind;
    public String TaxRep;
    public Boolean AirCarrier;
    public Boolean RoadCarrier;
    public Boolean RailCarrier;
    public Boolean SeaCarrier;
    public String StateCode;
    public String StateID;
    public String IECode;
    public String PartyName;
    public String BranchSINumber;
    private List<Long> OrgServices;
}