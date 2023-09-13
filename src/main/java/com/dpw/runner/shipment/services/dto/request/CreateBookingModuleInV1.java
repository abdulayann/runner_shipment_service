package com.dpw.runner.shipment.services.dto.request;

import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;

import java.math.BigDecimal;
import java.util.List;
import java.util.UUID;

@NoArgsConstructor
@AllArgsConstructor
@Data
@Builder
public class CreateBookingModuleInV1 {

    private Boolean IsP100Booking = true;
    private BookingEntity Entity;

    @Data
    @Builder
    @NoArgsConstructor
    @AllArgsConstructor
    public static class BookingEntity {
        private String ContractId;
        private String NotifyPartyCode;
        private String NotifyPartyAddressCode;
        private String Carrier;
        private String VesselName;
        private String Voyage;
        private Long Packs;
        private String PacksUnit;
        private BigDecimal Weight;
        private String WeightUnit;
        private BigDecimal VolumeWeight;
        private String WeightVolumeUnit;
        private BigDecimal Volume;
        private String VolumeUnit;
        private String ReferenceNo;
        private String CreatedDate;
        private String ClientCode;
        private String ClientAddressShortCode;
        private String ConsignerCode;
        private String ConsignerAddressCode;
        private String ConsigneeCode;
        private String ConsigneeAddressCode;
        private String OriginCode;
        private String DestinationCode;
        private String originPortCode;
        private String DestinationPortCode;
        private String ConsolidationType;
        private String TransportMode;
        private String ContainerType;
        private String CustomShipmentType;
        private String Incoterm;
        private String ServiceMode;
        private Boolean IsShipmentCreateEnabled;
        private Boolean IsConsolidationCreateEnabled;
        private String BookingType;
        private String Status;
        private String FmcTlcId;

        private List<QuoteContainer> QuoteContainers;
        private List<Routing> RoutingList;
        private List<Document> Documents;
        private List<LooseCargo> Loosecargos;
        private List<OrgDetail> OrgDetails;
        private List<BillCharge> BillCharges;

        @Data
        @Builder
        @NoArgsConstructor
        @AllArgsConstructor
        public static class BillCharge {
            private String ChargeTypeCode;
            private Boolean NoGST;
            private BigDecimal EstimatedRevenue;
            private BigDecimal LocalSellAmount;
            private String LocalSellCurrency;
            private BigDecimal OverseasSellAmount;
            private String OverseasSellCurrency;
            private BigDecimal SellExchange;
            private BigDecimal EstimatedCost;
            private BigDecimal LocalCostAmount;
            private String LocalCostCurrency;
            private BigDecimal CostExchange;
            private BigDecimal OverseasCostAmount;
            private String OverseasCostCurrency;
            private BigDecimal TaxPercentage;
            private List<UUID> ContainersGuid;
            private BigDecimal RevenueLineTotal;
            private BigDecimal OverseasTax;
            private BigDecimal TaxType1;
            private BigDecimal TaxType2;
            private BigDecimal TaxType3;
            private BigDecimal TaxType4;
            private BigDecimal CostTaxType4;
            private BigDecimal CostTaxType3;
            private BigDecimal CostTaxType2;
            private BigDecimal CostTaxType1;
            private BigDecimal CostLineTotal;
            private BigDecimal CostLocalTax;
            private BigDecimal CostOverseasTax;
            private BigDecimal CostTaxPercentage;
            private String EstimatedCostCurrency;
            private String EstimatedRevenueCurrency;
            private BigDecimal CurrentCostRate;
            private BigDecimal CurrentSellRate;
            private BigDecimal LocalTax;
            private String DebtorCode;
            private String CreditorCode;
            private String DebitorAddressCode;
            private String CreditorAddressCode;
        }

        @Data
        @Builder
        @NoArgsConstructor
        @AllArgsConstructor
        public static class QuoteContainer {
            private String ContainerTypeCode;
            private Long Count;
            private String CommodityCode;
            private BigDecimal Weight;
            private String WeightUnit;
            private UUID ReferenceGuid;
        }

        @Data
        @Builder
        @NoArgsConstructor
        @AllArgsConstructor
        public static class Routing {
            private UUID ReferenceGuid;
            private Long Leg;
            private String Mode;
            private String PolCode;
            private String PodCode;
        }

        @Data
        @Builder
        @NoArgsConstructor
        @AllArgsConstructor
        public static class Document {
            private String DocType;
            private String FileName;
            private String Path;
            private Boolean ClientEnabled;
            private String EventCode;
        }

        @Data
        @Builder
        @NoArgsConstructor
        @AllArgsConstructor
        public static class LooseCargo {
            private UUID ReferenceGuid;
            private Long Packs;
            private String PacksUnit;
            private BigDecimal Length;
            private BigDecimal Height;
            private BigDecimal Width;
            private String Du;
            private BigDecimal Weight;
            private String WeightUnit;
            private BigDecimal Volume;
            private String VolumeUnit;
            private BigDecimal Chargeable;
            private String ChargeableUnit;
            private String GoodsDescription;
            private String CommodityCode;
            private Boolean HazardousCheckBox;
            private String HsCode;
        }

        @Data
        @Builder
        @NoArgsConstructor
        @AllArgsConstructor
        public static class OrgDetail {
            private String OrgSource;
            private String OrganizationCode;
            private String FullName;
            private String Address1;
            private String Address2;
            private String Country;
            private String CityCode;
            private String State;
            private String ZipPostCode;
            private String UnlocoCode;
            private String CurrencyCode;
            private String Phone;
            private String Mobile;
            private String Fax;
            private String Email;
            private Boolean ActiveClient;
            private String DefaultAddressSiteIdentifier;
            private Boolean Receivables;
            private Boolean Consigner;
            private Boolean Consignee;
            private List<OrgDetailAddress> Addresses;


            @Builder
            @Data
            @NoArgsConstructor
            @AllArgsConstructor
            public static class OrgDetailAddress {
                private String AddressShortCode;
                private String CompanyName;
                private String SiteIdentifier;
                private String Address1;
                private String Country;
                private String City;
                private String State;
                private String ZipPostCode;
                private String Mobile;
                private String Email;
            }
        }
    }
}
