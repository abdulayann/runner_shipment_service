package com.dpw.runner.shipment.services.syncing.Entity;

import com.dpw.runner.shipment.services.commons.requests.IRunnerRequest;
import com.fasterxml.jackson.annotation.JsonProperty;
import lombok.Data;

@Data
public class HawbLockDto implements IRunnerRequest {
    @JsonProperty("ShipperNameLock")
    private Boolean ShipperNameLock;
    @JsonProperty("ShipperAddressLock")
    private Boolean ShipperAddressLock;
    @JsonProperty("ConsigneeNameLock")
    private Boolean ConsigneeNameLock;
    @JsonProperty("ConsigneeAddressLock")
    private Boolean ConsigneeAddressLock;
    @JsonProperty("IssuingAgentNameLock")
    private Boolean IssuingAgentNameLock;
    @JsonProperty("IssuingAgentAddressLock")
    private Boolean IssuingAgentAddressLock;
    @JsonProperty("IataCodeLock")
    private Boolean IataCodeLock;
    @JsonProperty("AwbNumberLock")
    private Boolean AwbNumberLock;
    @JsonProperty("NotifyPartyLock")
    private Boolean NotifyPartyLock;
    @JsonProperty("RoutingDetailsLock")
    private Boolean RoutingDetailsLock;
    @JsonProperty("AccountingInfoLock")
    private Boolean AccountingInfoLock;
    @JsonProperty("HandlingInfoLock")
    private Boolean HandlingInfoLock;
    @JsonProperty("OtherInfoLock")
    private Boolean OtherInfoLock;
    @JsonProperty("NtrQtyGoodsLock")
    private Boolean NtrQtyGoodsLock;
    @JsonProperty("SpecialHandlingCodesLock")
    private Boolean SpecialHandlingCodesLock;
    @JsonProperty("ReferenceNumberLock")
    private Boolean ReferenceNumberLock;
    @JsonProperty("ShippingInformationLock")
    private Boolean ShippingInformationLock;
    @JsonProperty("SciLock")
    private Boolean SciLock;
    @JsonProperty("CurrencyLock")
    private Boolean CurrencyLock;
    @JsonProperty("ChargeCodeLock")
    private Boolean ChargeCodeLock;
    @JsonProperty("CarriageValueLock")
    private Boolean CarriageValueLock;
    @JsonProperty("CustomsValueLock")
    private Boolean CustomsValueLock;
    @JsonProperty("InsuranceAmountLock")
    private Boolean InsuranceAmountLock;
    @JsonProperty("CustomOriginCodeLock")
    private Boolean CustomOriginCodeLock;
    @JsonProperty("GoodsDescriptionLock")
    private Boolean GoodsDescriptionLock;
    @JsonProperty("PackingLock")
    private Boolean PackingLock;
    @JsonProperty("PrepaidDueAgentChargesLock")
    private Boolean PrepaidDueAgentChargesLock;
    @JsonProperty("PrepaidDueCarrierChargesLock")
    private Boolean PrepaidDueCarrierChargesLock;
    @JsonProperty("TotalPrepaidLock")
    private Boolean TotalPrepaidLock;
    @JsonProperty("CollectWeightChargesLock")
    private Boolean CollectWeightChargesLock;
    @JsonProperty("CollectValuationChargeLock")
    private Boolean CollectValuationChargeLock;
    @JsonProperty("CollectTaxLock")
    private Boolean CollectTaxLock;
    @JsonProperty("TotalCollectLock")
    private Boolean TotalCollectLock;
    @JsonProperty("OtherChargesLock")
    private Boolean OtherChargesLock;
    @JsonProperty("ShipperLock")
    private Boolean ShipperLock;
    @JsonProperty("CarrierLock")
    private Boolean CarrierLock;
    @JsonProperty("ExecutedAtLock")
    private Boolean ExecutedAtLock;
    @JsonProperty("ExecutedOnLock")
    private Boolean ExecutedOnLock;
    @JsonProperty("OtherCustomsLock")
    private Boolean OtherCustomsLock;
    @JsonProperty("NotifyOrganizationLock")
    private Boolean NotifyOrganizationLock;
    @JsonProperty("NotifyOrganizationAddressLock")
    private Boolean NotifyOrganizationAddressLock;
    @JsonProperty("AirportOfDepartureLock")
    private Boolean AirportOfDepartureLock;
    @JsonProperty("AirportOfDestinationLock")
    private Boolean AirportOfDestinationLock;
    @JsonProperty("FirstCarrierLock")
    private Boolean FirstCarrierLock;
    @JsonProperty("OriginPortLock")
    private Boolean OriginPortLock;
    @JsonProperty("DestinationPortLock")
    private Boolean DestinationPortLock;
    @JsonProperty("ByCarrierLock")
    private Boolean ByCarrierLock;
    @JsonProperty("FlightNumberLock")
    private Boolean FlightNumberLock;
    @JsonProperty("FlightDateLock")
    private Boolean FlightDateLock;
    @JsonProperty("InformationIdentifierLock")
    private Boolean InformationIdentifierLock;
    @JsonProperty("TradeIdentificationCodeLock")
    private Boolean TradeIdentificationCodeLock;
    @JsonProperty("TradeIdentificationCommentLock")
    private Boolean TradeIdentificationCommentLock;
    @JsonProperty("ChargeDescriptionLock")
    private Boolean ChargeDescriptionLock;
    @JsonProperty("RateLock")
    private Boolean RateLock;
    @JsonProperty("ChargeBasisLock")
    private Boolean ChargeBasisLock;
    @JsonProperty("ChargesDueLock")
    private Boolean ChargesDueLock;
    @JsonProperty("PiecesNoLock")
    private Boolean PiecesNoLock;
    @JsonProperty("GrossWtLock")
    private Boolean GrossWtLock;
    @JsonProperty("GrossWtUnitLock")
    private Boolean GrossWtUnitLock;
    @JsonProperty("RateClassLock")
    private Boolean RateClassLock;
    @JsonProperty("CommodityItemNoLock")
    private Boolean CommodityItemNoLock;
    @JsonProperty("ChargeableWtLock")
    private Boolean ChargeableWtLock;
    @JsonProperty("RateChargeLock")
    private Boolean RateChargeLock;
    @JsonProperty("TotalAmountLock")
    private Boolean TotalAmountLock;
    @JsonProperty("SlacCodeLock")
    private Boolean SlacCodeLock;
    @JsonProperty("HsCodeLock")
    private Boolean HsCodeLock;
    @JsonProperty("PackingAwbGoodsDescIdLock")
    private Boolean PackingAwbGoodsDescIdLock;
    @JsonProperty("PackingPacksLock")
    private Boolean PackingPacksLock;
    @JsonProperty("PackingPacksTypeLock")
    private Boolean PackingPacksTypeLock;
    @JsonProperty("PackingOriginLock")
    private Boolean PackingOriginLock;
    @JsonProperty("PackingOrderLock")
    private Boolean PackingOrderLock;
    @JsonProperty("PackingLengthLock")
    private Boolean PackingLengthLock;
    @JsonProperty("PackingLengthUnitLock")
    private Boolean PackingLengthUnitLock;
    @JsonProperty("PackingWidthLock")
    private Boolean PackingWidthLock;
    @JsonProperty("PackingWidthUnitLock")
    private Boolean PackingWidthUnitLock;
    @JsonProperty("PackingHeightLock")
    private Boolean PackingHeightLock;
    @JsonProperty("PackingHeightUnitLock")
    private Boolean PackingHeightUnitLock;
    @JsonProperty("PackingWeightLock")
    private Boolean PackingWeightLock;
    @JsonProperty("PackingWeightUnitLock")
    private Boolean PackingWeightUnitLock;
    @JsonProperty("PackingVolumeLock")
    private Boolean PackingVolumeLock;
    @JsonProperty("PackingVolumeUnitLock")
    private Boolean PackingVolumeUnitLock;
    @JsonProperty("PackingNetWeightLock")
    private Boolean PackingNetWeightLock;
    @JsonProperty("PackingNetWeightUnitLock")
    private Boolean PackingNetWeightUnitLock;
    @JsonProperty("PackingVolumeWeightLock")
    private Boolean PackingVolumeWeightLock;
    @JsonProperty("PackingVolumeWeightUnitLock")
    private Boolean PackingVolumeWeightUnitLock;
    @JsonProperty("PackingMarksnNumsLock")
    private Boolean PackingMarksnNumsLock;
    @JsonProperty("PackingCountryCodeLock")
    private Boolean PackingCountryCodeLock;
    @JsonProperty("PackingGoodsDescLock")
    private Boolean PackingGoodsDescLock;
    @JsonProperty("PackingReferenceNumberLock")
    private Boolean PackingReferenceNumberLock;
    @JsonProperty("PackingInspectionsLock")
    private Boolean PackingInspectionsLock;
    @JsonProperty("PackingDgClassLock")
    private Boolean PackingDgClassLock;
    @JsonProperty("PackingDgSubstanceIdLock")
    private Boolean PackingDgSubstanceIdLock;
    @JsonProperty("PackingMinTempLock")
    private Boolean PackingMinTempLock;
    @JsonProperty("PackingMinTempUnitLock")
    private Boolean PackingMinTempUnitLock;
    @JsonProperty("PackingMaxTempLock")
    private Boolean PackingMaxTempLock;
    @JsonProperty("PackingMaxTempUnitLock")
    private Boolean PackingMaxTempUnitLock;
    @JsonProperty("PackingCommodityLock")
    private Boolean PackingCommodityLock;
    @JsonProperty("PackingHsCodeLock")
    private Boolean PackingHsCodeLock;
}
