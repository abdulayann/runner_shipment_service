package com.dpw.runner.shipment.services.syncing.Entity;

import com.dpw.runner.shipment.services.commons.requests.IRunnerRequest;
import com.fasterxml.jackson.annotation.JsonProperty;
import lombok.Data;

@Data
public class HblLockDto implements IRunnerRequest {
    @JsonProperty("ConsignorNameLock")
    private Boolean ConsignorNameLock;
    @JsonProperty("ConsignorAddressLock")
    private Boolean ConsignorAddressLock;
    @JsonProperty("ConsigneeNameLock")
    private Boolean ConsigneeNameLock;
    @JsonProperty("ConsigneeAddressLock")
    private Boolean ConsigneeAddressLock;
    @JsonProperty("NotifyPartyNameLock")
    private Boolean NotifyPartyNameLock;
    @JsonProperty("NotifyPartyAddressLock")
    private Boolean NotifyPartyAddressLock;
    @JsonProperty("NotifyPartyTaxIdLock")
    private Boolean NotifyPartyTaxIdLock;
    @JsonProperty("NotifyPartyEmailLock")
    private Boolean NotifyPartyEmailLock;
    @JsonProperty("AdditionalNotifyPartyNameLock")
    private Boolean AdditionalNotifyPartyNameLock;
    @JsonProperty("AdditionalNotifyPartyAddressLock")
    private Boolean AdditionalNotifyPartyAddressLock;
    @JsonProperty("AdditionalNotifyPartyTaxIdLock")
    private Boolean AdditionalNotifyPartyTaxIdLock;
    @JsonProperty("AdditionalNotifyPartyEmailLock")
    private Boolean AdditionalNotifyPartyEmailLock;
    @JsonProperty("AdditionalNotifyPartyName2Lock")
    private Boolean AdditionalNotifyPartyName2Lock;
    @JsonProperty("AdditionalNotifyPartyAddress2Lock")
    private Boolean AdditionalNotifyPartyAddress2Lock;
    @JsonProperty("AdditionalNotifyPartyTaxId2Lock")
    private Boolean AdditionalNotifyPartyTaxId2Lock;
    @JsonProperty("AdditionalNotifyPartyEmail2Lock")
    private Boolean AdditionalNotifyPartyEmail2Lock;
    @JsonProperty("PurchaseOrderNumberLock")
    private Boolean PurchaseOrderNumberLock;
    @JsonProperty("ExporterReferenceNumberLock")
    private Boolean ExporterReferenceNumberLock;
    @JsonProperty("BlReferenceNumberLock")
    private Boolean BlReferenceNumberLock;
    @JsonProperty("OriginOfGoodsLock")
    private Boolean OriginOfGoodsLock;
    @JsonProperty("PlaceOfReceiptLock")
    private Boolean PlaceOfReceiptLock;
    @JsonProperty("PortOfLoadLock")
    private Boolean PortOfLoadLock;
    @JsonProperty("PortOfDischargeLock")
    private Boolean PortOfDischargeLock;
    @JsonProperty("PlaceOfDeliveryLock")
    private Boolean PlaceOfDeliveryLock;
    @JsonProperty("PackageCountLock")
    private Boolean PackageCountLock;
    @JsonProperty("PackageTypeLock")
    private Boolean PackageTypeLock;
    @JsonProperty("HsCodeLock")
    private Boolean HsCodeLock;
    @JsonProperty("ScheduleBNumberLock")
    private Boolean ScheduleBNumberLock;
    @JsonProperty("CargoDescriptionLock")
    private Boolean CargoDescriptionLock;
    @JsonProperty("MarksAndNumbersLock")
    private Boolean MarksAndNumbersLock;
    @JsonProperty("HazmatDetailsLock")
    private Boolean HazmatDetailsLock;
    @JsonProperty("CargoGrossWeightLock")
    private Boolean CargoGrossWeightLock;
    @JsonProperty("CargoNetWeightLock")
    private Boolean CargoNetWeightLock;
    @JsonProperty("CargoGrossVolumeLock")
    private Boolean CargoGrossVolumeLock;
    @JsonProperty("ShipperDeclaredValueLock")
    private Boolean ShipperDeclaredValueLock;
    @JsonProperty("ShipperDeclaredCurrencyLock")
    private Boolean ShipperDeclaredCurrencyLock;
    @JsonProperty("BlInstructionTypeLock")
    private Boolean BlInstructionTypeLock;
    @JsonProperty("BlCommentsLock")
    private Boolean BlCommentsLock;
    @JsonProperty("ConsignorTaxIdLock")
    private Boolean ConsignorTaxIdLock;
    @JsonProperty("ConsignorReferenceNumberLock")
    private Boolean ConsignorReferenceNumberLock;
    @JsonProperty("ConsigneeTaxIdLock")
    private Boolean ConsigneeTaxIdLock;
    @JsonProperty("ConsigneeReferenceNumberLock")
    private Boolean ConsigneeReferenceNumberLock;
    @JsonProperty("NumberOfCopiesLock")
    private Boolean NumberOfCopiesLock;
    @JsonProperty("GrossWeightUnitLock")
    private Boolean GrossWeightUnitLock;
    @JsonProperty("WeightUnitLock")
    private Boolean WeightUnitLock;
    @JsonProperty("VolumeUnitLock")
    private Boolean VolumeUnitLock;
    @JsonProperty("ContainerTypeLock")
    private Boolean ContainerTypeLock;
    @JsonProperty("CarrierSealNumberLock")
    private Boolean CarrierSealNumberLock;
    @JsonProperty("ShipperSealNumberLock")
    private Boolean ShipperSealNumberLock;
    @JsonProperty("ContainerGrossWeightLock")
    private Boolean ContainerGrossWeightLock;
    @JsonProperty("ContainerGrossWeightUnitLock")
    private Boolean ContainerGrossWeightUnitLock;
    @JsonProperty("ContainerGrossVolumeLock")
    private Boolean ContainerGrossVolumeLock;
    @JsonProperty("ContainerGrossVolumeUnitLock")
    private Boolean ContainerGrossVolumeUnitLock;
    @JsonProperty("ContainerNumberLock")
    private Boolean ContainerNumberLock;
    @JsonProperty("BlContainerIdLock")
    private Boolean BlContainerIdLock;
    @JsonProperty("CargoGrossWeightUnitLock")
    private Boolean CargoGrossWeightUnitLock;
    @JsonProperty("CargoNetWeightUnitLock")
    private Boolean CargoNetWeightUnitLock;
    @JsonProperty("CargoGrossVolumeUnitLock")
    private Boolean CargoGrossVolumeUnitLock;
    @JsonProperty("ClassificationLock")
    private Boolean ClassificationLock;
    @JsonProperty("UnNoLock")
    private Boolean UnNoLock;
    @JsonProperty("ImcoNoLock")
    private Boolean ImcoNoLock;
    @JsonProperty("ContainerDescLock")
    private Boolean ContainerDescLock;
    @JsonProperty("VesselNameLock")
    private Boolean VesselNameLock;
    @JsonProperty("VoyageLock")
    private Boolean VoyageLock;
    @JsonProperty("HouseBillLock")
    private Boolean HouseBillLock;
    @JsonProperty("TransportTypeLock")
    private Boolean TransportTypeLock;
    @JsonProperty("ShipmentTypeLock")
    private Boolean ShipmentTypeLock;
    @JsonProperty("Etd")
    private Boolean Etd;
    @JsonProperty("ShippingTime")
    private Boolean ShippingTime;
    @JsonProperty("IncoTerms")
    private Boolean IncoTerms;
    @JsonProperty("IncoTermPlace")
    private Boolean IncoTermPlace;
    @JsonProperty("PaymentTerm")
    private Boolean PaymentTerm;
    @JsonProperty("FinalDestination")
    private Boolean FinalDestination;
    @JsonProperty("ElNumber")
    private Boolean ElNumber;
    @JsonProperty("LcNumber")
    private Boolean LcNumber;
    @JsonProperty("ElDate")
    private Boolean ElDate;
    @JsonProperty("InvoiceNumbers")
    private Boolean InvoiceNumbers;
    @JsonProperty("Quantity")
    private Boolean Quantity;
    @JsonProperty("QuantityCode")
    private Boolean QuantityCode;
    @JsonProperty("BlDeliveryAgent")
    private Boolean BlDeliveryAgent;
    @JsonProperty("BlDeliveryAgentAddress")
    private Boolean BlDeliveryAgentAddress;
    @JsonProperty("BLTermsandConditionsIdLock")
    private Boolean BLTermsandConditionsIdLock;
    @JsonProperty("CargoTermsLock")
    private Boolean CargoTermsLock;
    @JsonProperty("CargoTermsDescriptionLock")
    private Boolean CargoTermsDescriptionLock;
    @JsonProperty("BLRemarksLock")
    private Boolean BLRemarksLock;
    @JsonProperty("BLRemarksDescriptionLock")
    private Boolean BLRemarksDescriptionLock;
}
