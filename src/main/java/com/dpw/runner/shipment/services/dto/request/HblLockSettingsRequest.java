package com.dpw.runner.shipment.services.dto.request;

import com.dpw.runner.shipment.services.commons.requests.CommonRequest;
import com.dpw.runner.shipment.services.commons.requests.IRunnerRequest;
import io.swagger.v3.oas.annotations.media.Schema;
import lombok.*;

@Getter
@Setter
@Schema(description = "Hbl Lock Settings Request Model")
@ToString
@NoArgsConstructor
@AllArgsConstructor
@Builder
public class HblLockSettingsRequest extends CommonRequest implements IRunnerRequest {

    private Long id;
    private Boolean consignorNameLock;
    private Boolean consignorAddressLock;
    private Boolean consigneeNameLock;
    private Boolean consigneeAddressLock;
    private Boolean notifyPartyNameLock;
    private Boolean notifyPartyAddressLock;
    private Boolean notifyPartyTaxIdLock;
    private Boolean notifyPartyEmailLock;
    private Boolean additionalNotifyPartyNameLock;
    private Boolean additionalNotifyPartyAddressLock;
    private Boolean additionalNotifyPartyTaxIdLock;
    private Boolean additionalNotifyPartyEmailLock;
    private Boolean additionalNotifyPartyName2Lock;
    private Boolean additionalNotifyPartyAddress2Lock;
    private Boolean additionalNotifyPartyTaxId2Lock;
    private Boolean additionalNotifyPartyEmail2Lock;
    private Boolean purchaseOrderNumberLock;
    private Boolean exporterReferenceNumberLock;
    private Boolean blReferenceNumberLock;
    private Boolean originOfGoodsLock;
    private Boolean placeOfReceiptLock;
    private Boolean portOfLoadLock;
    private Boolean portOfDischargeLock;
    private Boolean placeOfDeliveryLock;
    private Boolean packageCountLock;
    private Boolean packageTypeLock;
    private Boolean hsCodeLock;
    private Boolean scheduleBNumberLock;
    private Boolean cargoDescriptionLock;
    private Boolean marksAndNumbersLock;
    private Boolean hazmatDetailsLock;
    private Boolean cargoGrossWeightLock;
    private Boolean cargoNetWeightLock;
    private Boolean cargoGrossVolumeLock;
    private Boolean shipperDeclaredValueLock;
    private Boolean shipperDeclaredCurrencyLock;
    private Boolean blInstructionTypeLock;
    private Boolean blCommentsLock;
    private Boolean consignorTaxIdLock;
    private Boolean consignorReferenceNumberLock;
    private Boolean consigneeTaxIdLock;
    private Boolean consigneeReferenceNumberLock;
    private Boolean numberOfCopiesLock;
    private Boolean grossWeightUnitLock;
    private Boolean weightUnitLock;
    private Boolean volumeUnitLock;
    private Boolean containerTypeLock;
    private Boolean carrierSealNumberLock;
    private Boolean shipperSealNumberLock;
    private Boolean containerGrossWeightLock;
    private Boolean containerGrossWeightUnitLock;
    private Boolean containerGrossVolumeLock;
    private Boolean containerGrossVolumeUnitLock;
    private Boolean containerNumberLock;
    private Boolean blContainerIdLock;
    private Boolean cargoGrossWeightUnitLock;
    private Boolean cargoNetWeightUnitLock;
    private Boolean cargoGrossVolumeUnitLock;
    private Boolean classificationLock;
    private Boolean unNoLock;
    private Boolean imcoNoLock;
    private Boolean containerDescLock;
    private Boolean vesselNameLock;
    private Boolean voyageLock;
    private Boolean houseBillLock;
    private Boolean transportTypeLock;
    private Boolean shipmentTypeLock;
    private Boolean etd;
    private Boolean shippingTime;
    private Boolean incoTerms;
    private Boolean incoTermPlace;
    private Boolean paymentTerm;
    private Boolean finalDestination;
    private Boolean elNumber;
    private Boolean lcNumber;
    private Boolean elDate;
    private Boolean invoiceNumbers;
    private Boolean quantity;
    private Boolean quantityCode;
    private Boolean blDeliveryAgent;
    private Boolean blDeliveryAgentAddress;
    private Boolean bLTermsandConditionsIdLock;
    private Boolean cargoTermsLock;
    private Boolean cargoTermsDescriptionLock;
    private Boolean bLRemarksLock;
    private Boolean bLRemarksDescriptionLock;
}
