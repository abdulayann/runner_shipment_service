package com.dpw.runner.shipment.services.dto.request.hbl;

import lombok.*;

import java.math.BigDecimal;
import java.time.LocalDateTime;
import java.util.UUID;


@Data
@Builder
@ToString
@AllArgsConstructor
@NoArgsConstructor
public class HblDataDto {
    private Long id;
    private UUID guid;
    private Long shipmentId;
    private Integer version;
    private String status;
    private String blType;
    private String consignorName;
    private String consignorAddress;
    private String consignorTaxId;
    private String consignorReferenceNumber;
    private String consigneeName;
    private String consigneeAddress;
    private String consigneeReferenceNumber;
    private String purchaseOrderNumber;
    private String exporterReferenceNumber;
    private String blReferenceNumber;
    private String originOfGoods;
    private String placeOfReceipt;
    private String portOfLoad;
    private String portOfDischarge;
    private String placeOfDelivery;
    private Integer packageCount;
    private String packageType;
    private String hsCode;
    private String scheduleBNumber;
    private String cargoDescription;
    private String marksAndNumbers;
    private String hazmatDetails;
    private BigDecimal cargoGrossWeight;
    private BigDecimal cargoNetWeight;
    private BigDecimal cargoGrossVolume;
    private BigDecimal shipperDeclaredValue;
    private String shipperDeclaredCurrency;
    private String blInstructionType;
    private String blComments;
    private String blTermsAndConditionsId;
    private String noOfCopies;
    private String cargoNetWeightUnit;
    private String cargoGrossVolumeUnit;
    private String cargoGrossWeightUnit;
    private String reason;
    private String houseBill;
    private String voyage;
    private String vesselName;
    private Integer originalSeq;
    private Boolean disableOriginal;
    private String transportType;
    private String shipmentType;
    private LocalDateTime etd;
    private String shippingTime;
    private String incoTerms;
    private String bLRemarks;
    private String cargoTermsDescription;
    private String cargoTerms;
    private String deliveryAgentAddress;
    private String deliveryAgent;
    private String quantityCode;
    private String invoiceNumbers;
    private String jsonFiled;
    private String elDate;
    private String lcNumber;
    private String elNumber;
    private String finalDestination;
    private String paymentTerm;
    private String incoTermPlace;
    private Integer quantity;
    private String blRemarksDescription;
    private String blRemark;
}
