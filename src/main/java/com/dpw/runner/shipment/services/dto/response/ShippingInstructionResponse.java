package com.dpw.runner.shipment.services.dto.response;

import com.dpw.runner.shipment.services.commons.responses.IRunnerResponse;
import com.dpw.runner.shipment.services.entity.enums.SIStatus;
import io.swagger.annotations.ApiModel;
import lombok.*;

import java.math.BigDecimal;
import java.time.LocalDateTime;
import java.util.List;
import java.util.Map;
import java.util.UUID;

@Data
@Builder
@ApiModel("Shipping Instruction Response Model")
@ToString
@NoArgsConstructor
@AllArgsConstructor
public class ShippingInstructionResponse implements IRunnerResponse {
    private Long id;
    private UUID guid;
    private Long bookingId;
    private SIStatus status;
    private String shipperDeclaredValue;
    private String shipperDeclaredValueCurrency;
    private String blReleaseOffice;
    private Boolean originalSeaway;
    private String originalFreightedCopies;
    private String originalUnFreightedCopies;
    private String seawayExpressFreightedCopies;
    private String seawayExpressUnFreightedCopies;
    private String blComments;
    private LocalDateTime reqDateOfIssue;
    private String placeOfReceipt;
    private String placeOfDelivery;
    private String portOfLoad;
    private String portOfDischarge;
    private CarrierDetailResponse carrierDetails;
    private String consignorReferenceNumber;
    private String consigneeReferenceNumber;
    private String exporterReferenceNumber;
    private String purchaseOrderNumber;
    private String bLReferenceNumber;
    private String consignorTaxId;
    private String consigneeTaxId;
    private String consignorName;
    private String consigneeName;
    private String consignorAddress;
    private String consigneeAddress;
    private String noOfCopies;
    private String reason;
    private String bLObjectStatus;
    private String userDefinedClauses;
    private List<ContainerResponse> containersList;
    private List<PackingResponse> packingList;
    private String totalContainerCount;
    private String totalPackageCount;
    private String totalShipmentWeight;
    private BigDecimal totalShipmentVolume;
    private List<EventsResponse> eventsList;
    private Boolean isSyncWithBlObject;
    private Map<String, String> masterData;
}
