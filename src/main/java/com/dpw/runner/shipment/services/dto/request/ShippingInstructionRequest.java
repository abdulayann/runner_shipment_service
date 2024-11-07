package com.dpw.runner.shipment.services.dto.request;

import com.dpw.runner.shipment.services.commons.requests.CommonRequest;
import com.dpw.runner.shipment.services.commons.requests.IRunnerRequest;
import com.dpw.runner.shipment.services.entity.enums.SIStatus;
import io.swagger.annotations.ApiModel;
import lombok.*;

import java.math.BigDecimal;
import java.time.LocalDateTime;
import java.util.List;

@Data
@Builder
@ApiModel("Shipping Instruction Request Model")
@ToString
@AllArgsConstructor
@NoArgsConstructor
public class ShippingInstructionRequest extends CommonRequest implements IRunnerRequest {
    private Long id;
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
    private CarrierDetailRequest carrierDetailRequest;
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
    private List<ContainerRequest> containersList;
    private List<PackingRequest> packingList;
    private String totalContainerCount;
    private String totalPackageCount;
    private String totalShipmentWeight;
    private BigDecimal totalShipmentVolume;
    private List<EventsRequest> eventsList;
    private Boolean isSyncWithBlObject;
}
