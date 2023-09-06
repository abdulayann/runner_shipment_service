package com.dpw.runner.shipment.services.dto.request.awb;

import io.swagger.annotations.ApiModel;
import lombok.*;

@Data
@ApiModel("Awb Shipment Info Request Model")
@ToString
@Builder
@NoArgsConstructor
@AllArgsConstructor
public class AwbShipmentInfo {
    private Long entityId;
    private String entityType;
    private String shipperName;
    private String shipperAddress;
    private String shipperReferenceNumber;
    private String consigneeName;
    private String consigneeAddress;
    private String consigneeReferenceNumber;
    private String issuingAgentName;
    private String issuingAgentAddress;
    private String iataCode;
    private String noOfCopies;
    private String status;
    private String awbNumber;
    //TODO- Loc Code
    private String originAirport;
    //TODO- Loc Code
    private String destinationAirport;
    private String firstCarrier;
    private Integer abrId;
    private String agentCASSCode;
}
