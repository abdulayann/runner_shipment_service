package com.dpw.runner.shipment.services.dto.request.awb;

import com.dpw.runner.shipment.services.utils.UnlocationData;
import com.fasterxml.jackson.annotation.JsonIgnoreProperties;
import io.swagger.annotations.ApiModel;
import lombok.*;

import java.io.Serializable;

@Data
@ApiModel("Awb Shipment Info Request Model")
@ToString
@Builder
@NoArgsConstructor
@AllArgsConstructor
@JsonIgnoreProperties(ignoreUnknown = true)
public class AwbShipmentInfo implements Serializable {
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
    @UnlocationData
    private String originAirport;
    @UnlocationData
    private String destinationAirport;
    private String firstCarrier;
    private Integer abrId;
    private String agentCASSCode;
    private String shipperAccountNumber;
    private String consigneeAccountNumber;
    private String accountNumber;
}
