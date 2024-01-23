package com.dpw.runner.shipment.services.dto.response;

import com.dpw.runner.shipment.services.commons.responses.IRunnerResponse;
import com.dpw.runner.shipment.services.utils.UnlocationData;
import lombok.Data;

import java.util.Map;

@Data
public class AwbShipmentInfoResponse implements IRunnerResponse {
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
    private String originAirport;
    private String destinationAirport;
    private String firstCarrier;
    private Integer abrId;
    private String agentCASSCode;
    // Master Data
    private Map<String, String> masterData;
    private Map<String, String> unlocationData;
    private Map<String, String> commodityMasterData;
}
