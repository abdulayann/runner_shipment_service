package com.dpw.runner.shipment.services.dto.response;

import com.dpw.runner.shipment.services.commons.responses.IRunnerResponse;
import lombok.Data;

import java.util.Map;

@Data
public class AwbShipmentInfoResponse implements IRunnerResponse {
    private Long entityId;
    private String entityType;
    private String shipperName;
    private String shipperAddress;
    private String shipperCountry;
    private String shipperState;
    private String shipperCity;
    private String shipperZipCode;
    private String shipperPhone;
    private String shipperReferenceNumber;
    private String consigneeName;
    private String consigneeAddress;
    private String consigneeCountry;
    private String consigneeState;
    private String consigneeCity;
    private String consigneeZipCode;
    private String consigneePhone;
    private String consigneeReferenceNumber;
    private String issuingAgentName;
    private String issuingAgentAddress;
    private String issuingAgentCountry;
    private String issuingAgentState;
    private String issuingAgentCity;
    private String issuingAgentZipCode;
    private String issuingAgentPhone;
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
    private String shipperAccountNumber;
    private String consigneeAccountNumber;
    private String accountNumber;
}
