package com.dpw.runner.shipment.services.syncing.Entity;

import lombok.Data;

@Data
public class AwbShipmentInfoV2 {
    public String entityType;
    public String shipperName;
    public String shipperAddress;
    public String shipperReferenceNumber;
    public String consigneeName;
    public String consigneeAddress;
    public String consigneeReferenceNumber;
    public String issuingAgentName;
    public String issuingAgentAddress;
    public String iataCode;
    public String noOfCopies;
    public String status;
    public String awbNumber;
    //TODO- Loc Code
    public String originAirport;
    //TODO- Loc Code
    public String destinationAirport;
    public String firstCarrier;
    public Integer abrId;
    public String agentCASSCode;
}
