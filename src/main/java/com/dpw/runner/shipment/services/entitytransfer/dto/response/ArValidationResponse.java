package com.dpw.runner.shipment.services.entitytransfer.dto.response;

import com.dpw.runner.shipment.services.commons.responses.IRunnerResponse;
import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;

import java.util.UUID;

@Data
@AllArgsConstructor
@NoArgsConstructor
@Builder
public class ArValidationResponse implements IRunnerResponse {
    private String shipmentType;
    private UUID shipmentGuid;
    private String consolidationType;
    private Long receivingAgent;
    private Long triangulationPartner;
    private Integer sourceBranch;
    private Integer destination;
    private Integer origin;
    private String destinationCountry;
    private Boolean transferToReceivingAgent;
    private Boolean transferToTriangulationPartner;
    private UUID originShipmentGuid;
    private UUID receivingAgentShipmentGud;
    private UUID triangulationPartnerShipmentGud;
    private String originShipmentCreatedBy;
    private String receivingAgentShipmentCreatedBy;
    private String triangulationPartnerShipmentCreatedBy;

}
