package com.dpw.runner.shipment.services.entitytransfer.dto.response;

import com.dpw.runner.shipment.services.commons.responses.IRunnerResponse;
import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;

import java.util.List;
import java.util.Map;
import java.util.UUID;

@Data
@AllArgsConstructor
@NoArgsConstructor
@Builder
public class PostArValidationResponse implements IRunnerResponse {
    List<ArValidationData> data;

    @Data
    @AllArgsConstructor
    @NoArgsConstructor
    @Builder
    public static class ArValidationData {
        private String shipmentType;
        private UUID shipmentGuid;
        private String consolidationType;
        private Long receivingAgent;
        private Long triangulationPartner;
        private Integer sourceBranch;
        private Integer destinationReceivingBranch;
        private Integer originReceivingBranch;
        private Integer destinationTriangulationBranch;
        private Integer originTriangulationBranch;
        private String destinationCountry;
        private Boolean transferToReceivingAgent;
        private Boolean transferToTriangulationPartner;

    }
}
