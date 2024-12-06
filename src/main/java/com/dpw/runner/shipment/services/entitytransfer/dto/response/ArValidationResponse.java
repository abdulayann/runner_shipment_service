package com.dpw.runner.shipment.services.entitytransfer.dto.response;

import com.dpw.runner.shipment.services.commons.responses.IRunnerResponse;
import java.io.Serializable;
import java.time.LocalDateTime;
import java.util.List;
import java.util.UUID;
import com.dpw.runner.shipment.services.dto.response.TriangulationPartnerResponse;
import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;

@Data
@AllArgsConstructor
@NoArgsConstructor
@Builder
public class ArValidationResponse implements IRunnerResponse {
    private UUID shipmentGuid;
    private String consolidationType;
    private Long receivingAgent;
    private List<TriangulationPartnerResponse> triangulationPartnerList;
    private Long triangulationPartner;
    private Integer sourceBranch;
    private Integer origin;
    private String salesBranch;
    private String destinationCountry;
    private Boolean transferToReceivingAgent;
    private Boolean transferToTriangulationPartner;
    private ProfitShareShipmentData originShipment;
    private ProfitShareShipmentData receivingShipment;
    private ProfitShareShipmentData triangulationShipment;

    @Data
    @Builder
    @AllArgsConstructor
    @NoArgsConstructor
    public static class ProfitShareShipmentData implements Serializable {
        private String shipmentType;
        private String transportMode;
        private UUID shipmentGuid;
        private String createdBy;
        private String hbl;
        private String mbl;
        private String shipmentId;
        private LocalDateTime createdAt;
        private String bookingType;
        private String bookingReference;
        private String status;
        private String jobType;
        private Long orderNumber;
    }
}

