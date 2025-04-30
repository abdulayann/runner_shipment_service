package com.dpw.runner.shipment.services.dto.response;
import com.fasterxml.jackson.annotation.JsonIgnore;
import lombok.AllArgsConstructor;
import lombok.Data;
import lombok.NoArgsConstructor;
import java.util.List;
import java.util.Map;
import java.util.UUID;

@NoArgsConstructor
@AllArgsConstructor
@Data
public class ShipmentPendingNotificationResponse extends ShipmentDetailsResponse {
    private Long id;
    private UUID guid;
    private Integer tenantId;
    private String transportMode;
    private Boolean containsHazardous;
    private Long receivingBranch;
    private Map<String, Object> tenantMasterData;
    private Long sourceTenantId;
    private Long documentationPartner;
    private Long triangulationPartner;
    private List<TriangulationPartnerResponse> triangulationPartnerList;
    @JsonIgnore
    private Boolean cargoFinanceBooking;
    @JsonIgnore
    private Boolean isShipmentReadOnly;
    @JsonIgnore
    private Boolean intraBranch;
}