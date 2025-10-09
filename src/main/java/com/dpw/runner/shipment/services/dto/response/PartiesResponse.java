package com.dpw.runner.shipment.services.dto.response;

import com.dpw.runner.shipment.services.commons.responses.IRunnerResponse;
import com.dpw.runner.shipment.services.dto.v1.response.RAKCDetailsResponse;

import io.swagger.v3.oas.annotations.media.Schema;
import lombok.*;

import java.util.Map;
import java.util.UUID;

@Data
@Builder
@Schema(description = "Parties Response Model")
@ToString
@NoArgsConstructor
@AllArgsConstructor
@SuppressWarnings("java:S1948")
public class PartiesResponse implements IRunnerResponse {
    private Long id;
    private UUID guid;
    private Long entityId;
    private String entityType;
    private String type;
    private String orgCode;
    private Integer tenantId;
    private String addressCode;
    private String orgId;
    private String addressId;
    private Map<String, Object> orgData;
    private Map<String, Object> addressData;
    private Boolean isAddressFreeText;
    private RAKCDetailsResponse rAKCDetails;
    private String countryCode;
    private String message;
}
