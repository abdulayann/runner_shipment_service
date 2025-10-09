package com.dpw.runner.shipment.services.dto.request;

import com.dpw.runner.shipment.services.commons.requests.CommonRequest;
import com.dpw.runner.shipment.services.commons.requests.IRunnerRequest;
import io.swagger.v3.oas.annotations.media.Schema;
import lombok.*;

import java.util.Map;

@Data
@Schema(description = "Parties Request Model")
@ToString
@NoArgsConstructor
@AllArgsConstructor
@Builder
@SuppressWarnings("java:S1948")
public class PartiesRequest extends CommonRequest implements IRunnerRequest {
    private Long id;
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
    private String countryCode;
}
