package com.dpw.runner.booking.services.dto.response;

import com.dpw.runner.booking.services.commons.responses.IRunnerResponse;
import io.swagger.annotations.ApiModel;
import lombok.*;

import java.util.Map;
import java.util.UUID;

@Data
@Builder
@ApiModel("Parties Response Model")
@ToString
@NoArgsConstructor
@AllArgsConstructor
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
}
