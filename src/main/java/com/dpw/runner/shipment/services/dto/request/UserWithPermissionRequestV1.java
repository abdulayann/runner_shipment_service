package com.dpw.runner.shipment.services.dto.request;

import com.dpw.runner.shipment.services.masterdata.request.CommonV1ListRequest;
import com.fasterxml.jackson.annotation.JsonProperty;
import lombok.*;

import java.util.List;

@Data
@ToString
@NoArgsConstructor
@AllArgsConstructor
public class UserWithPermissionRequestV1 extends CommonV1ListRequest {
    @JsonProperty("permissionKeys")
    private List<String> permissionKeys;
    @JsonProperty("userTenantId")
    private Integer userTenantId;
}
