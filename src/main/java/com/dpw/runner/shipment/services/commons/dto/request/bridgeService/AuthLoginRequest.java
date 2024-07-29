package com.dpw.runner.shipment.services.commons.dto.request.bridgeService;

import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;

@Data
@Builder
@AllArgsConstructor
@NoArgsConstructor
public class AuthLoginRequest {
    private String tenantCode;
    private String username;
    private String password;
}
