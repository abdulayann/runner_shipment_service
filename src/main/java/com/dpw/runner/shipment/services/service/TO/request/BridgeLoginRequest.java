package com.dpw.runner.shipment.services.service.TO.request;

import lombok.*;

@Getter
@Setter
@AllArgsConstructor
@NoArgsConstructor
@Builder
public class BridgeLoginRequest {
    private String tenantCode;
    private String username;
    private String password;

}
