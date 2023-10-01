package com.dpw.runner.shipment.services.dto.v1.response;

import lombok.Data;

@Data
public class V1TenantSettingsResponse {
    private boolean EnableIGMDetails;
    private boolean GSTTaxAutoCalculation;
}
