package com.dpw.runner.shipment.services.dto.v1.response;

import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;

import java.util.List;

@Data
@Builder
@AllArgsConstructor
@NoArgsConstructor
public class V1TenantSettingsResponse {
    private List<String> RestrictedItemsForCreditLimit;
    private Boolean EnableCreditLimitManagement;
    private Boolean IsCreditLimitWithFusionEnabled;
    private int CreditLimitOn;
    private Boolean IsGlobalFusionIntegrationEnabled;
    private String BusinessUnitName;
}
