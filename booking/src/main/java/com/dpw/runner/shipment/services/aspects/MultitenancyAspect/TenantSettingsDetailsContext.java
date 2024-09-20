package com.dpw.runner.shipment.services.aspects.MultitenancyAspect;

import com.dpw.runner.shipment.services.dto.v1.response.V1TenantSettingsResponse;
import org.springframework.stereotype.Component;

@Component
public class TenantSettingsDetailsContext {
    private TenantSettingsDetailsContext(){}

    private static ThreadLocal<V1TenantSettingsResponse> tenantSettingsThreadLocal =
      new InheritableThreadLocal<>();

    public static V1TenantSettingsResponse getCurrentTenantSettings() {
        return tenantSettingsThreadLocal.get();
    }

    public static void setCurrentTenantSettings(V1TenantSettingsResponse tenantSettings) {
         tenantSettingsThreadLocal.set(tenantSettings);
    }

    public static void remove() {
        tenantSettingsThreadLocal.remove();
    }

}
