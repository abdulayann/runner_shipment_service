package com.dpw.runner.shipment.services.migration.utils;

import com.dpw.runner.shipment.services.aspects.MultitenancyAspect.RequestAuthContext;
import com.dpw.runner.shipment.services.aspects.MultitenancyAspect.TenantContext;
import com.dpw.runner.shipment.services.aspects.MultitenancyAspect.TenantSettingsDetailsContext;
import com.dpw.runner.shipment.services.migration.IRun;
import lombok.extern.slf4j.Slf4j;
import org.slf4j.MDC;
import org.springframework.stereotype.Component;

import java.util.Map;
import java.util.concurrent.Callable;

@Slf4j
@Component
public class MigrationUtil {

    public static <V> Callable<V> wrapWithMdc(IRun<V> runnable) {
        Map<String, String> mdc = MDC.getCopyOfContextMap();
        String token = RequestAuthContext.getAuthToken();
        var tenantId = TenantContext.getCurrentTenant();
        return () -> {
            try {
                if(mdc!=null)
                    MDC.setContextMap(mdc);
                RequestAuthContext.setAuthToken(token);
                TenantContext.setCurrentTenant(tenantId);
                return runnable.run();
            } finally {
                MDC.clear();
                RequestAuthContext.removeToken();
                TenantSettingsDetailsContext.remove();
                TenantContext.removeTenant();
            }
        };
    }

}
