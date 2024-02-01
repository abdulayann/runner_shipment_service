package com.dpw.runner.shipment.services.aspects.MultitenancyAspect;

import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Component;
import org.springframework.web.context.annotation.RequestScope;

@Component
@RequestScope
public class TenantContext {
    private ThreadLocal<Integer> currentTenant = new InheritableThreadLocal<>();

    public Integer getCurrentTenant() {
        if (currentTenant.get() == null) {
            setCurrentTenant(1);
        }
        return currentTenant.get();
    }

    public void setCurrentTenant(Integer tenant) {
        currentTenant.set(tenant);
    }

    public void removeTenant() {
        currentTenant.remove();
    }

}