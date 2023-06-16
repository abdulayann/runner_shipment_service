package com.dpw.runner.shipment.services.aspects.MultitenancyAspect;

import org.springframework.stereotype.Component;

@Component
public class TenantContext {
    private static ThreadLocal<Integer> currentTenant = new InheritableThreadLocal<>();

    public static Integer getCurrentTenant() {
        return currentTenant.get();
    }

    public static void setCurrentTenant(Integer tenant) {
        currentTenant.set(tenant);
    }
    public static void removeTenant(){
        currentTenant.remove();
    }

}