package com.dpw.runner.shipment.services.aspects.MultitenancyAspect;

public class TenantContext {
    private static final ThreadLocal<Integer> currentTenant = new InheritableThreadLocal<>();

    private TenantContext() {
    }

    public static Integer getCurrentTenant() {
        if (currentTenant.get() == null) {
            setCurrentTenant(1);
        }
        return currentTenant.get();
    }

    public static void setCurrentTenant(Integer tenant) {
        currentTenant.set(tenant);
    }

    public static void removeTenant() {
        currentTenant.remove();
    }

}