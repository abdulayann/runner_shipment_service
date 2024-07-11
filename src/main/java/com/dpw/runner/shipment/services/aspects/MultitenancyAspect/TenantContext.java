package com.dpw.runner.shipment.services.aspects.MultitenancyAspect;

public class TenantContext {
    private TenantContext(){}
    private static ThreadLocal<Integer> currentTenant = new InheritableThreadLocal<>();

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