package com.dpw.runner.shipment.services.aspects.MultitenancyAspect;

import org.springframework.stereotype.Component;

import java.util.List;

@Component
public class MultiBranchesContext {
    private static final ThreadLocal<List<Long>> branches = new InheritableThreadLocal<>();

    private MultiBranchesContext() {
    }

    public static List<Long> getBranches() {
        return branches.get();
    }

    public static void setBranches(List<Long> tenants) {
        branches.set(tenants);
    }

    public static void removeTenant() {
        branches.remove();
    }

}