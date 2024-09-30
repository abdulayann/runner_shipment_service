package com.dpw.runner.booking.services.aspects.MultitenancyAspect;

import org.springframework.stereotype.Component;

import java.util.List;

@Component
public class MultiBranchesContext {
    private MultiBranchesContext(){}
    private static ThreadLocal<List<Long>> branches = new InheritableThreadLocal<>();

    public static List<Long> getBranches() {
        return branches.get();
    }

    public static void setBranches(List<Long> tenants) {
        branches.set(tenants);
    }
    public static void removeTenant(){
        branches.remove();
    }

}