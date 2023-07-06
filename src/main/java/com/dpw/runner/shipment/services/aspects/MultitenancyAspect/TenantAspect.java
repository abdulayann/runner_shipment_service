package com.dpw.runner.shipment.services.aspects.MultitenancyAspect;


import com.dpw.runner.shipment.services.aspects.PermissionsValidationAspect.PermissionsContext;
import org.aspectj.lang.annotation.Aspect;
import org.aspectj.lang.annotation.Before;
import org.hibernate.Session;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Component;

import javax.persistence.EntityManager;
import javax.persistence.PersistenceContext;
import java.util.Collections;
import java.util.HashSet;
import java.util.Set;

import static com.fasterxml.jackson.databind.cfg.CoercionInputShape.Array;

@Aspect
@Component
public class TenantAspect {

    @PersistenceContext
    private EntityManager entityManager;

    private HashSet<String> permissions;

    @Autowired
    private TenantContext tenantContext;

    @Autowired
    private PermissionsContext permissionsContext;

    @Before("execution(* com.dpw.runner.shipment.services.aspects.MultitenancyAspect.MultiTenancyRepository+.*(..))")
    public void beforeFindOfMultiTenancyRepository() {

        long tenantId = tenantContext.getCurrentTenant();
        ;


        permissions = new HashSet<>();
        permissions.add("SuperAdmin");

        if (!permissions.contains("SuperAdmin")) {
            if (permissions.contains("ParentCompanyAdmin")) {
                //fetch tenant ids of parent company from tenants service api
                int[] tenantIdsOfParent = new int[]{1, 2, 3};

                entityManager.unwrap(Session.class)
                        .enableFilter(MultiTenancy.MULTI_BRANCH_FILTER_NAME)
                        .setParameterList(MultiTenancy.TENANT_PARAMETER_NAME, Collections.singleton(tenantIdsOfParent));
            } else if (permissions.contains("CompanyAdmin")) {
                //fetch tenant ids of current company from tenants service api
                int[] tenantIdsOfCompany = new int[]{1, 2};

                entityManager.unwrap(Session.class)
                        .enableFilter(MultiTenancy.MULTI_BRANCH_FILTER_NAME)
                        .setParameterList(MultiTenancy.TENANT_PARAMETER_NAME, Collections.singleton(tenantIdsOfCompany));
            } else {
                entityManager.unwrap(Session.class)
                        .enableFilter(MultiTenancy.TENANT_FILTER_NAME)
                        .setParameter(MultiTenancy.TENANT_PARAMETER_NAME, tenantId);
            }
        } else {
            entityManager.unwrap(Session.class)
                    .enableFilter(MultiTenancy.TENANT_FILTER_NAME)
                    .setParameter(MultiTenancy.TENANT_PARAMETER_NAME, tenantId);
        }
    }
}
