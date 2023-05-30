package com.dpw.runner.shipment.services.filter.Multitenancy;


import org.aspectj.lang.annotation.Aspect;
import org.aspectj.lang.annotation.Before;
import org.hibernate.Session;
import org.springframework.stereotype.Component;

import javax.persistence.EntityManager;
import javax.persistence.PersistenceContext;
import java.util.Arrays;
import java.util.Collections;
import java.util.HashSet;

@Aspect
@Component
public class TenantAspect {

    @PersistenceContext
    private EntityManager entityManager;

    private HashSet<String> permissions;

    @Before("execution(* com.dpw.runner.shipment.services.filter.Multitenancy.MultiTenancyRepository+.*(..))")
    public void beforeFindOfMultiTenancyRepository() {

        //fetch current tenantId from the user service api
        long tenantId = 1;

        //fetch permissions from user service api
        permissions = new HashSet<>(Arrays.asList("SuperAdmin", "ParentCompanyAdmin", "CompanyAdmin", "TenantAdmin"));

        if(!permissions.contains("SuperAdmin")) {
            if(permissions.contains("ParentCompanyAdmin"))
            {
                //fetch tenant ids of parent company from tenants service api
                int[] tenantIdsOfParent = new int[]{1,2,3};

                entityManager.unwrap(Session.class)
                .enableFilter(MultiTenancy.MULTI_BRANCH_FILTER_NAME)
                .setParameterList(MultiTenancy.TENANT_PARAMETER_NAME, Collections.singleton(tenantIdsOfParent));
            }
            else if(permissions.contains("CompanyAdmin"))
            {
                //fetch tenant ids of current company from tenants service api
                int[] tenantIdsOfCompany = new int[]{1,2};

                entityManager.unwrap(Session.class)
                        .enableFilter(MultiTenancy.MULTI_BRANCH_FILTER_NAME)
                        .setParameterList(MultiTenancy.TENANT_PARAMETER_NAME, Collections.singleton(tenantIdsOfCompany));
            }
            else
            {
                entityManager.unwrap(Session.class)
                        .enableFilter(MultiTenancy.TENANT_FILTER_NAME)
                        .setParameter(MultiTenancy.TENANT_PARAMETER_NAME, tenantId);
            }
        }
        else
        {
            entityManager.unwrap(Session.class)
                    .enableFilter(MultiTenancy.TENANT_FILTER_NAME)
                    .setParameter(MultiTenancy.TENANT_PARAMETER_NAME, tenantId);
        }
    }
}
