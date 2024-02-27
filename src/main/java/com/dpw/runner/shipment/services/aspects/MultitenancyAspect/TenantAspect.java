package com.dpw.runner.shipment.services.aspects.MultitenancyAspect;


import com.dpw.runner.shipment.services.aspects.PermissionsValidationAspect.PermissionsContext;
import com.dpw.runner.shipment.services.commons.constants.PermissionConstants;
import org.aspectj.lang.annotation.Aspect;
import org.aspectj.lang.annotation.Before;
import org.hibernate.Session;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Component;

import javax.persistence.EntityManager;
import javax.persistence.PersistenceContext;
import java.util.Map;

@Aspect
@Component
public class TenantAspect {

    @PersistenceContext
    private EntityManager entityManager;

    @Autowired
    private TenantContext tenantContext;

    @Autowired
    private PermissionsContext permissionsContext;

    @Before("execution(* com.dpw.runner.shipment.services.aspects.MultitenancyAspect.MultiTenancyRepository+.*(..))")
    public void beforeFindOfMultiTenancyRepository() {

        long tenantId = tenantContext.getCurrentTenant();

        Map<String, Boolean> permissions = UserContext.getUser().Permissions;

        if (!permissions.containsKey(PermissionConstants.tenantSuperAdmin) && !permissions.containsKey(PermissionConstants.crossTenantListPermission) && !permissions.containsKey(PermissionConstants.crossTenantRetrievePermission) && !permissions.containsKey(PermissionConstants.companySuperAdmin)) {
            entityManager.unwrap(Session.class)
                    .enableFilter(MultiTenancy.TENANT_FILTER_NAME)
                    .setParameter(MultiTenancy.TENANT_PARAMETER_NAME, tenantId);
        }
    }
}
