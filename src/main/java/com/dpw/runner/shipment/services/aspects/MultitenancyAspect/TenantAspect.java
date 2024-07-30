package com.dpw.runner.shipment.services.aspects.MultitenancyAspect;


import com.dpw.runner.shipment.services.commons.constants.PermissionConstants;
import com.dpw.runner.shipment.services.dto.request.UsersDto;
import org.aspectj.lang.annotation.Aspect;
import org.aspectj.lang.annotation.Before;
import org.hibernate.Session;
import org.springframework.stereotype.Component;

import javax.persistence.EntityManager;
import javax.persistence.PersistenceContext;
import java.util.HashMap;
import java.util.Map;
import java.util.Optional;

@Aspect
@Component
public class TenantAspect {
    @PersistenceContext
    private EntityManager entityManager;

    @Before("execution(* com.dpw.runner.shipment.services.aspects.MultitenancyAspect.MultiTenancyRepository+.*(..))")
    public void beforeFindOfMultiTenancyRepository() {

        long tenantId = TenantContext.getCurrentTenant();

        Map<String, Boolean> permissions = Optional.ofNullable(UserContext.getUser()).map(UsersDto::getPermissions).orElse(new HashMap<>());

        if (!permissions.containsKey(PermissionConstants.tenantSuperAdmin) && !permissions.containsKey(PermissionConstants.crossTenantListPermission) && !permissions.containsKey(PermissionConstants.crossTenantRetrievePermission) && !permissions.containsKey(PermissionConstants.companySuperAdmin)) {
            entityManager.unwrap(Session.class)
                    .enableFilter(MultiTenancy.TENANT_FILTER_NAME)
                    .setParameter(MultiTenancy.TENANT_PARAMETER_NAME, tenantId);
        }
    }
}
