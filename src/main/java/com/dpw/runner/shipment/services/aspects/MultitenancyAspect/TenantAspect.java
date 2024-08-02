package com.dpw.runner.shipment.services.aspects.MultitenancyAspect;


import com.dpw.runner.shipment.services.aspects.intraBranch.InterBranchContext;
import com.dpw.runner.shipment.services.commons.constants.PermissionConstants;
import com.dpw.runner.shipment.services.dto.request.UsersDto;
import com.dpw.runner.shipment.services.dto.request.intraBranch.InterBranchDto;
import com.dpw.runner.shipment.services.utils.CommonUtils;
import org.aspectj.lang.annotation.Aspect;
import org.aspectj.lang.annotation.Before;
import org.hibernate.Session;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Component;

import javax.persistence.EntityManager;
import javax.persistence.PersistenceContext;
import java.util.*;

@Aspect
@Component
public class TenantAspect {
    @PersistenceContext
    private EntityManager entityManager;

    @Autowired
    private CommonUtils commonUtils;

    @Before("execution(* com.dpw.runner.shipment.services.aspects.MultitenancyAspect.MultiTenancyRepository+.*(..))")
    public void beforeFindOfMultiTenancyRepository() {

        long tenantId = TenantContext.getCurrentTenant();

        Map<String, Boolean> permissions = Optional.ofNullable(UserContext.getUser()).map(UsersDto::getPermissions).orElse(new HashMap<>());
        
        
        InterBranchDto interBranchDto = commonUtils.getInterBranchContext();
        if(!Objects.isNull(interBranchDto)) {
            var tenantIds = new ArrayList<>(Arrays.asList((long) TenantContext.getCurrentTenant()));
            if (Boolean.TRUE.equals(interBranchDto.isCoLoadStation()) && !Objects.isNull(interBranchDto.getHubTenantIds()))
                tenantIds.addAll(interBranchDto.getHubTenantIds());
            if (Boolean.TRUE.equals(interBranchDto.isHub()) && !Objects.isNull(interBranchDto.getColoadStationsTenantIds()))
                tenantIds.addAll(interBranchDto.getColoadStationsTenantIds());

            entityManager.unwrap(Session.class)
                    .enableFilter(MultiTenancy.MULTI_BRANCH_FILTER_NAME)
                    .setParameter(MultiTenancy.TENANT_PARAMETER_NAME, tenantIds);
        }

        else if (!permissions.containsKey(PermissionConstants.tenantSuperAdmin) && !permissions.containsKey(PermissionConstants.crossTenantListPermission) && !permissions.containsKey(PermissionConstants.crossTenantRetrievePermission) && !permissions.containsKey(PermissionConstants.companySuperAdmin)) {
            entityManager.unwrap(Session.class)
                    .enableFilter(MultiTenancy.TENANT_FILTER_NAME)
                    .setParameter(MultiTenancy.TENANT_PARAMETER_NAME, tenantId);
        }
    }
}
