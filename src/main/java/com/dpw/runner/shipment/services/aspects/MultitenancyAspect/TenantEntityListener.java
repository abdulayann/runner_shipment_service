package com.dpw.runner.shipment.services.aspects.MultitenancyAspect;


import com.dpw.runner.shipment.services.commons.constants.Permissions;
import org.springframework.beans.factory.annotation.Autowired;

import javax.persistence.EntityNotFoundException;
import javax.persistence.PrePersist;
import javax.persistence.PreRemove;
import javax.persistence.PreUpdate;
import java.util.Map;
import java.util.Objects;

public class TenantEntityListener {

    @Autowired
    private TenantContext tenantContext;

    @PrePersist
    public void prePersist(Object object) {
        if (object instanceof MultiTenancy) {
            Integer tenantId = ((MultiTenancy) object).getTenantId();
            Map<String, Boolean> permissions = UserContext.getUser().Permissions;

            if (permissions.containsKey(Permissions.tenantSuperAdmin) && !Objects.isNull(tenantId))
                ((MultiTenancy) object).setTenantId(tenantId);
            else
                ((MultiTenancy) object).setTenantId(TenantContext.getCurrentTenant());

            // Special case handled to retrigger sync from V1 to V2 on demand basis from admin account
            if (permissions.containsKey(Permissions.tenantSuperAdmin) && !Objects.isNull(UserContext.getUser().getSyncTenantId()))
                ((MultiTenancy) object).setTenantId(UserContext.getUser().getSyncTenantId());

        }
    }

    @PreUpdate
    public void preUpdate(Object object) {
        if(object instanceof MultiTenancy) {
            Integer tenantId = ((MultiTenancy) object).getTenantId();
            Map<String, Boolean> permissions = UserContext.getUser().Permissions;

            if (permissions.containsKey(Permissions.tenantSuperAdmin) && !Objects.isNull(tenantId))
                ((MultiTenancy) object).setTenantId(tenantId);
            else
                ((MultiTenancy) object).setTenantId(TenantContext.getCurrentTenant());

            if(tenantId == null)
                tenantId = TenantContext.getCurrentTenant();

            if(! permissions.containsKey(Permissions.tenantSuperAdmin) && !Objects.equals(TenantContext.getCurrentTenant(), tenantId))
                throw new RuntimeException("Authorization has been denied for this request, tenantId mismatch");


        }
    }

    @PreRemove
    public void preRemove(Object object) {
        if(object instanceof MultiTenancy) {
            Integer tenantId = ((MultiTenancy) object).getTenantId();
            Map<String, Boolean> permissions = UserContext.getUser().Permissions;

            if (permissions.containsKey(Permissions.tenantSuperAdmin) && !Objects.isNull(tenantId))
                ((MultiTenancy) object).setTenantId(tenantId);
            else
                ((MultiTenancy) object).setTenantId(TenantContext.getCurrentTenant());

            if(! permissions.containsKey(Permissions.tenantSuperAdmin) && !Objects.equals(TenantContext.getCurrentTenant(), tenantId))
                throw new EntityNotFoundException();
        }
    }
}

