package com.dpw.runner.shipment.services.aspects.MultitenancyAspect;


import org.springframework.beans.factory.annotation.Autowired;

import javax.persistence.EntityNotFoundException;
import javax.persistence.PrePersist;
import javax.persistence.PreRemove;
import javax.persistence.PreUpdate;
import java.util.Objects;

public class TenantEntityListener {

    @Autowired
    private TenantContext tenantContext;

    @PrePersist
    public void prePersist(Object object) {
        if (object instanceof MultiTenancy) {
            //needs to be the current tenantId of the user which can be fetched from the user service
            ((MultiTenancy) object).setTenantId(TenantContext.getCurrentTenant());
        }
    }

    @PreUpdate
    public void preUpdate(Object object) {
        if(object instanceof MultiTenancy)
        {
            Integer tenantId = ((MultiTenancy) object).getTenantId();
            if(TenantContext.getCurrentTenant() != tenantId) {
                throw new RuntimeException("Authorization has been denied for this request");
            }
        }
    }

    @PreRemove
    public void preRemove(Object object) {
        if (object instanceof MultiTenancy && TenantContext.getCurrentTenant() != null &&  !TenantContext.getCurrentTenant().equals(((MultiTenancy) object).getTenantId() )) {
            throw new EntityNotFoundException();
        }
    }
}

