package com.dpw.runner.shipment.services.filter.Multitenancy;


import org.springframework.beans.factory.annotation.Autowired;

import javax.persistence.EntityNotFoundException;
import javax.persistence.PrePersist;
import javax.persistence.PreRemove;
import javax.persistence.PreUpdate;

public class TenantEntityListener {

    @Autowired
    private TenantContext tenantContext;

    @PrePersist
    @PreUpdate
    public void prePersistOrUpdate(Object object) {
        if (object instanceof MultiTenancy) {
            //needs to be the current tenantId of the user which can be fetched from the user service
            ((MultiTenancy) object).setTenantId(1);
        }
    }

    @PreRemove
    public void preRemove(Object object) {
        if (object instanceof MultiTenancy && TenantContext.getCurrentTenant() != null &&  !TenantContext.getCurrentTenant().equals(((MultiTenancy) object).getTenantId() )) {
            throw new EntityNotFoundException();
        }
    }
}

