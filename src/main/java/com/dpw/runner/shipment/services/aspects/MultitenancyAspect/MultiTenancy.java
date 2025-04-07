package com.dpw.runner.shipment.services.aspects.MultitenancyAspect;


import com.dpw.runner.shipment.services.entity.commons.BaseEntity;
import com.dpw.runner.shipment.services.utils.TenantIdData;
import lombok.Data;
import lombok.Getter;
import org.hibernate.annotations.Filter;
import org.hibernate.annotations.FilterDef;
import org.hibernate.annotations.Filters;
import org.hibernate.annotations.ParamDef;

import javax.persistence.Column;
import javax.persistence.EntityListeners;
import javax.persistence.MappedSuperclass;

@SuppressWarnings({"java:S1710", "ALL"})
@Data
@MappedSuperclass
@FilterDef(name = MultiTenancy.TENANT_FILTER_NAME,
        parameters = @ParamDef(name = MultiTenancy.TENANT_PARAMETER_NAME, type = "long"),
        defaultCondition = MultiTenancy.TENANT_COLUMN + " = :" + "tenant_id")
@FilterDef(name = MultiTenancy.MULTI_BRANCH_FILTER_NAME,
        parameters = @ParamDef(name = MultiTenancy.TENANT_PARAMETER_NAME, type = "long"),
        defaultCondition = MultiTenancy.TENANT_COLUMN + " IN (:" + MultiTenancy.TENANT_PARAMETER_NAME + ")")
@Filters({@Filter(name = MultiTenancy.TENANT_FILTER_NAME),@Filter(name = MultiTenancy.MULTI_BRANCH_FILTER_NAME)})
@EntityListeners(TenantEntityListener.class)
@Getter
public class MultiTenancy extends BaseEntity {

    public static final String TENANT_FILTER_NAME = "tenantFilter";
    public static final String MULTI_BRANCH_FILTER_NAME = "multiBranchFilter";
    public static final String TENANT_PARAMETER_NAME = "tenant_id";
    public static final String TENANT_COLUMN = "tenant_id";

    @TenantIdData
    @Column(name = "tenant_id", nullable = false, updatable = false)
    private Integer tenantId;
}