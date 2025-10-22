package com.dpw.runner.shipment.services.entity;

import com.dpw.runner.shipment.services.aspects.MultitenancyAspect.MultiTenancy;
import lombok.*;
import lombok.experimental.Accessors;
import org.hibernate.annotations.JdbcTypeCode;

import jakarta.persistence.Column;
import jakarta.persistence.Entity;
import jakarta.persistence.Table;
import org.hibernate.type.SqlTypes;


@Entity
@Setter
@Getter
@Table(name = "sync_queue")
@NoArgsConstructor
@AllArgsConstructor
@Builder
@Accessors(chain = true)
public class SyncQueue extends MultiTenancy {

    @Column(name = "module_type")
    private String moduleType;

    @Column(name = "module_id")
    private String moduleId;

    @Column(name = "sync_tenant_id")
    private Integer syncTenantId;

    @JdbcTypeCode(SqlTypes.JSON)
    @Column(name = "sync_data")
    private String data;

}