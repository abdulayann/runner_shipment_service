package com.dpw.runner.shipment.services.entity;

import com.dpw.runner.shipment.services.aspects.MultitenancyAspect.MultiTenancy;
import com.vladmihalcea.hibernate.type.json.JsonBinaryType;
import lombok.*;
import lombok.experimental.Accessors;
import org.hibernate.annotations.Type;
import org.hibernate.annotations.TypeDef;

import javax.persistence.Column;
import javax.persistence.Entity;
import javax.persistence.Table;


@Entity
@Setter
@Getter
@Table(name = "sync_queue")
@NoArgsConstructor
@AllArgsConstructor
@Builder
@Accessors(chain = true)
@TypeDef(name = "jsonb", typeClass = JsonBinaryType.class)
public class SyncQueue extends MultiTenancy {

    @Column(name = "module_type")
    private String moduleType;

    @Column(name = "module_id")
    private String moduleId;

    @Column(name = "sync_tenant_id")
    private Integer syncTenantId;

    @Type(type = "jsonb")
    @Column(name = "sync_data")
    private String data;

}