package com.dpw.runner.shipment.services.entity;

import com.dpw.runner.shipment.services.aspects.MultitenancyAspect.MultiTenancy;
import com.dpw.runner.shipment.services.entity.enums.LifecycleHooks;
import com.vladmihalcea.hibernate.type.json.JsonBinaryType;
import lombok.*;
import lombok.experimental.Accessors;
import org.hibernate.annotations.JdbcTypeCode;
import org.hibernate.annotations.Type;

import jakarta.persistence.*;
import org.hibernate.type.SqlTypes;

import java.util.Map;

@Entity
@Table(name = "validations")
@Getter
@Setter
@ToString(onlyExplicitlyIncluded = true)
@Builder
@NoArgsConstructor
@AllArgsConstructor
@Accessors(chain = true)
@SuppressWarnings("java:S1948")
public class Validations extends MultiTenancy {

    @Column(name = "entity")
    private String entity;

    @Column(name = "lifecycle_hook")
    @Enumerated(EnumType.STRING)
    private LifecycleHooks lifecycleHook;

    @JdbcTypeCode(SqlTypes.JSON)
    @Column(name = "schema_validator", columnDefinition = "jsonb")
    private Map<String, Object> jsonSchema;

}
