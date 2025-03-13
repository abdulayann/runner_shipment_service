package com.dpw.runner.shipment.services.entity;

import com.dpw.runner.shipment.services.aspects.MultitenancyAspect.MultiTenancy;
import com.dpw.runner.shipment.services.entity.enums.LifecycleHooks;
import com.vladmihalcea.hibernate.type.json.JsonBinaryType;
import lombok.*;
import lombok.experimental.Accessors;
import org.hibernate.annotations.Type;
import org.hibernate.annotations.TypeDef;

import javax.persistence.*;
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
@TypeDef(name = "jsonb", typeClass = JsonBinaryType.class)
@SuppressWarnings("java:S1948")
public class Validations extends MultiTenancy {

    @Column(name = "entity")
    private String entity;

    @Column(name = "lifecycle_hook")
    @Enumerated(EnumType.STRING)
    private LifecycleHooks lifecycleHook;

    @Type(type = "jsonb")
    @Column(name = "schema_validator", columnDefinition = "jsonb")
    private Map<String, Object> jsonSchema;

}
