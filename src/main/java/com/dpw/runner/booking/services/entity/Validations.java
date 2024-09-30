package com.dpw.runner.booking.services.entity;

import com.dpw.runner.booking.services.aspects.MultitenancyAspect.MultiTenancy;
import com.dpw.runner.booking.services.entity.enums.LifecycleHooks;
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
