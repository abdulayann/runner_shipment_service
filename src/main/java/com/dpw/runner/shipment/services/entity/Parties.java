package com.dpw.runner.shipment.services.entity;


import com.dpw.runner.shipment.services.aspects.MultitenancyAspect.MultiTenancy;
import com.vladmihalcea.hibernate.type.json.JsonBinaryType;
import lombok.*;
import lombok.experimental.Accessors;
import org.hibernate.annotations.Type;
import org.hibernate.annotations.TypeDef;

import javax.persistence.*;
import java.util.List;
import java.util.Map;
import java.util.UUID;


@Entity
@Setter
@Getter
@Table(name = "parties")
@Accessors(chain = true)
@ToString(onlyExplicitlyIncluded = true)
@NoArgsConstructor
@AllArgsConstructor
@Builder
@TypeDef(name = "jsonb", typeClass = JsonBinaryType.class)
public class Parties extends MultiTenancy {

    private static final long serialVersionUID = 190794279984274725L;

    @Column(name = "entity_id")
    private Long entityId;

    @Column(name = "entity_type")
    private String entityType;

    @Column(name = "party_type")
    private String type;

    @Column(name = "org_code")
    private Integer orgCode;

    @Column(name = "address_code")
    private Integer addressCode;

    @Type(type = "jsonb")
    @Column(name = "org_data", columnDefinition = "jsonb")
    private Map<String, Object> orgData;

    @Type(type = "jsonb")
    @Column(name = "address_data", columnDefinition = "jsonb")
    private Map<String, Object> addressData;
}
