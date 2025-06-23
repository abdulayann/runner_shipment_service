package com.dpw.runner.shipment.services.entity;

import com.dpw.runner.shipment.services.aspects.MultitenancyAspect.MultiTenancy;
import com.dpw.runner.shipment.services.entity.enums.IntegrationType;
import com.dpw.runner.shipment.services.entity.enums.Status;
import com.vladmihalcea.hibernate.type.json.JsonBinaryType;
import lombok.*;
import lombok.experimental.Accessors;
import org.hibernate.annotations.SQLDelete;
import org.hibernate.annotations.Type;
import org.hibernate.annotations.TypeDef;
import org.hibernate.annotations.Where;

import javax.persistence.*;


@Entity
@Setter
@Getter
@Builder
@Table(name = "integration_responses")
@Accessors(chain = true)
@ToString(onlyExplicitlyIncluded = true)
@NoArgsConstructor
@AllArgsConstructor
@TypeDef(name = "jsonb", typeClass = JsonBinaryType.class )
@SQLDelete(sql = "UPDATE integration_responses SET is_deleted = true WHERE id=?")
@Where(clause = "is_deleted = false")
public class IntegrationResponse extends MultiTenancy {

    @Column(name = "entity_id")
    private Long entityId;

    @Column(name = "entity_type")
    private String entityType;

    @Enumerated(EnumType.STRING)
    @Column(name = "integration_type")
    private IntegrationType integrationType;

    @Column(name = "status")
    @Enumerated(EnumType.STRING)
    private Status status;

    @Type(type = "jsonb")
    @Column(name = "response_message", columnDefinition = "jsonb")
    private String response_message;

}
