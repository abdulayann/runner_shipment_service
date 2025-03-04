package com.dpw.runner.shipment.services.entity;

import com.dpw.runner.shipment.services.aspects.MultitenancyAspect.MultiTenancy;
import com.dpw.runner.shipment.services.commons.requests.AuditLogChanges;
import com.vladmihalcea.hibernate.type.json.JsonBinaryType;
import lombok.*;
import lombok.experimental.Accessors;
import org.hibernate.annotations.SQLDelete;
import org.hibernate.annotations.Type;
import org.hibernate.annotations.TypeDef;
import org.hibernate.annotations.Where;

import javax.persistence.Column;
import javax.persistence.Entity;
import javax.persistence.Table;
import java.util.Map;

@Entity
@Setter
@Getter
@Table(name = "audit_log")
@Accessors(chain = true)
@ToString(onlyExplicitlyIncluded = true)
@NoArgsConstructor
@AllArgsConstructor
@Builder
@SQLDelete(sql = "UPDATE audit_log SET is_deleted = true WHERE id=?")
@Where(clause = "is_deleted = false")
@TypeDef(name = "jsonb", typeClass = JsonBinaryType.class)
public class AuditLog extends MultiTenancy {
    @Column(name = "operation")
    private String operation;

    @Column(name = "entity")
    private String entity;

    @Column(name = "entity_id")
    private Long entityId;

    @Column(name = "changes", columnDefinition = "jsonb")
    @Type(type = "jsonb")
    private Map<String, AuditLogChanges> changes;

    @Column(name = "parent_type")
    private String parentType;

    @Column(name = "parent_id")
    private Long parentId;

    @Column(name = "flow")
    private String flow;

    @Column(name = "is_integration_log")
    private Boolean isIntegrationLog;

    @Column(name = "data_type")
    private String dataType;

}
