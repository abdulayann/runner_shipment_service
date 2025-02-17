package com.dpw.runner.shipment.services.entity;

import com.dpw.runner.shipment.services.aspects.MultitenancyAspect.MultiTenancy;
import lombok.*;
import lombok.experimental.Accessors;
import org.hibernate.annotations.SQLDelete;
import org.hibernate.annotations.Where;

import javax.persistence.Column;
import javax.persistence.Entity;
import javax.persistence.Table;
import javax.validation.constraints.Size;
import java.util.Objects;
import java.util.UUID;

@Entity
@Setter
@Getter
@Table(name = "logs_history")
@Accessors(chain = true)
@ToString(onlyExplicitlyIncluded = true)
@NoArgsConstructor
@AllArgsConstructor
@Builder
@SQLDelete(sql = "UPDATE logs SET is_deleted = true WHERE id=?")
@Where(clause = "is_deleted = false")
public class LogsHistory extends MultiTenancy {
    @Column(name = "entity_id")
    private Long entityId;

    @Column(name = "entity_type")
    @Size(max = 100, message = "max size is 100 for entity_type")
    private String entityType;

    @Column(name = "entity_guid")
    private UUID entityGuid;

    @Column(name = "entity_payload")
    private String entityPayload;

    @Override
    public boolean equals(Object o) {
        if (this == o) return true;
        if (o == null || getClass() != o.getClass()) return false;
        LogsHistory that = (LogsHistory) o;
        return Objects.equals(this, that);
    }

    @Override
    public int hashCode() {
        return Objects.hash(entityId, entityType);
    }

}
