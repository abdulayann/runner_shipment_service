package com.dpw.runner.shipment.services.entity;

import com.dpw.runner.shipment.services.entity.enums.DpsEntityType;
import com.dpw.runner.shipment.services.entity.enums.DpsExecutionStatus;
import com.dpw.runner.shipment.services.entity.enums.DpsWorkflowState;
import com.dpw.runner.shipment.services.entity.enums.DpsWorkflowType;
import com.vladmihalcea.hibernate.type.json.JsonBinaryType;
import java.time.LocalDateTime;
import java.util.List;
import java.util.UUID;
import javax.persistence.CollectionTable;
import javax.persistence.Column;
import javax.persistence.ElementCollection;
import javax.persistence.Embeddable;
import javax.persistence.Entity;
import javax.persistence.EnumType;
import javax.persistence.Enumerated;
import javax.persistence.FetchType;
import javax.persistence.GeneratedValue;
import javax.persistence.GenerationType;
import javax.persistence.Id;
import javax.persistence.JoinColumn;
import javax.persistence.PrePersist;
import javax.persistence.PreUpdate;
import javax.persistence.Table;
import lombok.AllArgsConstructor;
import lombok.Data;
import lombok.Getter;
import lombok.NoArgsConstructor;
import lombok.Setter;
import lombok.ToString;
import lombok.experimental.Accessors;
import org.hibernate.annotations.BatchSize;
import org.hibernate.annotations.CreationTimestamp;
import org.hibernate.annotations.SQLDelete;
import org.hibernate.annotations.Type;
import org.hibernate.annotations.TypeDef;
import org.hibernate.annotations.UpdateTimestamp;
import org.hibernate.annotations.Where;
import org.springframework.data.annotation.CreatedDate;
import org.springframework.data.annotation.LastModifiedDate;

@Entity
@Setter
@Getter
@Table(name = "dps_event")
@Accessors(chain = true)
@ToString(onlyExplicitlyIncluded = true)
@TypeDef(name = "jsonb", typeClass = JsonBinaryType.class )
@NoArgsConstructor
@AllArgsConstructor
@SQLDelete(sql = "UPDATE dps_event SET is_deleted = true WHERE id=?")
@Where(clause = "is_deleted = false")
public class DpsEvent {

    @Id
    @ToString.Include
    @GeneratedValue(strategy = GenerationType.IDENTITY)
    private Long id;

    @Column(name = "guid", nullable = false, updatable = false)
    private UUID guid;

    @Column(name = "execution_id", nullable = false)
    private UUID executionId;

    @Column(name = "entity_id", nullable = false)
    private String entityId;

    @Column(name = "entity_type", nullable = false)
    @Enumerated(EnumType.STRING)
    private DpsEntityType entityType;

    @Column(name = "workflow_type")
    @Enumerated(EnumType.STRING)
    private DpsWorkflowType workflowType;

    @Column(name = "state")
    @Enumerated(EnumType.STRING)
    private DpsWorkflowState state;

    @Column(name = "status")
    @Enumerated(EnumType.STRING)
    private DpsExecutionStatus status;

    @Column(name = "text")
    private String text;

    @Column(name = "implication_list")
    @ElementCollection(targetClass = String.class, fetch = FetchType.EAGER)
    @CollectionTable(name = "dps_event_implication", joinColumns = @JoinColumn(name = "dps_event_id"))
    @BatchSize(size = 50)
    private List<String> implicationList;

    @Column(name = "condition_message_list")
    @ElementCollection(targetClass = String.class, fetch = FetchType.EAGER)
    @CollectionTable(name = "dps_event_condition_message", joinColumns = @JoinColumn(name = "dps_event_id"))
    @BatchSize(size = 50)
    private List<String> conditionMessageList;

    @Type(type = "jsonb")
    @Column(name = "dpsFieldData", columnDefinition = "jsonb")
    private List<DpsFieldData> dpsFieldData;

    @CreationTimestamp
    @Column(name = "created_at", nullable = false, updatable = false)
    @CreatedDate
    private LocalDateTime createdAt;

    @UpdateTimestamp
    @Column(name = "updated_at", nullable = false)
    @LastModifiedDate
    private LocalDateTime updatedAt;

    @Column(name = "is_deleted")
    private Boolean isDeleted = Boolean.FALSE;

    @Column(name = "event_timestamp")
    private LocalDateTime eventTimestamp;

    @Column(name = "username")
    private String username;

    @PreUpdate
    void preUpdate() {
        this.updatedAt = LocalDateTime.now();
        if (Boolean.TRUE.equals(this.isDeleted)) {
            this.implicationList.clear();
            this.conditionMessageList.clear();
        }
    }

    @PrePersist
    void prePersist() {
        this.createdAt = LocalDateTime.now();
        this.updatedAt = LocalDateTime.now();
        if (this.guid == null) {
            this.guid = UUID.randomUUID();
        }
    }

    @Data
    @NoArgsConstructor
    @AllArgsConstructor
    @Embeddable
    public static class DpsFieldData {
        private String key;
        private String value;
    }

}