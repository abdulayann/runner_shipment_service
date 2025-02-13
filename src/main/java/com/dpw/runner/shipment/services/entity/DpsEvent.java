package com.dpw.runner.shipment.services.entity;

import com.dpw.runner.shipment.services.entity.enums.DpsEntityType;
import com.dpw.runner.shipment.services.entity.enums.DpsExecutionStatus;
import com.dpw.runner.shipment.services.entity.enums.DpsWorkflowState;
import com.dpw.runner.shipment.services.entity.enums.DpsWorkflowType;
import com.vladmihalcea.hibernate.type.json.JsonBinaryType;
import lombok.*;
import lombok.experimental.Accessors;
import org.hibernate.annotations.*;
import org.springframework.data.annotation.CreatedDate;
import org.springframework.data.annotation.LastModifiedDate;

import javax.persistence.Entity;
import javax.persistence.Table;
import javax.persistence.*;
import java.time.LocalDateTime;
import java.util.List;
import java.util.UUID;

@Entity
@Setter
@Getter
@Table(name = "dps_event")
@Accessors(chain = true)
@ToString(onlyExplicitlyIncluded = true)
@TypeDef(name = "jsonb", typeClass = JsonBinaryType.class)
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

    @Column(name = "matching_condition")
    private String matchingCondition;

    @Column(name = "implication")
    @ElementCollection(targetClass = String.class, fetch = FetchType.LAZY)
    @CollectionTable(name = "dps_event_implication", joinColumns = @JoinColumn(name = "dps_event_id"))
    @BatchSize(size = 50)
    private List<String> implicationList;

    @Column(name = "condition_message")
    @ElementCollection(targetClass = String.class, fetch = FetchType.LAZY)
    @CollectionTable(name = "dps_event_condition_message", joinColumns = @JoinColumn(name = "dps_event_id"))
    @BatchSize(size = 50)
    private List<String> conditionMessageList;

    @Column(name = "rule_matched_field")
    @ElementCollection(targetClass = String.class, fetch = FetchType.LAZY)
    @CollectionTable(name = "dps_event_rule_matched_field", joinColumns = @JoinColumn(name = "dps_event_id"))
    @BatchSize(size = 50)
    private List<String> ruleMatchedFieldList;

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

    @Column(name = "transaction_id", nullable = false)
    private String transactionId;

    @Type(type = "jsonb")
    @Column(name = "username_list", columnDefinition = "jsonb")
    private List<String> usernameList;

    @Type(type = "jsonb")
    @Column(name = "tasks", columnDefinition = "jsonb")
    private List<Object> tasks;

    @ElementCollection(fetch = FetchType.LAZY)
    @CollectionTable(name = "dps_approval_detail", joinColumns = @JoinColumn(name = "dps_event_id"))
    @BatchSize(size = 50)
    private List<DpsApprovalDetail> dpsApprovalDetailList;

    @PreUpdate
    void preUpdate() {
        this.updatedAt = LocalDateTime.now();
        if (Boolean.TRUE.equals(this.isDeleted)) {
            this.implicationList.clear();
            this.conditionMessageList.clear();
            this.ruleMatchedFieldList.clear();
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
    @Builder
    @Embeddable
    public static class DpsFieldData {
        private String key;
        private String value;
    }

    @Data
    @NoArgsConstructor
    @AllArgsConstructor
    @Builder
    @Embeddable
    public static class DpsApprovalDetail {
        @Column(name = "username")
        private String username;

        @Column(name = "action_time")
        private LocalDateTime actionTime;

        @Column(name = "message")
        private String message;

        @Column(name = "state")
        private String state;

        @Column(name = "approval_level")
        private String approvalLevel;

        @Column(name = "role_name")
        private String roleName;

        @Column(name = "role_id")
        private String roleId;
    }

}