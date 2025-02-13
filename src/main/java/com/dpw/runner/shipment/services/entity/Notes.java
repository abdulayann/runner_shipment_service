package com.dpw.runner.shipment.services.entity;

import com.dpw.runner.shipment.services.aspects.MultitenancyAspect.MultiTenancy;
import com.dpw.runner.shipment.services.utils.ExcludeAuditLog;
import lombok.*;
import lombok.experimental.Accessors;
import org.hibernate.annotations.SQLDelete;
import org.hibernate.annotations.Where;

import javax.persistence.Column;
import javax.persistence.Entity;
import javax.persistence.Table;
import java.time.LocalDateTime;

@Entity
@Setter
@Getter
@Table(name = "notes")
@Accessors(chain = true)
@ToString(onlyExplicitlyIncluded = true)
@NoArgsConstructor
@AllArgsConstructor
@Builder
@SQLDelete(sql = "UPDATE notes SET is_deleted = true WHERE id=?")
@Where(clause = "is_deleted = false")
public class Notes extends MultiTenancy {

    @Column(name = "text")
    private String text;

    @Column(name = "label")
    private String label;

    @Column(name = "assigned_to")
    private String assignedTo;

    @ExcludeAuditLog
    @Column(name = "entity_id")
    private Long entityId;

    @ExcludeAuditLog
    @Column(name = "entity_type")
    private String entityType;

    @Column(name = "insert_user_id")
    private String insertUserId;

    @Column(name = "insert_date")
    private LocalDateTime insertDate;

    @Column(name = "insert_user_display_name")
    private String insertUserDisplayName;

    @Column(name = "is_public")
    private Boolean isPublic;

    @Column(name = "is_active")
    private Boolean isActive;
}
