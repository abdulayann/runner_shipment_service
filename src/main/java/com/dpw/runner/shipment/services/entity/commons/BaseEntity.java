package com.dpw.runner.shipment.services.entity.commons;

import com.dpw.runner.shipment.services.aspects.MultitenancyAspect.UserContext;
import com.dpw.runner.shipment.services.utils.ContextUtility;
import lombok.*;
import lombok.experimental.Accessors;
import org.hibernate.annotations.*;
import org.hibernate.annotations.Generated;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.data.annotation.CreatedDate;
import org.springframework.data.annotation.LastModifiedDate;

import javax.persistence.*;
import java.io.Serializable;
import java.time.LocalDateTime;
import java.util.UUID;

/**
 *
 */
@RequiredArgsConstructor
@Setter
@Getter
@Accessors(chain = true)
@ToString(onlyExplicitlyIncluded = true)
@MappedSuperclass
@Inheritance(strategy = InheritanceType.SINGLE_TABLE)
public class BaseEntity implements Serializable {

    private static final long serialVersionUID = 1L;

    @Autowired
    private ContextUtility contextUtility;
    @Column(name = "guid", columnDefinition = "uuid", updatable = false)
    @ColumnDefault("random_uuid()")
    private UUID guid;

    @Id
    @GeneratedValue(strategy = GenerationType.IDENTITY)
    @Column(name = "id")
    @ToString.Include
    private Long id;

    @CreationTimestamp
    @Column(name = "created_at", nullable = false, updatable = false)
    @CreatedDate
    private LocalDateTime createdAt;

    @UpdateTimestamp
    @Column(name = "updated_at", nullable = false)
    @LastModifiedDate
    private LocalDateTime updatedAt;

    @Column(name = "created_by", nullable = false, updatable = false)
    private String createdBy;

    @Column(name = "updated_by")
    private String updatedBy;

    @Column(name = "is_deleted")
    private Boolean isDeleted = Boolean.FALSE;

    @PreUpdate
    void preUpdate() {
        if (contextUtility.userContext.getUser() != null) {
            String username = contextUtility.userContext.getUser().getUsername();
            this.updatedBy = username;
        }
    }

    @PrePersist
    void prePersist() {
        if (contextUtility.userContext.getUser() != null) {
            String username = contextUtility.userContext.getUser().getUsername();
            this.createdBy = username;
            this.updatedBy = username;
        }

        if (this.guid == null) {
            this.guid = UUID.randomUUID();
        }
    }
}
