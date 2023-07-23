package com.dpw.runner.shipment.services.entity.commons;

import lombok.*;
import lombok.experimental.Accessors;
import org.hibernate.annotations.*;
import org.hibernate.annotations.Generated;
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

    @GeneratedValue(generator = "UUID")
    @Generated(GenerationTime.ALWAYS)
    @GenericGenerator(name = "UUID", strategy = "org.hibernate.id.UUIDGenerator")
    @Column(name = "guid", columnDefinition = "uuid")
    @ColumnDefault("random_uuid()")
    @Type(type = "uuid-char")
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

    @Column(name = "created_by")
    private Integer createdBy;

    @Column(name = "updated_by")
    private Integer updatedBy;

    @Column(name = "is_deleted")
    private Boolean isDeleted = Boolean.FALSE;
}
