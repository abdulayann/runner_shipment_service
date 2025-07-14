package com.dpw.runner.shipment.services.entity.commons;

import com.dpw.runner.shipment.services.aspects.MultitenancyAspect.UserContext;
import com.dpw.runner.shipment.services.entity.Parties;
import com.fasterxml.jackson.annotation.JsonFormat;
import com.google.common.base.Strings;
import lombok.*;
import lombok.Generated;
import lombok.experimental.Accessors;
import org.hibernate.annotations.*;
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
@Generated
public class BaseEntity implements Serializable {

    private static final long serialVersionUID = 1L;

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
    @ JsonFormat(shape = JsonFormat.Shape.STRING, pattern = "yyyy-MM-dd'T'HH:mm:ss")
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
        if (UserContext.getUser() != null) {
            String username = UserContext.getUser().getUsername();
            this.updatedBy = username;
        }
    }

    @PrePersist
    void prePersist() {
        if (UserContext.getUser() != null) {
            String username = UserContext.getUser().getUsername();
            if(this.createdBy == null || this.createdBy.isEmpty())
            {
                this.createdBy = username;
            }
            this.updatedBy = username;
        }

        if (this.guid == null) {
            this.guid = UUID.randomUUID();
        }

        if(this instanceof Parties parties) {
            if(Strings.isNullOrEmpty(parties.getOrgId()) && parties.getOrgData() != null && !parties.getOrgData().isEmpty() &&
                    parties.getOrgData().containsKey("Id")) {
                parties.setOrgId(String.valueOf(parties.getOrgData().get("Id")));
            }
            if(Strings.isNullOrEmpty(parties.getAddressId()) && parties.getAddressData() != null && !parties.getAddressData().isEmpty() &&
                    parties.getAddressData().containsKey("Id")) {
                parties.setAddressId(String.valueOf(parties.getAddressData().get("Id")));
            }
        }
    }
}
