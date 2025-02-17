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

@Entity
@Setter
@Getter
@Table(name = "file_repo")
@Accessors(chain = true)
@ToString(onlyExplicitlyIncluded = true)
@NoArgsConstructor
@AllArgsConstructor
@SQLDelete(sql = "UPDATE file_repo SET is_deleted = true WHERE id=?")
@Where(clause = "is_deleted = false")
public class FileRepo extends MultiTenancy {

    @Column(name = "entity_id")
    private Long entityId;

    @Column(name = "entity_type")
    @Size(max = 100, message = "max size is 100 for entity_type")
    private String entityType;

    @Column(name = "file_name")
    @Size(max = 512, message = "max size is 512 for file_name")
    private String fileName;

    @Column(name = "path")
    @Size(max = 1024, message = "max size is 1024 for path")
    private String path;

    @Column(name = "doc_type")
    private String docType;

    @Column(name = "client_enabled")
    private Boolean clientEnabled;

    @Column(name = "is_posted")
    private Boolean isPosted;

    @Column(name = "event_code")
    private String eventCode;
}
