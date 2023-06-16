package com.dpw.runner.shipment.services.entity;

import lombok.*;
import lombok.experimental.Accessors;
import org.hibernate.annotations.CollectionId;

import javax.persistence.*;

@Entity
@Setter
@Getter
@Table(name = "file_repo")
@Accessors(chain = true)
@ToString(onlyExplicitlyIncluded = true)
@NoArgsConstructor
@AllArgsConstructor
public class FileRepo {

    @Id
    @Column(name = "id")
    private Long id;

    @Column(name = "file_name")
    private String fileName;

    @Column(name = "path")
    private String path;

    @Column(name = "doc_type")
    private String docType;

    @Column(name = "client_enabled")
    private Boolean clientEnabled;

    @Column(name = "is_posted")
    private Boolean isPosted;

    @Column(name = "event_code")
    private String eventCode;

    @Column(name = "shipment_id")
    private Long shipmentId;

    @Column(name = "consolidation_id")
    private Long consolidationId;
}
