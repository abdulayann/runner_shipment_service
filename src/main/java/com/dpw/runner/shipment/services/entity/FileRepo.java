package com.dpw.runner.shipment.services.entity;

import com.dpw.runner.shipment.services.aspects.MultitenancyAspect.MultiTenancy;
import com.dpw.runner.shipment.services.entity.commons.BaseEntity;
import lombok.*;
import lombok.experimental.Accessors;
import org.hibernate.annotations.CollectionId;
import org.hibernate.annotations.Where;

import javax.persistence.*;
import java.util.List;

@Entity
@Setter
@Getter
@Table(name = "file_repo")
@Accessors(chain = true)
@ToString(onlyExplicitlyIncluded = true)
@NoArgsConstructor
@AllArgsConstructor
public class FileRepo extends MultiTenancy {

    @Id
    @GeneratedValue(strategy = GenerationType.IDENTITY)
    @Column(name = "id")
    @ToString.Include
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

    @OneToMany(cascade = CascadeType.ALL, fetch = FetchType.EAGER, mappedBy = "entityId")
    @Where(clause = "entity_type = 'fileRepo'")
    @JoinColumn(name = "shipment_id", referencedColumnName = "id")
    private List<ShipmentDetails> shipmentDetailsList;


    @Column(name = "consolidation_id")
    private Long consolidationId;
}
