package com.dpw.runner.shipment.services.entity;

import com.dpw.runner.shipment.services.aspects.MultitenancyAspect.MultiTenancy;
import lombok.*;
import lombok.experimental.Accessors;
import org.hibernate.annotations.BatchSize;
import org.hibernate.annotations.SQLDelete;
import org.hibernate.annotations.Where;
import javax.persistence.*;
import javax.validation.constraints.Size;
import java.time.LocalDateTime;
import java.util.List;

@Entity
@Setter
@Getter
@Table(name = "jobs")
@Accessors(chain = true)
@ToString(onlyExplicitlyIncluded = true)
@NoArgsConstructor
@AllArgsConstructor
@SQLDelete(sql = "UPDATE jobs SET is_deleted = true WHERE id=?")
@Where(clause = "is_deleted = false")
public class Jobs extends MultiTenancy {

    @Column(name = "shipment_id")
    private Long shipmentId;

    @Column(name = "consolidation_id")
    private Long consolidationId;

    @OneToOne(targetEntity = Parties.class, cascade = CascadeType.ALL, fetch = FetchType.LAZY)
    @JoinColumn(name = "buyer_detail_id", referencedColumnName = "id")
    private Parties buyerDetail;

    @OneToOne(targetEntity = Parties.class, cascade = CascadeType.ALL, fetch = FetchType.LAZY)
    @JoinColumn(name = "supplier_detail_id", referencedColumnName = "id")
    private Parties supplierDetail;

    @Column(name = "order_number")
    @Size(max=50, message = "max size is 50 for order_number")
    private String orderNumber;

    @Column(name = "order_date")
    private LocalDateTime orderDate;

    @Column(name = "confirm_number")
    @Size(max=50, message = "max size is 50 for confirm_number")
    private String confirmNumber;

    @Column(name = "confirm_date")
    private LocalDateTime confirmDate;

    @Column(name = "invoice_number")
    @Size(max=50, message = "max size is 50 for invoice_number")
    private String invoiceNumber;

    @Column(name = "invoice_date")
    private LocalDateTime invoiceDate;

    @Column(name = "buyer_id")
    private Long buyerId;

    @Column(name = "order_status")
    @Size(max=3, message = "max size is 3 for order_status")
    private String orderStatus;

    @Column(name = "follow_up_date")
    private LocalDateTime followUpDate;

    @Column(name = "description")
    @Size(max=1000, message = "max size is 1000 for description")
    private String description;

    @Column(name = "currency")
    @Size(max=3, message = "max size is 3 for currency")
    private String currency;

    @Column(name = "service_mode")
    @Size(max=50, message = "max size is 50 for service_mode")
    private String serviceMode;

    @Column(name = "inco_term")
    @Size(max=50, message = "max size is 50 for inco_term")
    private String incoTerm;

    @Column(name = "additional_terms")
    @Size(max=1000, message = "max size is 1000 for additional_terms")
    private String additionalTerms;

    @Column(name = "transport_mode")
    @Size(max=3, message = "max size is 3 for transport_mode")
    private String transportMode;

    @Column(name = "country_of_origin")
    @Size(max=3, message = "max size is 3 for country_of_origin")
    private String countryOfOrigin;

    @OneToMany(fetch = FetchType.LAZY, mappedBy = "entityId")
    @Where(clause = "entity_type = 'JOBS'")
    @BatchSize(size = 50)
    private List<Events> eventsList;

}
