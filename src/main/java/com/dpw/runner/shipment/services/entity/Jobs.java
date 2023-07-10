package com.dpw.runner.shipment.services.entity;

import com.dpw.runner.shipment.services.aspects.MultitenancyAspect.MultiTenancy;
import lombok.*;
import lombok.experimental.Accessors;
import org.hibernate.annotations.Where;

import javax.persistence.*;
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
public class Jobs extends MultiTenancy {

    @Column(name = "shipment_id")
    private Long shipmentId;

    @OneToOne(targetEntity = Parties.class, cascade = CascadeType.ALL)
    @JoinColumn(name = "buyer_detail_id", referencedColumnName = "id")
    private Parties buyerDetail;

    @OneToOne(targetEntity = Parties.class, cascade = CascadeType.ALL)
    @JoinColumn(name = "supplier_detail_id", referencedColumnName = "id")
    private Parties supplierDetail;

    @Column(name = "order_number")
    private String orderNumber;

    @Column(name = "order_date")
    private LocalDateTime orderDate;

    @Column(name = "confirm_number")
    private String confirmNumber;

    @Column(name = "confirm_date")
    private LocalDateTime confirmDate;

    @Column(name = "invoice_number")
    private String invoiceNumber;

    @Column(name = "invoice_date")
    private LocalDateTime invoiceDate;

    @Column(name = "buyer_id")
    private Long buyerId;

    @Column(name = "order_status")
    private String orderStatus;

    @Column(name = "follow_up_date")
    private LocalDateTime followUpDate;

    @Column(name = "description")
    private String description;

    @Column(name = "currency")
    private String currency;

    @Column(name = "service_mode")
    private String serviceMode;

    @Column(name = "inco_term")
    private String incoTerm;

    @Column(name = "additional_terms")
    private String additionalTerms;

    @Column(name = "transport_mode")
    private String transportMode;

    @Column(name = "country_of_origin")
    private String countryOfOrigin;

}
