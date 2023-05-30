package com.dpw.runner.shipment.services.entity;


import com.dpw.runner.shipment.services.filter.Multitenancy.MultiTenancy;
import com.vladmihalcea.hibernate.type.json.JsonBinaryType;
import lombok.*;
import lombok.experimental.Accessors;
import org.hibernate.annotations.TypeDef;
import org.hibernate.annotations.Where;

import javax.persistence.*;
import javax.persistence.Id;
import javax.persistence.Table;
import java.util.List;
import java.util.UUID;


@Entity
@Setter
@Getter
@Table(name = "shipment_details")
@Accessors(chain = true)
@ToString(onlyExplicitlyIncluded = true)
@NoArgsConstructor
@AllArgsConstructor
@Builder
@TypeDef(name = "jsonb", typeClass = JsonBinaryType.class )
public class ShipmentDetails extends MultiTenancy {

    private static final long serialVersionUID = 190794279984274725L;

    @Id
    @ToString.Include
    @GeneratedValue(strategy = GenerationType.IDENTITY)
    private Long id;

    @Column(name = "guid")
    private UUID guid;

    @OneToMany(cascade=CascadeType.ALL, fetch = FetchType.EAGER,mappedBy="entityId")
    @Where(clause = "entity_type = '[Shipments]'")
    private List<PartiesDetails> parties;

    @OneToOne(targetEntity = BlDetails.class)
    @JoinColumn(name = "bl_detail_id", referencedColumnName = "id")
    private BlDetails blDetails;

    @OneToOne(targetEntity = CarrierDetails.class)
    @JoinColumn(name = "carrier_detail_id", referencedColumnName = "id")
    private CarrierDetails carrierDetails;

    @OneToOne(targetEntity = MeasurementDetails.class)
    @JoinColumn(name = "measurement_detail_id", referencedColumnName = "id")
    private MeasurementDetails measurementDetails;

    @Column(name = "house_bill")
    private String houseBill;

    @Column(name = "transport_mode")
    private String transportMode;

    @Column(name = "direction")
    private String direction;

    @Column(name = "shipment_type")
    private String shipmentType;

    @Column(name = "status")
    private Integer status;

    @Column(name = "source")
    private String source;

    @Column(name = "job_type")
    private String jobType;

    @Column(name = "service_type")
    private String serviceType;

    @Column(name = "master_bill")
    private String masterBill;

    @Column(name = "booking_reference")
    private String bookingReference;

    @Column(name = "console_ref")
    private String consolRef;

    @Column(name = "sales_agent")
    private Long salesAgent;

    @Column(name = "payment_terms")
    private String paymentTerms;

    @Column(name = "incoterms")
    private String incoterms;

    @Column(name = "shipment_id")
    private String shipmentId;

    @Column(name = "is_domestic")
    private Boolean isDomestic;

    @Column(name = "assigned_to")
    private Integer assignedTo;

    @Column(name = "additional_terms")
    private String additionalTerms;

    @Column(name = "goods_description")
    private String goodsDescription;

    @OneToOne(targetEntity = PickupDetails.class)
    @JoinColumn(name = "pickup_detail_id", referencedColumnName = "id")
    private PickupDetails pickupDetails;

    @OneToOne(targetEntity = DeliveryDetails.class)
    @JoinColumn(name = "delivery_detail_id", referencedColumnName = "id")
    private DeliveryDetails deliveryDetails;
}
