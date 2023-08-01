package com.dpw.runner.shipment.services.entity;


import com.dpw.runner.shipment.services.aspects.MultitenancyAspect.MultiTenancy;
import com.fasterxml.jackson.annotation.JsonIgnoreProperties;
import com.vladmihalcea.hibernate.type.json.JsonBinaryType;
import lombok.*;
import lombok.experimental.Accessors;
import org.hibernate.annotations.BatchSize;
import org.hibernate.annotations.SQLDelete;
import org.hibernate.annotations.TypeDef;
import org.hibernate.annotations.Where;

import javax.persistence.*;
import javax.persistence.Table;
import java.math.BigDecimal;
import java.time.LocalDateTime;
import java.util.List;


@Entity
@Setter
@Getter
@Table(name = "shipment_details")
@Accessors(chain = true)
@ToString(onlyExplicitlyIncluded = true)
@NoArgsConstructor
@AllArgsConstructor
@Builder
@TypeDef(name = "jsonb", typeClass = JsonBinaryType.class)
@SQLDelete(sql = "UPDATE shipment_details SET is_deleted = true WHERE id=?")
@Where(clause = "is_deleted = false")
public class ShipmentDetails extends MultiTenancy {

    private static final long serialVersionUID = 190794279984274725L;

    @OneToOne(fetch = FetchType.LAZY, targetEntity = CarrierDetails.class)
    @JoinColumn(name = "carrier_detail_id", referencedColumnName = "id")
    private CarrierDetails carrierDetails;

    @Column(name = "house_bill")
    private String houseBill;

    @Column(name = "transport_mode")
    private String transportMode;

    @Column(name = "direction")
    private String direction;

    @Column(name = "shipment_type")
    private String shipmentType;

    @ManyToMany(fetch = FetchType.LAZY)
    @JoinTable(name = "shipments_containers_mapping",
            joinColumns = @JoinColumn(name = "shipment_id"),
            inverseJoinColumns = @JoinColumn(name = "container_id"))
    @JsonIgnoreProperties("shipmentsList")
    @BatchSize(size = 50)
    private List<Containers> containersList;

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
    private String assignedTo;

    @Column(name = "additional_terms")
    private String additionalTerms;

    @Column(name = "goods_description")
    private String goodsDescription;

    @OneToMany(fetch = FetchType.LAZY, mappedBy = "shipmentId")
    private List<BookingCarriage> bookingCarriagesList;

    @OneToMany(fetch = FetchType.LAZY, mappedBy = "shipmentId")
    private List<ELDetails> elDetailsList;

    @OneToMany(fetch = FetchType.LAZY, mappedBy = "entityId")
    @Where(clause = "entity_type = 'SHIPMENT'")
    @BatchSize(size = 50)
    private List<Events> eventsList;

    @OneToMany(fetch = FetchType.LAZY, mappedBy = "entityId")
    @Where(clause = "entity_type = 'SHIPMENT'")
    private List<FileRepo> fileRepoList;

    @OneToMany(fetch = FetchType.LAZY, mappedBy = "shipmentId")
    private List<Packing> packingList;

    @OneToMany(fetch = FetchType.LAZY, mappedBy = "shipmentId")
    private List<ReferenceNumbers> referenceNumbersList;

    @OneToMany(fetch = FetchType.LAZY, mappedBy = "shipmentId")
    private List<Routings> routingsList;

    @OneToMany(fetch = FetchType.LAZY, mappedBy = "shipmentId")
    private List<ServiceDetails> servicesList;

    @OneToMany(fetch = FetchType.LAZY, mappedBy = "shipmentId")
    private List<TruckDriverDetails> truckDriverDetails;

    @Column(name = "weight")
    private BigDecimal weight;

    @Column(name = "weight_unit")
    private String weightUnit;

    @Column(name = "volume")
    private BigDecimal volume;

    @Column(name = "volume_unit")
    private String volumeUnit;

    @Column(name = "volumetric_weight")
    private BigDecimal volumetricWeight;

    @Column(name = "volumetric_weight_unit")
    private String volumetricWeightUnit;

    @Column(name = "chargable")
    private BigDecimal chargable;

    @Column(name = "chargeable_unit")
    private String chargeableUnit;

    @Column(name = "net_weight")
    private BigDecimal netWeight;

    @Column(name = "net_weight_unit")
    private String netWeightUnit;

    @Column(name = "no_of_packs")
    private Integer noOfPacks;

    @Column(name = "packs_unit")
    private String packsUnit;

    @Column(name = "inner_packs")
    private Integer innerPacks;

    @Column(name = "inner_pack_unit")
    private String innerPackUnit;

    @Column(name = "freight_local")
    private Integer freightLocal;

    @Column(name = "freightLocal_Currency")
    private String freightLocalCurrency;

    @Column(name = "freight_overseas")
    private Integer freightOverseas;

    @Column(name = "freightOverseas_Currency")
    private String freightOverseasCurrency;

    @Column(name = "auto_update_wt_vol")
    private boolean autoUpdateWtVol;

    @Column(name = "container_auto_wv_update")
    private boolean containerAutoWeightVolumeUpdate;

    @Column(name = "marks_num")
    private String marksNum;

    @Column(name = "entry_detail")
    private String entryDetail;

    @Column(name = "is_locked")
    private Boolean isLocked;

    @Column(name = "locked_by")
    private String lockedBy;

    @Column(name = "is_notify_consignee_equal")
    private Boolean isNotifyConsigneeEqual;

    //ShipmentOrderId

    @Column(name = "booking_type")
    private String bookingType;

    @Column(name = "cargo_finance_booking")
    private boolean cargoFinanceBooking;

    @Column(name = "booking_number")
    private String bookingNumber;

    @Column(name = "route")
    private String route;

    @Column(name = "source_tenant_id")
    private long sourceTenantId;

    @Column(name = "documentation_partner")
    private long documentationPartner;

    @Column(name = "triangulation_partner")
    private long triangulationPartner;

    @Column(name = "receiving_branch")
    private long receivingBranch;

    @Column(name = "intra_branch")
    private boolean intraBranch;

    @Column(name = "prev_shipment_status")
    private Integer prevShipmentStatus;

    @Column(name = "is_shipment_read_only")
    private boolean isShipmentReadOnly;

    @Column(name = "shipment_completed_by")
    private String shipmentCompletedBy;

    @Column(name = "shipment_completed_on")
    private LocalDateTime shipmentCompletedOn;

    @Column(name = "finance_closed_by")
    private String financeClosedBy;

    @Column(name = "finance_closed_on")
    private LocalDateTime financeClosedOn;

    @OneToOne(fetch = FetchType.LAZY, targetEntity = AdditionalDetails.class)
    @JoinColumn(name = "additional_details_id", referencedColumnName = "id")
    private AdditionalDetails additionalDetails;

    @OneToMany(fetch = FetchType.LAZY, mappedBy = "entityId")
    @Where(clause = "entity_name = 'shipments'")
    private List<Logs> logsList;

    @OneToMany(fetch = FetchType.LAZY, mappedBy = "entityId")
    @Where(clause = "entity_type = 'SHIPMENT'")
    private List<Notes> notesList;

    @OneToOne(fetch = FetchType.LAZY, targetEntity = PickupDeliveryDetails.class, cascade = CascadeType.ALL)
    @JoinColumn(name = "delivery_details_id", referencedColumnName = "id")
    private PickupDeliveryDetails deliveryDetails;

    @OneToOne(fetch = FetchType.LAZY, targetEntity = PickupDeliveryDetails.class, cascade = CascadeType.ALL)
    @JoinColumn(name = "pickup_details_id", referencedColumnName = "id")
    private PickupDeliveryDetails pickupDetails;

    @OneToOne(fetch = FetchType.LAZY, targetEntity = Parties.class, cascade = CascadeType.ALL)
    @JoinColumn(name = "client_id", referencedColumnName = "id")
    private Parties client;

    @OneToOne(fetch = FetchType.LAZY, targetEntity = Parties.class, cascade = CascadeType.ALL)
    @JoinColumn(name = "consigner_id", referencedColumnName = "id")
    private Parties consigner;

    @OneToOne(fetch = FetchType.LAZY, targetEntity = Parties.class, cascade = CascadeType.ALL)
    @JoinColumn(name = "consignee_id", referencedColumnName = "id")
    private Parties consignee;

    @OneToMany(fetch = FetchType.LAZY, mappedBy = "shipmentId")
    private List<Jobs> jobsList;

    @ManyToMany
    @JoinTable(name = "console_shipment_mapping",
            joinColumns = @JoinColumn(name = "shipment_id"),
            inverseJoinColumns = @JoinColumn(name = "consolidation_id"))
    private List<ConsolidationDetails> consolidationList;

    @OneToMany(fetch = FetchType.LAZY, mappedBy = "entityId")
    @Where(clause = "entity_type = 'SHIPMENT_ADDRESSES'")
    private List<Parties> shipmentAddresses;

    @Column(name = "job_status")
    private String jobStatus;
}
