package com.dpw.runner.shipment.services.entity;


import com.dpw.runner.shipment.services.aspects.MultitenancyAspect.MultiTenancy;
import com.dpw.runner.shipment.services.commons.constants.Constants;
import com.dpw.runner.shipment.services.commons.enums.TransportInfoStatus;
import com.dpw.runner.shipment.services.entity.enums.*;
import com.dpw.runner.shipment.services.masterdata.enums.MasterDataType;
import com.dpw.runner.shipment.services.utils.DedicatedMasterData;
import com.dpw.runner.shipment.services.utils.MasterData;
import com.dpw.runner.shipment.services.utils.OrganizationData;
import com.dpw.runner.shipment.services.utils.OrganizationMasterData;
import com.dpw.runner.shipment.services.utils.TenantIdData;
import com.fasterxml.jackson.annotation.JsonIgnoreProperties;
import com.vladmihalcea.hibernate.type.json.JsonBinaryType;
import lombok.*;
import lombok.experimental.Accessors;
import org.hibernate.annotations.*;

import javax.persistence.CascadeType;
import javax.persistence.Entity;
import javax.persistence.OrderBy;
import javax.persistence.Table;
import javax.persistence.*;
import javax.validation.constraints.Size;
import java.math.BigDecimal;
import java.time.LocalDateTime;
import java.util.List;
import java.util.Objects;
import java.util.Set;
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
@TypeDef(name = "jsonb", typeClass = JsonBinaryType.class)
@SQLDelete(sql = "UPDATE shipment_details SET is_deleted = true WHERE id=?")
@Where(clause = "is_deleted = false")
@SuppressWarnings("java:S6539")
public class ShipmentDetails extends MultiTenancy {

    private static final long serialVersionUID = 190794279984274725L;

    @OneToOne(fetch = FetchType.LAZY, targetEntity = CarrierDetails.class, cascade = CascadeType.ALL)
    @JoinColumn(name = "carrier_detail_id", referencedColumnName = "id")
    private CarrierDetails carrierDetails;

    @Column(name = "house_bill")
    private String houseBill;

    @Column(name = "transport_mode")
    @Size(max=4, message = "max size is 4 for transport_mode")
    @MasterData(type = MasterDataType.TRANSPORT_MODE)
    private String transportMode;

    @Column(name = "direction")
    @MasterData(type = MasterDataType.CUSTOM_SHIPMENT_TYPE)
    private String direction;

    @Column(name = "shipment_type")
    @Size(max=3, message = "max size is 3 for shipment_type")
    @MasterData(type = MasterDataType.CONTAINER_CATEGORY, cascade = Constants.TRANSPORT_MODE)
    private String shipmentType;

    @ManyToMany(fetch = FetchType.LAZY)
    @JoinTable(name = "shipments_containers_mapping",
            joinColumns = @JoinColumn(name = "shipment_id"),
            inverseJoinColumns = @JoinColumn(name = "container_id"))
    @JsonIgnoreProperties(value = "shipmentsList", allowSetters = true)
    @BatchSize(size = 50)
    private Set<Containers> containersList;

    @Column(name = "status")
    private Integer status;

    @Column(name = "source")
    @MasterData(type = MasterDataType.SOURCE_TYPE)
    private String source;

    @Column(name = "job_type")
    @MasterData(type = MasterDataType.SHIPMENT_TYPE)
    private String jobType;

    @Column(name = "service_type")
    @Size(max=3, message = "max size is 3 for service Type")
    @MasterData(type = MasterDataType.SERVICE_MODE)
    private String serviceType;

    @Column(name = "master_bill")
    @Size(max=50, message = "max size is 50 for master_bill")
    private String masterBill;

    @Column(name = "booking_reference")
    private String bookingReference;

    @Column(name = "console_ref")
    private String consolRef;

    @Column(name = "sales_agent")
    @DedicatedMasterData(type = Constants.SALES_AGENT)
    private Long salesAgent;

    @Column(name = "payment_terms")
    @MasterData(type = MasterDataType.PAYMENT)
    private String paymentTerms;

    @Column(name = "incoterms")
    @MasterData(type = MasterDataType.INCOTERMS)
    private String incoterms;

    @Column(name = "shipment_id")
    @Size(max=50, message = "max size is 50 for shipment_id")
    private String shipmentId;

    @Column(name = "is_domestic")
    private Boolean isDomestic;

    @Column(name = "assigned_to")
    private String assignedTo;

    @Column(name = "additional_terms")
    @Size(max=25000, message = "max size is 25000 for additional_terms")
    private String additionalTerms;

    @Column(name = "goods_description")
    private String goodsDescription;

    @OneToMany(fetch = FetchType.LAZY, mappedBy = "shipmentId")
    @BatchSize(size = 50)
    private List<BookingCarriage> bookingCarriagesList;

    @OneToMany(fetch = FetchType.LAZY, mappedBy = "shipmentId")
    @BatchSize(size = 50)
    private List<ELDetails> elDetailsList;

    @OneToMany(fetch = FetchType.LAZY, mappedBy = "entityId")
    @Where(clause = "entity_type = 'SHIPMENT'")
    @BatchSize(size = 50)
    private List<Events> eventsList;


    @OneToMany(fetch = FetchType.LAZY, mappedBy = "shipmentId")
    @BatchSize(size = 50)
    private List<Packing> packingList;

    @OneToMany(fetch = FetchType.LAZY, mappedBy = "shipmentId")
    @BatchSize(size = 50)
    private List<ReferenceNumbers> referenceNumbersList;

    @OneToMany(fetch = FetchType.LAZY, mappedBy = "shipmentId")
    @OrderBy("leg ASC")
    @BatchSize(size = 50)
    private List<Routings> routingsList;

    @OneToMany(fetch = FetchType.LAZY, mappedBy = "shipmentId")
    @BatchSize(size = 50)
    private List<ServiceDetails> servicesList;

    @OneToMany(fetch = FetchType.LAZY, mappedBy = "shipmentId")
    @BatchSize(size = 50)
    private List<TruckDriverDetails> truckDriverDetails;

    @Column(name = "weight")
    private BigDecimal weight;

    @Column(name = "weight_unit")
    @MasterData(type = MasterDataType.WEIGHT_UNIT)
    private String weightUnit;

    @Column(name = "volume")
    private BigDecimal volume;

    @Column(name = "volume_unit")
    @Size(max=10, message = "max size is 10 for volume_unit")
    @MasterData(type = MasterDataType.VOLUME_UNIT)
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
    @MasterData(type = MasterDataType.WEIGHT_UNIT)
    private String netWeightUnit;

    @Column(name = "no_of_packs")
    private Integer noOfPacks;

    @Column(name = "packs_unit")
    @MasterData(type = MasterDataType.PACKS_UNIT)
    private String packsUnit;

    @Column(name = "inner_packs")
    private Integer innerPacks;

    @Column(name = "inner_pack_unit")
    @MasterData(type = MasterDataType.PACKS_UNIT)
    private String innerPackUnit;

    @Column(name = "freight_local")
    private BigDecimal freightLocal;

    @Column(name = "freightLocal_Currency")
    private String freightLocalCurrency;

    @Column(name = "freight_overseas")
    private BigDecimal freightOverseas;

    @Column(name = "freightOverseas_Currency")
    @DedicatedMasterData(type = Constants.CURRENCY_MASTER_DATA)
    private String freightOverseasCurrency;

    @Column(name = "auto_update_wt_vol")
    private Boolean autoUpdateWtVol;

    @Column(name = "container_auto_wv_update")
    private Boolean containerAutoWeightVolumeUpdate;

    @Column(name = "marks_num")
    private String marksNum;

    @Column(name = "entry_detail")
    @Size(max = 3, message = "max size is 3 for entry_detail")
    @MasterData(type = MasterDataType.ENTRY_DETAILS)
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
    private Boolean cargoFinanceBooking;

    @Column(name = "booking_number")
    private String bookingNumber;

    @Column(name = "route")
    private String route;

    @Column(name = "source_tenant_id")
    @TenantIdData
    private Long sourceTenantId;

    @Column(name = "documentation_partner")
    @TenantIdData
    private Long documentationPartner;

    @ElementCollection(fetch = FetchType.LAZY)
    @CollectionTable(name = "triangulation_partner_shipment", joinColumns = @JoinColumn(name = "shipment_id"))
    @BatchSize(size = 50)
    private List<TriangulationPartner> triangulationPartnerList;

    @Column(name = "triangulation_partner")
    @TenantIdData
    private Long triangulationPartner;

    @Column(name = "receiving_branch")
    @TenantIdData
    private Long receivingBranch;

    @Column(name = "origin_branch")
    @TenantIdData
    private Long originBranch;

    @Column(name = "intra_branch")
    private Boolean intraBranch;

    @Column(name = "prev_shipment_status")
    private Integer prevShipmentStatus;

    @Column(name = "is_shipment_read_only")
    private Boolean isShipmentReadOnly;

    @Column(name = "shipment_created_on")
    private LocalDateTime shipmentCreatedOn;

    @Column(name = "shipment_completed_by")
    private String shipmentCompletedBy;

    @Column(name = "shipment_completed_on")
    private LocalDateTime shipmentCompletedOn;

    @Column(name = "finance_closed_by")
    private String financeClosedBy;

    @Column(name = "finance_closed_on")
    private LocalDateTime financeClosedOn;

    @Column(name = "goods_value")
    public BigDecimal goodsValue;

    @Column(name = "goods_value_currency")
    @Size(max=3, message = "max size is 3 for goods_value_currency")
    public String goodsValueCurrency;

    @Column(name = "insurance_value")
    public BigDecimal insuranceValue;

    @Column(name = "insurance_value_currency")
    @Size(max=3, message = "max size is 3 for insurance_value_currency")
    public String InsuranceValueCurrency;

    @OneToOne(fetch = FetchType.LAZY, targetEntity = AdditionalDetails.class, cascade = CascadeType.ALL)
    @JoinColumn(name = "additional_details_id", referencedColumnName = "id")
    private AdditionalDetails additionalDetails;

    @OneToMany(fetch = FetchType.LAZY, mappedBy = "entityId")
    @Where(clause = "entity_type = 'SHIPMENT'")
    @BatchSize(size = 50)
    private List<Notes> notesList;

    @OneToOne(fetch = FetchType.LAZY, targetEntity = PickupDeliveryDetails.class, cascade = CascadeType.ALL)
    @JoinColumn(name = "delivery_details_id", referencedColumnName = "id")
    private PickupDeliveryDetails deliveryDetails;

    @Column(name = "delivery_details_id", insertable = false, updatable = false)
    private Long deliveryDetailsId;

    @OneToOne(fetch = FetchType.LAZY, targetEntity = PickupDeliveryDetails.class, cascade = CascadeType.ALL)
    @JoinColumn(name = "pickup_details_id", referencedColumnName = "id")
    private PickupDeliveryDetails pickupDetails;

    @Column(name = "pickup_details_id", insertable = false, updatable = false)
    private Long pickupDetailsId;

    @OneToOne(fetch = FetchType.LAZY, targetEntity = Parties.class, cascade = CascadeType.ALL)
    @JoinColumn(name = "client_id", referencedColumnName = "id")
    @OrganizationData
    private Parties client;

    @Column(name = "client_id", insertable = false, updatable = false)
    private Long clientId;

    @Column(name = "consigner_id", insertable = false, updatable = false)
    private Long consignerId;

    @Column(name = "consignee_id", insertable = false, updatable = false)
    private Long consigneeId;

    @Column(name = "carrier_detail_id", insertable = false, updatable = false)
    private Long carrierDetailId;

    @Column(name = "additional_details_id", insertable = false, updatable = false)
    private Long additionalDetailId;

    @OneToOne(fetch = FetchType.LAZY, targetEntity = Parties.class, cascade = CascadeType.ALL)
    @JoinColumn(name = "consigner_id", referencedColumnName = "id")
    @OrganizationData
    private Parties consigner;

    @OneToOne(fetch = FetchType.LAZY, targetEntity = Parties.class, cascade = CascadeType.ALL)
    @JoinColumn(name = "consignee_id", referencedColumnName = "id")
    @OrganizationData
    private Parties consignee;

    @OneToMany(fetch = FetchType.LAZY, mappedBy = "shipmentId")
    @BatchSize(size = 50)
    private List<Jobs> jobsList;

    @ManyToMany(fetch = FetchType.LAZY)
    @JoinTable(name = "console_shipment_mapping",
            joinColumns = @JoinColumn(name = "shipment_id"),
            inverseJoinColumns = @JoinColumn(name = "consolidation_id"))
    @JsonIgnoreProperties(value = {"shipmentsList", "containersList"}, allowSetters = true)
    @WhereJoinTable(clause = "is_attachment_done = 'True'")
    @BatchSize(size = 50)
    private Set<ConsolidationDetails> consolidationList;

    @OneToMany(fetch = FetchType.LAZY, mappedBy = "entityId")
    @Where(clause = "entity_type = 'SHIPMENT_ADDRESSES'")
    @BatchSize(size = 50)
    private List<Parties> shipmentAddresses;

    @Column(name = "job_status")
    @Size(max=3, message = "max size is 3 for job_status")
    @MasterData(type = MasterDataType.BILL_JOBS)
    private String jobStatus;

    @Column(name = "file_status")
    @Enumerated(EnumType.STRING)
    private FileStatus fileStatus;

    @Column(name = "entry_ref_no")
    @Size(max=250, message = "max size is 250 for entry_ref_no")
    private String entryRefNo;

    @Column(name = "flight_status")
    private String flightStatus;

    @Column(name = "contains_hazardous")
    private Boolean containsHazardous = false;

    @Column(name = "fmc_tlc_id")
    private String fmcTlcId;

    @Column(name = "commodity")
    private String commodity;

    @Column(name = "order_number")
    private Long orderNumber;

    @Column(name = "order_management_id")
    private String orderManagementId;

    @Column(name = "order_management_number")
    private String orderManagementNumber;

    @OneToMany(fetch = FetchType.LAZY, mappedBy = "shipmentId")
    @BatchSize(size = 50)
    private List<ShipmentOrder> shipmentOrders;

    @Enumerated(EnumType.STRING)
    @Column(name = "customer_category")
    private CustomerCategoryRates customerCategory;

    @Column(name = "contract_id")
    @Size(max=64, message = "max size is 64 for contract_id")
    private String contractId;

    @Column(name = "contract_type")
    @Size(max=64, message = "max size is 64 for contract_type")
    private String contractType;

    @Column(name = "parent_contract_id")
    private String parentContractId;

    @MasterData(type = MasterDataType.COUNTRIES)
    @Column(name = "client_country")
    private String clientCountry;

    @MasterData(type = MasterDataType.COUNTRIES)
    @Column(name = "consignor_country")
    private String consignorCountry;

    @MasterData(type = MasterDataType.COUNTRIES)
    @Column(name = "consignee_country")
    private String consigneeCountry;

    @MasterData(type = MasterDataType.COUNTRIES)
    @Column(name = "notify_party_country")
    private String notifyPartyCountry;

    @Column(name = "primary_sales_agent_email")
    private String primarySalesAgentEmail;

    @Column(name = "secondary_sales_agent_email")
    private String secondarySalesAgentEmail;

    @Column(name = "sales_branch")
    private String salesBranch;

    @Column(name = "cloned_guid")
    private UUID clonedGuid;

    @Column(name = "source_guid")
    private UUID sourceGuid;

    @Column(name = "consignee_dps_address_id")
    private Long consigneeDpsAddressId;

    @Column(name = "client_dps_address_id")
    private Long clientDpsAddressId;

    @Column(name = "consignor_dps_address_id")
    private Long consignorDpsAddressId;

    @Column(name = "notify_party_dps_address_id")
    private Long notifyPartyDpsAddressId;

    @Column(name = "booking_created_date")
    private LocalDateTime bookingCreatedDate;

    @Column(name = "security_status")
    private String securityStatus;

    @Column(name = "current_party_for_quote")
    private String currentPartyForQuote;

    @Column(name = "entity_transfer")
    private Boolean entityTransfer;

    @Column(name = "destination_sales_branch")
    private String destinationSalesBranch;

    @Column(name = "destination_primary_sales_agent_email")
    private String destinationPrimarySalesAgentEmail;

    @Column(name = "destination_secondary_sales_agent_email")
    private String destinationSecondarySalesAgentEmail;

    @Column(name = "destination_current_party_for_quote")
    private String destinationCurrentPartyForQuote;

    @Column(name = "destination_contract_id")
    private String destinationContractId;

    @Column(name = "destination_contract_type")
    private String destinationContractType;

    @OneToMany(fetch = FetchType.LAZY, mappedBy = "shipmentId")
    @BatchSize(size = 50)
    private List<PickupDeliveryDetails> pickupDeliveryDetailsInstructions;

     @Column(name = "date_type")
     @Enumerated(EnumType.STRING)
     private DateBehaviorType dateType;

     @Column(name = "shipment_gate_in_date")
     private LocalDateTime shipmentGateInDate;

     @Column(name = "shipment_pack_status")
     @Enumerated(EnumType.STRING)
     private ShipmentPackStatus shipmentPackStatus;

     @Column(name = "cargo_ready_date")
     private LocalDateTime cargoReadyDate;

     @Column(name = "cargo_delivery_date")
     private LocalDateTime cargoDeliveryDate;

    @Column(name = "is_receiving_branch_added")
    private Boolean isReceivingBranchAdded;

    @OneToMany(fetch = FetchType.LAZY, mappedBy = "shipmentId")
    @Where(clause = "is_attachment_done = 'false'")
    @BatchSize(size = 50)
    private List<ConsoleShipmentMapping> consoleShipmentMappings;

    @Column(name = "department")
    @Size(max=32, message = "max size is 32 for department")
    @MasterData(type = MasterDataType.DEPARTMENT_MASTER_LIST)
    private String department;
    
     @Column(name = "ocean_dg_status")
     @Enumerated(EnumType.STRING)
     private OceanDGStatus oceanDGStatus;

     @Column(name = "sync_routing_from_consolidation")
     private Boolean syncRoutingFromConsolidation;

    @Column(name = "is_network_file")
    private Boolean isNetworkFile;

    @Column(name = "is_receiving_branch_manually")
    private Boolean isReceivingBranchManually;

    @Column(name = "is_transferred_to_receiving_branch")
    private Boolean isTransferredToReceivingBranch;

    @Column(name = "b2b")
    private Boolean b2b;

    @Column(name = "is_co_load_enabled")
    private Boolean isCoLoadEnabled;

    @Column(name = "co_load_carrier_name")
    @MasterData(type = MasterDataType.CARRIER)
    @Size(max = 64)
    private String coLoadCarrierName;

    @Column(name = "co_load_bl_number")
    @Size(max = 50)
    private String coLoadBlNumber;

    @Column(name = "issuing_carrier_name")
    @MasterData(type = MasterDataType.CARRIER)
    @Size(max = 64)
    private String issuingCarrierName;

    @Column(name = "ocean_bl_number")
    @Size(max = 64)
    private String oceanBlNumber;

    @Column(name = "customer_booking_guid")
    private UUID customerBookingGuid;

    @Column(name = "is_frob")
    private Boolean isFrob;

    @Column(name = "is_reefer")
    private Boolean isReefer = false;

    @Column(name = "incoterms_location")
    @Size(max = 64)
    private String incotermsLocation;

    @Column(name = "cargo_readiness_date")
    private LocalDateTime cargoReadinessDate;

    @Column(name = "controlled")
    private Boolean controlled;

    @Column(name = "controlled_reference_number")
    @Size(max = 50)
    private String controlledReferenceNumber;

    @Column(name = "partner")
    @MasterData(type = MasterDataType.ORDER_DPW)
    private String partner;

    @Column(name = "booking_agent")
    @OrganizationMasterData
    private Long bookingAgent;

    @Column(name = "co_load_bkg_number")
    @Size(max = 50)
    private String coLoadBkgNumber;

    @Column(name = "pickup_at_origin_type")
    @MasterData(type = MasterDataType.ORDER_DPW)
    private String pickupAtOriginType;

    @Column(name = "delivery_at_destination_type")
    @MasterData(type = MasterDataType.ORDER_DPW)
    private String deliveryAtDestinationType;

    @Column(name = "brokerage_at_origin_type")
    @MasterData(type = MasterDataType.ORDER_DPW)
    private String brokerageAtOriginType;

    @Column(name = "brokerage_at_destination_type")
    @MasterData(type = MasterDataType.ORDER_DPW)
    private String brokerageAtDestinationType;

    @Column(name = "pickup_at_origin")
    @OrganizationMasterData
    private Long pickupAtOrigin;

    @Column(name = "delivery_at_destination")
    @OrganizationMasterData
    private Long deliveryAtDestination;

    @Column(name = "brokerage_at_origin")
    @OrganizationMasterData
    private Long brokerageAtOrigin;

    @Column(name = "brokerage_at_destination")
    @OrganizationMasterData
    private Long brokerageAtDestination;

    @Column(name = "brokerage_at_origin_date")
    private LocalDateTime brokerageAtOriginDate;

    @Column(name = "brokerage_at_destination_date")
    private LocalDateTime brokerageAtDestinationDate;

    @Column(name = "terminal_cut_off")
    private LocalDateTime terminalCutoff;

    @Column(name = "verified_gross_mass_cut_off")
    private LocalDateTime verifiedGrossMassCutoff;

    @Column(name = "shipping_instruction_cutoff")
    private LocalDateTime shippingInstructionCutoff;

    @Column(name = "dg_cut_off")
    private LocalDateTime dgCutoff;

    @Column(name = "reefer_cut_off")
    private LocalDateTime reeferCutoff;

    @Column(name = "earliest_empty_equipment_pickup")
    private LocalDateTime earliestEmptyEquipmentPickUp;

    @Column(name = "latest_full_equipment_delivered_to_carrier")
    private LocalDateTime latestFullEquipmentDeliveredToCarrier;

    @Column(name = "earliest_drop_off_full_equipment_to_carrier")
    private LocalDateTime earliestDropOffFullEquipmentToCarrier;

    @Column(name = "latest_arrival_time")
    private LocalDateTime latestArrivalTime;

    @Column(name = "container_assigned_to_shipment_cargo")
    private Long containerAssignedToShipmentCargo;

    @Column(name = "is_borrowed")
    private Boolean isBorrowed;

    @Column(name = "slac")
    private Integer slac;

    @Column(name = "dg_packs_count")
    private Integer dgPacksCount;

    @Column(name = "dg_packs_unit")
    @MasterData(type = MasterDataType.PACKS_UNIT)
    private String dgPacksUnit;

    @Column(name = "transport_info_status")
    @Enumerated(EnumType.STRING)
    private TransportInfoStatus transportInfoStatus;

    @Column(name = "migration_status")
    @Enumerated(EnumType.STRING)
    private MigrationStatus migrationStatus;

    @Override
    public boolean equals(Object o) {
        if (this == o) return true;
        if (o == null || getClass() != o.getClass()) return false;
        ShipmentDetails that = (ShipmentDetails) o;
        return Objects.equals(getId(), that.getId());
    }

    @Override
    public int hashCode() {
        return Objects.hash(getId());
    }

}
