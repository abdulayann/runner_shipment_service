package com.dpw.runner.shipment.services.entity;


import com.dpw.runner.shipment.services.aspects.MultitenancyAspect.MultiTenancy;
import com.dpw.runner.shipment.services.commons.constants.Constants;
import com.dpw.runner.shipment.services.entity.enums.CustomerCategoryRates;
import com.dpw.runner.shipment.services.entity.enums.DateBehaviorType;
import com.dpw.runner.shipment.services.entity.enums.FileStatus;
import com.dpw.runner.shipment.services.entity.enums.ShipmentPackStatus;
import com.dpw.runner.shipment.services.masterdata.enums.MasterDataType;
import com.dpw.runner.shipment.services.utils.DedicatedMasterData;
import com.dpw.runner.shipment.services.utils.MasterData;
import com.dpw.runner.shipment.services.utils.OrganizationData;
import com.dpw.runner.shipment.services.utils.TenantIdData;
import com.fasterxml.jackson.annotation.JsonIgnoreProperties;
import com.vladmihalcea.hibernate.type.json.JsonBinaryType;
import lombok.*;
import lombok.experimental.Accessors;
import org.hibernate.annotations.*;

import javax.persistence.*;
import javax.persistence.CascadeType;
import javax.persistence.Entity;
import javax.persistence.Table;
import javax.validation.constraints.Size;
import java.math.BigDecimal;
import java.time.LocalDateTime;
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
@TypeDef(name = "jsonb", typeClass = JsonBinaryType.class)
@SQLDelete(sql = "UPDATE shipment_details SET is_deleted = true WHERE id=?")
@Where(clause = "is_deleted = false")
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
    private List<Containers> containersList;

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
    @Size(max=2048, message = "max size is 2048 for additional_terms")
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

    @Column(name = "triangulation_partner")
    @TenantIdData
    private Long triangulationPartner;

    @Column(name = "receiving_branch")
    @TenantIdData
    private Long receivingBranch;

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
    private List<Notes> notesList;

    @OneToOne(fetch = FetchType.LAZY, targetEntity = PickupDeliveryDetails.class, cascade = CascadeType.ALL)
    @JoinColumn(name = "delivery_details_id", referencedColumnName = "id")
    private PickupDeliveryDetails deliveryDetails;

    @OneToOne(fetch = FetchType.LAZY, targetEntity = PickupDeliveryDetails.class, cascade = CascadeType.ALL)
    @JoinColumn(name = "pickup_details_id", referencedColumnName = "id")
    private PickupDeliveryDetails pickupDetails;

    @OneToOne(fetch = FetchType.LAZY, targetEntity = Parties.class, cascade = CascadeType.ALL)
    @JoinColumn(name = "client_id", referencedColumnName = "id")
    @OrganizationData
    private Parties client;

    @OneToOne(fetch = FetchType.LAZY, targetEntity = Parties.class, cascade = CascadeType.ALL)
    @JoinColumn(name = "consigner_id", referencedColumnName = "id")
    @OrganizationData
    private Parties consigner;

    @OneToOne(fetch = FetchType.LAZY, targetEntity = Parties.class, cascade = CascadeType.ALL)
    @JoinColumn(name = "consignee_id", referencedColumnName = "id")
    @OrganizationData
    private Parties consignee;

    @OneToMany(fetch = FetchType.LAZY, mappedBy = "shipmentId")
    private List<Jobs> jobsList;

    @ManyToMany(fetch = FetchType.LAZY)
    @JoinTable(name = "console_shipment_mapping",
            joinColumns = @JoinColumn(name = "shipment_id"),
            inverseJoinColumns = @JoinColumn(name = "consolidation_id"))
    @JsonIgnoreProperties(value = {"shipmentsList", "containersList"}, allowSetters = true)
    @WhereJoinTable(clause = "is_attachment_done = 'True'")
    private List<ConsolidationDetails> consolidationList;

    @OneToMany(fetch = FetchType.LAZY, mappedBy = "entityId")
    @Where(clause = "entity_type = 'SHIPMENT_ADDRESSES'")
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

    @Enumerated(EnumType.STRING)
    @Column(name = "customer_category")
    private CustomerCategoryRates customerCategory;

    @Column(name = "contract_id")
    @Size(max=64, message = "max size is 64 for contract_id")
    private String contractId;

    @Column(name = "contract_type")
    @Size(max=64, message = "max size is 64 for contract_type")
    private String contractType;

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
}
