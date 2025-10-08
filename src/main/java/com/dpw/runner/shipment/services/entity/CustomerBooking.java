package com.dpw.runner.shipment.services.entity;


import com.dpw.runner.shipment.services.aspects.MultitenancyAspect.MultiTenancy;
import com.dpw.runner.shipment.services.entity.enums.*;
import com.dpw.runner.shipment.services.masterdata.enums.MasterDataType;
import com.dpw.runner.shipment.services.utils.MasterData;
import com.dpw.runner.shipment.services.utils.OrganizationData;
import com.dpw.runner.shipment.services.utils.OrganizationMasterData;
import lombok.*;
import lombok.experimental.Accessors;
import org.hibernate.annotations.BatchSize;
import org.hibernate.annotations.SQLDelete;
import org.hibernate.annotations.Where;

import javax.persistence.*;
import javax.validation.constraints.Size;
import java.math.BigDecimal;
import java.time.LocalDateTime;
import java.util.List;
import java.util.UUID;


@Entity
@Data
@Table(name = "customer_booking")
@Accessors(chain = true)
@ToString(onlyExplicitlyIncluded = true)
@NoArgsConstructor
@AllArgsConstructor
@Builder
@SQLDelete(sql = "UPDATE customer_booking SET is_deleted = true WHERE id=?")
@Where(clause = "is_deleted = false")
public class CustomerBooking extends MultiTenancy {

    @Enumerated(EnumType.STRING)
    @Column(name = "booking_status")
    private BookingStatus bookingStatus;

    @OneToOne(targetEntity = Parties.class, cascade = CascadeType.ALL)
    @JoinColumn(name = "customer_id", referencedColumnName = "id")
    @OrganizationData
    private Parties customer;

    @Column(name = "is_customer_free_text")
    private Boolean isCustomerFreeText;

    @OneToOne(targetEntity = Parties.class, cascade = CascadeType.ALL)
    @JoinColumn(name = "consignor_id", referencedColumnName = "id")
    @OrganizationData
    private Parties consignor;

    @Column(name = "is_consignor_free_text")
    private Boolean isConsignorFreeText;

    @OneToOne(targetEntity = Parties.class, cascade = CascadeType.ALL)
    @JoinColumn(name = "consignee_id", referencedColumnName = "id")
    @OrganizationData
    private Parties consignee;

    @Column(name = "is_consignee_free_text")
    private Boolean isConsigneeFreeText;

    @OneToOne(targetEntity = Parties.class, cascade = CascadeType.ALL)
    @JoinColumn(name = "notify_party_id", referencedColumnName = "id")
    @OrganizationData
    private Parties notifyParty;

    @Column(name = "is_notify_party_free_text")
    private Boolean isNotifyPartyFreeText;

    @OneToMany(fetch = FetchType.LAZY, mappedBy = "entityId")
    @Where(clause = "entity_type = 'BOOKING_ADDITIONAL_PARTY'")
    @BatchSize(size = 50)
    private List<Parties> additionalParties;

    @Column(name = "customer_email")
    private String customerEmail;

    @Column(name = "booking_number")
    private String bookingNumber;

    @Column(name = "booking_date")
    private LocalDateTime bookingDate;

    @Column(name = "inco_terms")
    @MasterData(type = MasterDataType.INCOTERMS)
    private String incoTerms;

    @OneToOne(targetEntity = CarrierDetails.class, cascade = CascadeType.ALL)
    @JoinColumn(name = "carrier_detail_id", referencedColumnName = "id")
    private CarrierDetails carrierDetails;

    @Column(name = "transport_type")
    @MasterData(type = MasterDataType.TRANSPORT_MODE)
    private String transportType; //SEA, AIR

    @Column(name = "cargo_type")
    @MasterData(type = MasterDataType.CONTAINER_CATEGORY)
    private String cargoType; //LCL, FCL, LSE

    @Column(name = "direction")
    @MasterData(type = MasterDataType.CUSTOM_SHIPMENT_TYPE)
    private String direction;

    @Column(name = "cargo_delivery_date")
    private LocalDateTime cargoDeliveryDate;

    @Column(name = "quantity")
    private Integer quantity;

    @Column(name = "quantity_unit")
    @MasterData(type = MasterDataType.PACKS_UNIT)
    private String quantityUnit;

    @Column(name = "gross_weight")
    private BigDecimal grossWeight;

    @Column(name = "gross_weight_unit")
    @MasterData(type = MasterDataType.WEIGHT_UNIT)
    private String grossWeightUnit;

    @Column(name = "volume")
    private BigDecimal volume;

    @Column(name = "volume_unit")
    @MasterData(type = MasterDataType.VOLUME_UNIT)
    private String volumeUnit;

    @Column(name = "weight_volume")
    private BigDecimal weightVolume;

    @Column(name = "weight_volume_unit")
    @MasterData(type = MasterDataType.WEIGHT_UNIT)
    private String weightVolumeUnit;

    @Column(name = "chargeable")
    private BigDecimal chargeable;

    @Column(name = "chargeable_unit")
    @MasterData(type = MasterDataType.VOLUME_UNIT)
    private String chargeableUnit;

    @Column(name = "contract_id")
    private String contractId;

    @OneToMany(fetch = FetchType.LAZY, mappedBy = "bookingId")
    @BatchSize(size = 50)
    private List<Containers> containersList;

    @OneToMany(fetch = FetchType.LAZY, mappedBy = "bookingId")
    @BatchSize(size = 50)
    private List<Packing> packingList;

    @OneToMany(fetch = FetchType.LAZY, mappedBy = "bookingId")
    @OrderBy("leg ASC")
    @BatchSize(size = 50)
    private List<Routings> routingList;

    @OneToMany(fetch = FetchType.LAZY, mappedBy = "bookingId")
    @BatchSize(size = 50)
    private List<ReferenceNumbers> referenceNumbersList;

    @OneToMany(fetch = FetchType.LAZY, mappedBy = "entityId")
    @Where(clause = "entity_type = 'BOOKING'")
    @BatchSize(size = 50)
    private List<FileRepo> fileRepoList;

    @OneToMany(fetch = FetchType.LAZY, mappedBy = "bookingId")
    @BatchSize(size = 50)
    private List<BookingCharges> bookingCharges;

    @Column(name = "is_platform_booking_created")
    private Boolean isPlatformBookingCreated;

    @Column(name = "contract_status")
    private String contractStatus;

    @Column(name = "source")
    @Enumerated(EnumType.STRING)
    private BookingSource source;

    @Column(name = "business_code")
    private String businessCode;

    @MasterData(type = MasterDataType.SERVICE_MODE)
    @Column(name = "service_mode")
    private String serviceMode;

    @Column(name = "shipment_id")
    private String shipmentId;

    @Column(name = "shipment_entity_id")
    private String shipmentEntityId;

    @Column(name = "shipment_entity_id_v2")
    private String shipmentEntityIdV2;

    @Column(name = "shipment_guid")
    private String shipmentGuid;

    @Column(name = "auto_update_weight_volume")
    private Boolean isAutoWeightVolumeUpdate;

    @Column(name = "fmc_tlc_id")
    private String fmcTlcId;

    @Column(name = "is_package_manual")
    private Boolean isPackageManual;

    @Column(name = "is_consignor_address_free_text")
    private Boolean isConsignorAddressFreeText;

    @Column(name = "is_consignee_address_free_text")
    private Boolean isConsigneeAddressFreeText;

    @Column(name = "is_customer_address_free_text")
    private Boolean isCustomerAddressFreeText;

    @Column(name = "is_notify_party_address_free_text")
    private Boolean isNotifyPartyAddressFreeText;

    @Column(name = "total_revenue")
    private BigDecimal totalRevenue;

    @Column(name = "shipment_created_date")
    private LocalDateTime shipmentCreatedDate;

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

    @Column(name = "parent_contract_id")
    private String parentContractId;

    @Column(name = "quote_filter_start_date")
    private LocalDateTime quoteDate;

    @Enumerated(EnumType.STRING)
    @Column(name = "quote_filter_start_date_type")
    private CustomerBookingQuoteDateType quoteDateType;

    @Column(name = "primary_sales_agent_email")
    private String primarySalesAgentEmail;

    @Column(name = "secondary_sales_agent_email")
    private String secondarySalesAgentEmail;

    @Column(name = "sales_branch")
    private String salesBranch;

    @Column(name = "is_notify_consignee_equal")
    private Boolean isNotifyConsigneeEqual;

    @Column(name = "is_shipper_client_equal")
    private Boolean isShipperClientEqual;

    @Column(name = "is_consignee_client_equal")
    private Boolean isConsigneeClientEqual;

    @Column(name = "is_bill_created")
    private Boolean isBillCreated;

    @Column(name = "current_party_for_quote")
    private String currentPartyForQuote;

    @Column(name = "source_guid")
    private UUID sourceGuid;

    @Column(name = "order_management_id")
    private String orderManagementId;

    @Column(name = "order_management_number")
    private String orderManagementNumber;

    @Column(name = "is_dg")
    private Boolean isDg;

    @Column(name = "rejection_remarks")
    private String rejectionRemarks;

    @Column(name = "payment_terms")
    @MasterData(type = MasterDataType.PAYMENT)
    private String paymentTerms;

    @Column(name = "shipment_reference_number")
    private String shipmentReferenceNumber;

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
    @Size(max = 64)
    private String controlledReferenceNumber;

    @Column(name = "partner")
    @MasterData(type = MasterDataType.ORDER_DPW)
    private String partner;

    @Column(name = "booking_agent")
    @OrganizationMasterData
    private Long bookingAgent;

    @Column(name = "co_load_carrier_name")
    @MasterData(type = MasterDataType.CARRIER)
    @Size(max = 64)
    private String coLoadCarrierName;

    @Column(name = "partner_bkg_number")
    @Size(max = 64)
    private String partnerBkgNumber;

    @Column(name = "partner_bl_or_awb_number")
    @Size(max = 64)
    private String partnerBLOrAWBNumber;

    @Column(name = "carrier_bkg_number")
    @Size(max = 64)
    private String carrierBookingNumber;

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

    @Column(name = "pickup_at_origin_date")
    private LocalDateTime pickupAtOriginDate;

    @Column(name = "delivery_at_destination_date")
    private LocalDateTime deliveryAtDestinationDate;

    @Column(name = "est_pickup_at_origin_date")
    private LocalDateTime estimatedPickupAtOriginDate;

    @Column(name = "est_delivery_at_destination_date")
    private LocalDateTime estimatedDeliveryAtDestinationDate;

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

    @Column(name = "est_brokerage_at_origin_date")
    private LocalDateTime estimatedBrokerageAtOriginDate;

    @Column(name = "est_brokerage_at_destination_date")
    private LocalDateTime estimatedBrokerageAtDestinationDate;

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

    @Column(name = "carrier_doc_cut_off")
    private LocalDateTime carrierDocCutOff;

    @Column(name = "cargo_receipt_w_h_cut_off")
    private LocalDateTime cargoReceiptWHCutOff;

    @Column(name = "last_free_date_cut_off")
    private LocalDateTime lastFreeDateCutOff;

    @Column(name = "number_of_free_days_cut_off")
    private Integer numberOfFreeDaysCutOff;

    @Column(name = "earliest_empty_equipment_pickup")
    private LocalDateTime earliestEmptyEquipmentPickUp;

    @Column(name = "latest_full_equipment_delivered_to_carrier")
    private LocalDateTime latestFullEquipmentDeliveredToCarrier;

    @Column(name = "earliest_drop_off_full_equipment_to_carrier")
    private LocalDateTime earliestDropOffFullEquipmentToCarrier;

    @Column(name = "latest_arrival_time")
    private LocalDateTime latestArrivalTime;

    @Column(name = "teu_count")
    private BigDecimal teuCount;

    @Column(name = "containers")
    private Long containers;

    @Column(name = "package_type")
    @MasterData(type = MasterDataType.PACKS_UNIT)
    private String packageType;

    @Column(name = "packages")
    private Long packages;

    @Column(name = "description")
    private String description;

    @Column(name = "marks_n_numbers")
    private String marksnNumbers;

    @Column(name = "additional_terms")
    private String additionalTerms;

    @Column(name = "migration_status")
    @Enumerated(EnumType.STRING)
    private MigrationStatus migrationStatus;

    @Column(name = "integration_source")
    private String integrationSource;
}
