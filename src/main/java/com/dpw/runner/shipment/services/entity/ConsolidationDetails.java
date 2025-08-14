package com.dpw.runner.shipment.services.entity;

import com.dpw.runner.shipment.services.aspects.MultitenancyAspect.MultiTenancy;
import com.dpw.runner.shipment.services.commons.constants.Constants;
import com.dpw.runner.shipment.services.commons.enums.TransportInfoStatus;
import com.dpw.runner.shipment.services.entity.enums.MigrationStatus;
import com.dpw.runner.shipment.services.masterdata.enums.MasterDataType;
import com.dpw.runner.shipment.services.utils.*;
import com.fasterxml.jackson.annotation.JsonIgnoreProperties;
import lombok.*;
import lombok.experimental.Accessors;
import org.hibernate.annotations.BatchSize;
import org.hibernate.annotations.SQLDelete;
import org.hibernate.annotations.Where;
import org.hibernate.annotations.WhereJoinTable;

import javax.persistence.*;
import javax.validation.constraints.Size;
import java.time.LocalDateTime;
import java.util.List;
import java.util.Objects;
import java.util.Set;
import java.util.UUID;

@Entity
@Table(name = "consolidation_details")
@Getter
@Setter
@Data
@Builder
@NoArgsConstructor
@AllArgsConstructor
@Accessors(chain = true)
@SQLDelete(sql = "UPDATE consolidation_details SET is_deleted = true WHERE id=?")
@Where(clause = "is_deleted = false")
public class ConsolidationDetails extends MultiTenancy {

    @Column(name = "consolidation_number")
    @Size(max=20, message = "max size is 20 for consolidation_number")
    private String consolidationNumber;

    @Column(name = "consolidation_type")
    @Size(max=100, message = "max size is 100 for consolidation_type")
    @MasterData(type = MasterDataType.CONSOlIDATION_TYPE)
    private String consolidationType;

    @Column(name = "transport_mode")
    @Size(max=3, message = "max size is 3 for transport_mode")
    @MasterData(type = MasterDataType.MODE)
    private String transportMode;

    @Column(name = "container_category")
    @Size(max=100, message = "max size is 100 for container_category")
    @MasterData(type = MasterDataType.CONTAINER_CATEGORY, cascade = Constants.TRANSPORT_MODE)
    private String containerCategory;

    @Column(name = "is_domestic")
    private Boolean isDomestic;

    @Column(name = "mawb")
    private String mawb;

    @Column(name = "service_level")
    @Size(max=20, message = "max size is 20 for service_level")
    @MasterData(type = MasterDataType.SERVICE_LEVEL)
    private String serviceLevel;

    @Column(name = "payment")
    @Size(max=3, message = "max size is 3 for payment")
    @MasterData(type = MasterDataType.PAYMENT)
    private String payment;

    @Column(name = "declaration_type")
    @MasterData(type = MasterDataType.CUSTOM_DECL_TYPE)
    private String declarationType;

    @Column(name = "delivery_mode")
    @MasterData(type = MasterDataType.HBL_DELIVERY_MODE, cascade = Constants.TRANSPORT_MODE)
    private String deliveryMode;

    @Column(name = "is_linked")
    private Boolean isLinked;

    @Column(name = "is_charter")
    private Boolean isCharter;

    @Column(name = "reference_number")
    private String referenceNumber;

    @Column(name = "package_type")
    private String packageType;

    @Column(name = "agent_reference")
    @Size(max=64, message = "max size is 64 for agent_reference")
    private String agentReference;

    @Column(name = "co_load_mbl")
    @Size(max=64, message = "max size is 64 for co_load_mbl")
    private String coLoadMBL; // Coloader BL No., booking agent bl no

    @Column(name = "co_load_booking_reference")
    @Size(max=64, message = "max size is 64 for co_load_booking_reference")
    private String coLoadBookingReference; // Coloader Booking Number, booking agent booking number

    @Column(name = "manifest_print")
    @MasterData(type = MasterDataType.PRINT_OPTIONS)
    private String manifestPrint;

    @Column(name = "print_other_docs")
    @MasterData(type = MasterDataType.PRINT_OPTIONS)
    private String printOtherDocs;

    @Column(name = "awb_dims")
    private String awbDims;

    @Column(name = "release_type")
    @MasterData(type = MasterDataType.RELEASE_TYPE)
    private String releaseType;

    @Column(name = "masterbill_issue_date")
    private LocalDateTime masterBillIssueDate;

    @Column(name = "override")
    private Boolean override;

    @Column(name = "estimated_terminal_cutoff")
    private LocalDateTime estimatedTerminalCutoff;

    @Column(name = "terminal_cutoff")
    private LocalDateTime terminalCutoff;

    @Column(name = "verified_gross_mass_cutoff")
    private LocalDateTime verifiedGrossMassCutoff;

    @Column(name = "reefer_cutoff")
    private LocalDateTime reeferCutoff;

    @Column(name = "booking_cutoff")
    private LocalDateTime bookingCutoff;

    @Column(name = "ship_instruction_cutoff")
    private LocalDateTime shipInstructionCutoff;

    @Column(name = "hazardous_booking_cutoff")
    private LocalDateTime hazardousBookingCutoff; // DG Cutoff

    @Column(name = "latest_full_equ_delivered_to_carrier")
    private LocalDateTime latestFullEquDeliveredToCarrier;

    @Column(name = "earliest_drop_off_full_equ_to_carrier")
    private LocalDateTime earliestDropOffFullEquToCarrier;

    @Column(name = "earliest_empty_equ_pick_up")
    private LocalDateTime earliestEmptyEquPickUp;

    @Column(name = "volume_utilization")
    private String volumeUtilization;

    @Column(name = "weight_utilization")
    private String weightUtilization;

    @Column(name = "shipment_type")
    @MasterData(type = MasterDataType.CUSTOM_SHIPMENT_TYPE)
    private String shipmentType;

    @Column(name = "bol")
    @Size(max=50, message = "max size is 50 for bol")
    private String bol;

    @Column(name = "is_cargo_only")
    private Boolean isCargoOnly;

    @Column(name = "is_locked")
    private Boolean isLocked;

    @Column(name = "locked_by")
    private String lockedBy;

    @Column(name = "special_instructions")
    private String specialInstructions;

    @Column(name = "description")
    private String description;

    @Column(name = "marks_n_nums")
    private String marksnNums;

    @Column(name = "additional_terms")
    private String AdditionalTerms;

    @Column(name = "docs_closing_time")
    private LocalDateTime docsClosingTime;

    @Column(name = "cargo_closing_time")
    private LocalDateTime cargoClosingTime;

    @Column(name = "mrn_number")
    @Size(max=50, message = "max size is 50 for mrn_number")
    private String mrnNumber;

    @Column(name = "msn_number")
    private String msnNumber;

    @Column(name = "igm_file_date")
    private LocalDateTime igmFileDate;

    @Column(name = "igm_inward_date")
    private LocalDateTime igmInwardDate;

    @Column(name = "inward_date_and_time")
    private LocalDateTime inwardDateAndTime;

    @Column(name = "igm_file_no")
    @Size(max=10, message = "max size is 10 for igm_file_no")
    private String igmFileNo;

    @Column(name = "smtp_igm_number")
    @Size(max=10, message = "max size is 10 smtp_igm_number")
    private String smtpigmNumber;

    @Column(name = "smtp_igm_date")
    private LocalDateTime smtpigmDate;

    @Column(name = "is_inland")
    private Boolean isInland;

    @Column(name = "original")
    private Integer original;

    @Column(name = "copy")
    private Integer copy;

    @Column(name = "do_place_of_issue")
    private String doPlaceOfIssue;

    @Column(name = "do_issue_date")
    private LocalDateTime doIssueDate;

    @Column(name = "bonded_warehouse_id")
    @DedicatedMasterData(type = Constants.WARE_HOUSE_DATA)
    private Long bondedWarehouseId;

    @Column(name = "warehouse_id")
    @DedicatedMasterData(type = Constants.WARE_HOUSE_DATA)
    private Long warehouseId;

    @Column(name = "source_tenant_id")
    @TenantIdData
    private Long sourceTenantId;

    @Column(name = "edi_transaction_id")
    @Size(max=50, message = "max size is 50 for edi_transaction_id")
    private String ediTransactionId;

    @ElementCollection(fetch = FetchType.LAZY)
    @CollectionTable(name = "triangulation_partner_consolidation", joinColumns = @JoinColumn(name = "consolidation_id"))
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
    private boolean intraBranch;

    @Column(name = "documentation_partner")
    @TenantIdData
    private Long documentationPartner;

    @Column(name = "is_receiving_agent_freetext_address")
    private Boolean isReceivingAgentFreeTextAddress;

    @Column(name = "receiving_agent_freetext_address")
    @Size(max=256, message = "max size is 256 for receiving_agent_freetext_address")
    private String receivingAgentFreeTextAddress;

    @Column(name = "is_sending_agent_freetext_address")
    private Boolean isSendingAgentFreeTextAddress;

    @Column(name = "sending_agent_freetext_address")
    private String sendingAgentFreeTextAddress;

    @Column(name = "place_of_issue")
    @UnlocationData
    private String placeOfIssue;

    @OneToOne(targetEntity = CarrierDetails.class, cascade = CascadeType.ALL, fetch = FetchType.LAZY)
    @JoinColumn(name = "carrier_detail_id", referencedColumnName = "id")
    private CarrierDetails carrierDetails;

    @OneToOne(targetEntity = AchievedQuantities.class, cascade = CascadeType.ALL, fetch = FetchType.LAZY)
    @JoinColumn(name = "achieved_quantities_id", referencedColumnName = "id")
    private AchievedQuantities achievedQuantities;

    @OneToOne(targetEntity = Allocations.class, cascade = CascadeType.ALL, fetch = FetchType.LAZY)
    @JoinColumn(name = "allocations_id", referencedColumnName = "id")
    private Allocations allocations;

    @OneToOne(targetEntity = ArrivalDepartureDetails.class, cascade = CascadeType.ALL, fetch = FetchType.LAZY)
    @JoinColumn(name = "arrival_details_id", referencedColumnName = "id")
    private ArrivalDepartureDetails arrivalDetails;

    @OneToOne(targetEntity = ArrivalDepartureDetails.class, cascade = CascadeType.ALL, fetch = FetchType.LAZY)
    @JoinColumn(name = "departure_details_id", referencedColumnName = "id")
    private ArrivalDepartureDetails departureDetails;

    @MasterData(type = MasterDataType.COUNTRIES)
    @Column(name = "sending_agent_country")
    private String sendingAgentCountry;

    @MasterData(type = MasterDataType.COUNTRIES)
    @Column(name = "receiving_agent_country")
    private String receivingAgentCountry;

    @OneToOne(targetEntity = Parties.class, cascade = CascadeType.ALL, fetch = FetchType.LAZY)
    @JoinColumn(name = "sending_agent_id", referencedColumnName = "id")
    @OrganizationData
    private Parties sendingAgent;

    @OneToOne(targetEntity = Parties.class, cascade = CascadeType.ALL, fetch = FetchType.LAZY)
    @JoinColumn(name = "receiving_agent_id", referencedColumnName = "id")
    @OrganizationData
    private Parties receivingAgent;

    @OneToOne(targetEntity = Parties.class, cascade = CascadeType.ALL, fetch = FetchType.LAZY)
    @JoinColumn(name = "borrowed_from_id", referencedColumnName = "id")
    @OrganizationData
    private Parties borrowedFrom;

    @Column(name = "is_borrowed")
    private Boolean borrowed;

    @OneToOne(targetEntity = Parties.class, cascade = CascadeType.ALL, fetch = FetchType.LAZY)
    @JoinColumn(name = "creditor_id", referencedColumnName = "id")
    @OrganizationData
    private Parties creditor;

    @OneToOne(targetEntity = Parties.class, cascade = CascadeType.ALL, fetch = FetchType.LAZY)
    @JoinColumn(name = "co_load_with_id", referencedColumnName = "id")
    @OrganizationData
    private Parties coLoadWith;

    @Column(name = "co_load_carrier_name")
    @MasterData(type = MasterDataType.CARRIER)
    @Size(max = 64, message = "max size is 64 for coload carrier name")
    private String coLoadCarrierName; // Coloader

    @Column(name = "booking_agent_id")
    @OrganizationMasterData
    private Long bookingAgent; //booking agent

    @OneToMany(fetch = FetchType.LAZY, mappedBy = "consolidationId")
    @BatchSize(size = 50)
    private List<Packing> packingList;

    @OneToMany(fetch = FetchType.LAZY, mappedBy = "consolidationId")
    @BatchSize(size = 50)
    private List<ReferenceNumbers> referenceNumbersList;

    @OneToMany(fetch = FetchType.LAZY, mappedBy = "consolidationId")
    @BatchSize(size = 50)
    @OrderBy("leg ASC")
    private List<Routings> routingsList;

    @OneToMany(fetch = FetchType.LAZY, mappedBy =  "consolidationId")
    @JsonIgnoreProperties(value = "shipmentsList", allowSetters = true)
    @BatchSize(size = 50)
    private List<Containers> containersList;

    @OneToMany(fetch = FetchType.LAZY, mappedBy = "consolidationId")
    @BatchSize(size = 50)
    private List<Jobs> jobsList;

    @OneToMany(fetch = FetchType.LAZY, mappedBy = "consolidationId")
    @BatchSize(size = 50)
    private List<Events> eventsList;

    @OneToMany(fetch = FetchType.LAZY, mappedBy = "entityId")
    @Where(clause = "entity_type = 'CONSOLIDATION'")
    @BatchSize(size = 50)
    private List<FileRepo> fileRepoList;

    @ManyToMany(fetch = FetchType.LAZY)
    @JoinTable(name = "console_shipment_mapping",
            joinColumns = @JoinColumn(name = "consolidation_id"),
            inverseJoinColumns = @JoinColumn(name = "shipment_id"))
    @JsonIgnoreProperties(value = "consolidationList", allowSetters = true)
    @BatchSize(size = 50)
    @WhereJoinTable(clause = "is_attachment_done = 'True'")
    private Set<ShipmentDetails> shipmentsList;

    @OneToMany(fetch = FetchType.LAZY, mappedBy = "entityId")
    @Where(clause = "entity_type = 'CONSOLIDATION_ADDRESSES'")
    @BatchSize(size = 50)
    private List<Parties> consolidationAddresses;

    @Column(name = "carrier_booking_ref")
    @Size(max=64, message = "max size is 64 for carrier_booking_ref")
    private String carrierBookingRef;

    @Column(name = "mode_of_booking")
    @Size(max = 64, message = "max size is 64 for mode_of_booking")
    @MasterData(type = MasterDataType.MODE_OF_BOOKING)
    private String modeOfBooking;

    @Column(name = "auto_update_goods_desc")
    private Boolean autoUpdateGoodsDesc;

    @Column(name = "source_guid")
    private UUID sourceGuid;
    @Column(name = "booking_id")
    private String bookingId;
    @Column(name = "booking_status")
    private String bookingStatus;
    @Column(name = "booking_number")
    private String bookingNumber;

    @Size(max=3, message = "max size is 3 for efreight_status")
    @Column(name = "efreight_status")
    @MasterData(type = MasterDataType.EFREIGHT_STATUS)
    private String efreightStatus;

    @Column(name = "hazardous")
    private Boolean hazardous = false;

    @Column(name = "reefer")
    private Boolean reefer = false;

    @Column(name = "emergency_contact_number")
    @Size(max=31, message = "max size is 31 for emergency_contact_number")
    private String emergencyContactNumber;

    @Column(name = "emergency_contact_number_code")
    @Size(max=31, message = "max size is 31 for emergency_contact_number_code")
    private String emergencyContactNumberCode;

    @Column(name = "screening_status")
    @Size(max=50, message = "max size is 50 for screening_status")
    @ElementCollection(targetClass = String.class, fetch = FetchType.EAGER)
    @CollectionTable(name = "screening_status_consol", joinColumns = @JoinColumn(name = "consolidation_details_id"))
    @BatchSize(size = 10)
    private List<String> screeningStatus;

    @Column(name = "exemption_codes")
    @MasterData(type = MasterDataType.EXEMPTION_CODES)
    private String exemptionCodes;

    @Column(name = "aom_free_text")
    private String aomFreeText;

    @Column(name = "security_status")
    private String securityStatus;

    @Column(name = "sci")
    @MasterData(type = MasterDataType.SCI)
    private String sci;

    @Column(name = "additional_security_information")
    private String additionalSecurityInformation;

    @Column(name = "cfs_cut_off_date")
    private LocalDateTime cfsCutOffDate;

    @Column(name = "open_for_attachment")
    private Boolean openForAttachment;

    @Column(name = "open_for_interbranch_attachment")
    private Boolean interBranchConsole = false;

    @Column(name = "lat_date")
    private LocalDateTime latDate;

    @OneToMany(fetch = FetchType.LAZY, mappedBy = "consolidationId")
    @Where(clause = "is_attachment_done = 'false'")
    @BatchSize(size = 50)
    private List<ConsoleShipmentMapping> consoleShipmentMappings;

    @Column(name = "department")
    @Size(max=32, message = "max size is 32 for department")
    @MasterData(type = MasterDataType.DEPARTMENT_MASTER_LIST)
    private String department;

    @Column(name = "is_network_file")
    private Boolean isNetworkFile;

    @Column(name = "is_receiving_branch_manually")
    private Boolean isReceivingBranchManually;

    @Column(name = "is_transferred_to_receiving_branch")
    private Boolean isTransferredToReceivingBranch;

    @Column(name = "partner")
    @MasterData(type = MasterDataType.ORDER_DPW)
    @Size(max = 64, message = "max size is 64 for partner")
    private String partner;

    @Column(name = "incoterms")
    @MasterData(type = MasterDataType.INCOTERMS)
    private String incoterms;

    @Column(name = "transport_info_status")
    @Enumerated(EnumType.STRING)
    private TransportInfoStatus transportInfoStatus;

    @Column(name = "migration_status")
    @Enumerated(EnumType.STRING)
    private MigrationStatus migrationStatus;

    @Column(name = "trigger_migration_warning")
    private Boolean triggerMigrationWarning = false;

    @Override
    public boolean equals(Object o) {
        if (this == o) return true;
        if (o == null || getClass() != o.getClass()) return false;
        ConsolidationDetails that = (ConsolidationDetails) o;
        return Objects.equals(getId(), that.getId());
    }

    @Override
    public int hashCode() {
        return Objects.hash(getId());
    }

}
