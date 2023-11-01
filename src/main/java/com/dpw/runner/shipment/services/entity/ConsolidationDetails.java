package com.dpw.runner.shipment.services.entity;

import com.dpw.runner.shipment.services.aspects.MultitenancyAspect.MultiTenancy;
import com.dpw.runner.shipment.services.commons.constants.Constants;
import com.dpw.runner.shipment.services.masterdata.enums.MasterDataType;
import com.dpw.runner.shipment.services.utils.MasterData;
import com.dpw.runner.shipment.services.utils.OrganizationData;
import com.dpw.runner.shipment.services.utils.UnlocationData;
import com.fasterxml.jackson.annotation.JsonIgnoreProperties;
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
@Table(name = "consolidation_details")
@Getter
@Setter
@Data
@Builder
@NoArgsConstructor
@AllArgsConstructor
@Accessors(chain = true)
@SQLDelete(sql = "UPDATE containers SET is_deleted = true WHERE id=?")
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

    @Column(name = "first_load")
    @UnlocationData
    private String firstLoad;

    @Column(name = "last_discharge")
    @UnlocationData
    private String lastDischarge;

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
    private String coLoadMBL;

    @Column(name = "co_load_booking_reference")
    @Size(max=64, message = "max size is 64 for co_load_booking_reference")
    private String coLoadBookingReference;

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
    private LocalDateTime hazardousBookingCutoff;

    @Column(name = "volume_utilization")
    private String volumeUtilization;

    @Column(name = "weight_utilization")
    private String weightUtilization;

    @Column(name = "shipment_type")
    @MasterData(type = MasterDataType.CUSTOM_SHIPMENT_TYPE)
    private String shipmentType;

    @Column(name = "bol")
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
    private Long bondedWarehouseId;

    @Column(name = "warehouse_id")
    private Long warehouseId;

    @Column(name = "source_tenant_id")
    private long sourceTenantId;

    @Column(name = "edi_transaction_id")
    @Size(max=50, message = "max size is 50 for edi_transaction_id")
    private String ediTransactionId;

    @Column(name = "triangulation_partner")
    private Long triangulationPartner;

    @Column(name = "receiving_branch")
    private Long receivingBranch;

    @Column(name = "intra_branch")
    private boolean intraBranch;

    @Column(name = "documentation_partner")
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

    @OneToOne(targetEntity = CarrierDetails.class, cascade = CascadeType.ALL)
    @JoinColumn(name = "carrier_detail_id", referencedColumnName = "id")
    private CarrierDetails carrierDetails;

    @OneToOne(targetEntity = AchievedQuantities.class, cascade = CascadeType.ALL)
    @JoinColumn(name = "achieved_quantities_id", referencedColumnName = "id")
    private AchievedQuantities achievedQuantities;

    @OneToOne(targetEntity = Allocations.class, cascade = CascadeType.ALL)
    @JoinColumn(name = "allocations_id", referencedColumnName = "id")
    private Allocations allocations;

    @OneToOne(targetEntity = ArrivalDepartureDetails.class, cascade = CascadeType.ALL)
    @JoinColumn(name = "arrival_details_id", referencedColumnName = "id")
    private ArrivalDepartureDetails arrivalDetails;

    @OneToOne(targetEntity = ArrivalDepartureDetails.class, cascade = CascadeType.ALL)
    @JoinColumn(name = "departure_details_id", referencedColumnName = "id")
    private ArrivalDepartureDetails departureDetails;

    @OneToOne(targetEntity = Parties.class, cascade = CascadeType.ALL)
    @JoinColumn(name = "sending_agent_id", referencedColumnName = "id")
    @OrganizationData
    private Parties sendingAgent;

    @OneToOne(targetEntity = Parties.class, cascade = CascadeType.ALL)
    @JoinColumn(name = "receiving_agent_id", referencedColumnName = "id")
    @OrganizationData
    private Parties receivingAgent;

    @OneToOne(targetEntity = Parties.class, cascade = CascadeType.ALL)
    @JoinColumn(name = "borrowed_from_id", referencedColumnName = "id")
    @OrganizationData
    private Parties borrowedFrom;

    @OneToOne(targetEntity = Parties.class, cascade = CascadeType.ALL)
    @JoinColumn(name = "creditor_id", referencedColumnName = "id")
    @OrganizationData
    private Parties creditor;

    @OneToOne(targetEntity = Parties.class, cascade = CascadeType.ALL)
    @JoinColumn(name = "co_load_with_id", referencedColumnName = "id")
    @OrganizationData
    private Parties coLoadWith;

    @OneToMany(fetch = FetchType.LAZY, mappedBy = "consolidationId")
    @BatchSize(size = 50)
    private List<Packing> packingList;

    @OneToMany(fetch = FetchType.LAZY, mappedBy = "consolidationId")
    @BatchSize(size = 50)
    private List<ReferenceNumbers> referenceNumbersList;

    @OneToMany(fetch = FetchType.LAZY, mappedBy = "consolidationId")
    @BatchSize(size = 50)
    private List<Routings> routingsList;

    @OneToMany(fetch = FetchType.LAZY, mappedBy =  "consolidationId")
    @JsonIgnoreProperties("shipmentsList")
    @BatchSize(size = 50)
    private List<Containers> containersList;

    @OneToMany(fetch = FetchType.LAZY, mappedBy = "consolidationId")
    @BatchSize(size = 50)
    private List<TruckDriverDetails> truckDriverDetails;

    @OneToMany(fetch = FetchType.LAZY, mappedBy = "consolidationId")
    @BatchSize(size = 50)
    private List<Jobs> jobsList;

    @OneToMany(fetch = FetchType.LAZY, mappedBy = "entityId")
    @Where(clause = "entity_type = 'CONSOLIDATION'")
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
    @JsonIgnoreProperties("consolidationList")
    @BatchSize(size = 50)
    private List<ShipmentDetails> shipmentsList;

    @OneToMany(fetch = FetchType.LAZY, mappedBy = "entityId")
    @Where(clause = "entity_type = 'CONSOLIDATION_ADDRESSES'")
    @BatchSize(size = 50)
    private List<Parties> consolidationAddresses;
}
