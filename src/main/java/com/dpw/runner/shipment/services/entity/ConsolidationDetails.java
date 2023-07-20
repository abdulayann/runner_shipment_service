package com.dpw.runner.shipment.services.entity;

import com.dpw.runner.shipment.services.aspects.MultitenancyAspect.MultiTenancy;
import lombok.*;
import lombok.experimental.Accessors;
import org.hibernate.annotations.SQLDelete;
import org.hibernate.annotations.Where;

import javax.persistence.*;
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
    private String consolidationNumber;

    @Column(name = "consolidation_type")
    private String consolidationType;

    @Column(name = "transport_mode")
    private String transportMode;

    @Column(name = "container_category")
    private String containerCategory;

    @Column(name = "is_domestic")
    private Boolean isDomestic;

    @Column(name = "mawb")
    private String MAWB;

    @Column(name = "service_level")
    private String serviceLevel;

    @Column(name = "payment")
    private String payment;

    @Column(name = "first_load")
    private String firstLoad;

    @Column(name = "last_discharge")
    private String lastDischarge;

    @Column(name = "booking_type")
    private String bookingType;

    @Column(name = "declaration_type")
    private String declarationType;

    @Column(name = "delivery_mode")
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
    private String agentReference;

    @Column(name = "co_load_mbl")
    private String coLoadMBL;

    @Column(name = "co_load_booking_reference")
    private String coLoadBookingReference;

    @Column(name = "manifest_print")
    private String manifestPrint;

    @Column(name = "print_other_docs")
    private String printOtherDocs;

    @Column(name = "awb_dims")
    private String awbDims;

    @Column(name = "release_type")
    private String releaseType;

    @Column(name = "masterbill_issue_date")
    private LocalDateTime masterBillIssueDate;

    @Column(name = "override")
    private Boolean override;

    @Column(name = "estimated_terminal_cutoff")
    private LocalDateTime EstimatedTerminalCutoff;

    @Column(name = "terminal_cutoff")
    private LocalDateTime TerminalCutoff;

    @Column(name = "verified_gross_mass_cutoff")
    private LocalDateTime VerifiedGrossMassCutoff;

    @Column(name = "reefer_cutoff")
    private LocalDateTime ReeferCutoff;

    @Column(name = "booking_cutoff")
    private LocalDateTime BookingCutoff;

    @Column(name = "ship_instruction_cutoff")
    private LocalDateTime ShipInstructionCutoff;

    @Column(name = "hazardous_booking_cutoff")
    private LocalDateTime HazardousBookingCutoff;

    @Column(name = "volume_utilization")
    private String volumeUtilization;

    @Column(name = "weight_utilization")
    private String weightUtilization;

    @Column(name = "shipment_type")
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
    private String MarksnNums;

    @Column(name = "additional_terms")
    private String AdditionalTerms;

    @Column(name = "docs_closing_time")
    private LocalDateTime docsClosingTime;

    @Column(name = "cargo_closing_time")
    private LocalDateTime cargoClosingTime;

    @Column(name = "mrn_number")
    private String mrnNumber;

    @Column(name = "msn_number")
    private String msnNumber;

    @Column(name = "igm_file_date")
    private LocalDateTime IGMFileDate;

    @Column(name = "igm_inward_date")
    private LocalDateTime IGMInwardDate;

    @Column(name = "inward_date_and_time")
    private LocalDateTime inwardDateAndTime;

    @Column(name = "igm_file_no")
    private String igmFileNo;

    @Column(name = "smtp_igm_number")
    private String SMTPIGMNumber;

    @Column(name = "smtp_igm_date")
    private LocalDateTime SMTPIGMDate;

    @Column(name = "is_inland")
    private Boolean isInland;

    @Column(name = "original")
    private Integer original;

    @Column(name = "copy")
    private Integer copy;

    @Column(name = "do_place_of_issue_id")
    private Long DOPlaceOfIssueId;

    @Column(name = "do_issue_date")
    private LocalDateTime DOIssueDate;

    @Column(name = "bonded_warehouse_id")
    private Long bondedWarehouseId;

    @Column(name = "warehouse_id")
    private Long warehouseId;

    @Column(name = "source_tenant_id")
    private long SourceTenantId;

    @Column(name = "edi_transaction_id")
    private String ediTransactionId;

    @Column(name = "triangulation_partner")
    private long triangulationPartner;

    @Column(name = "receiving_branch")
    private long receivingBranch;

    @Column(name = "intra_branch")
    private boolean intraBranch;

    @Column(name = "documentation_partner")
    private long documentationPartner;

    @Column(name = "is_receiving_agent_freetext_address")
    private Boolean isReceivingAgentFreeTextAddress;

    @Column(name = "receiving_agent_freetext_address")
    private String receivingAgentFreeTextAddress;

    @Column(name = "is_sending_agent_freetext_address")
    private Boolean IsSendingAgentFreeTextAddress;

    @Column(name = "sending_agent_freetext_address")
    private String sendingAgentFreeTextAddress;

    @OneToOne(targetEntity = CarrierDetails.class)
    @JoinColumn(name = "carrier_detail_id", referencedColumnName = "id")
    private CarrierDetails carrierDetails;

    @OneToOne(targetEntity = AchievedQuantities.class)
    @JoinColumn(name = "achieved_quantities_id", referencedColumnName = "id")
    private AchievedQuantities achievedQuantities;

    @OneToOne(targetEntity = Allocations.class)
    @JoinColumn(name = "allocations_id", referencedColumnName = "id")
    private Allocations allocations;

    @OneToOne(targetEntity = ArrivalDepartureDetails.class)
    @JoinColumn(name = "arrival_departure_details_id", referencedColumnName = "id")
    private ArrivalDepartureDetails arrivalDepartureDetails;

    @OneToMany(fetch = FetchType.LAZY, mappedBy = "consolidationId")
    private List<Packing> packingList;

    @OneToMany(fetch = FetchType.LAZY, mappedBy = "consolidationId")
    private List<ReferenceNumbers> referenceNumbersList;

    @OneToMany(fetch = FetchType.LAZY, mappedBy = "consolidationId")
    private List<Routings> routingsList;

    @OneToMany(fetch = FetchType.LAZY, mappedBy =  "consolidationId")
    private List<Containers> containersList;

    @OneToMany(fetch = FetchType.LAZY, mappedBy = "consolidationId")
    private List<TruckDriverDetails> truckDriverDetails;

    @OneToMany(fetch = FetchType.LAZY, mappedBy = "consolidationId")
    private List<Jobs> jobsList;

    @OneToMany(fetch = FetchType.LAZY, mappedBy = "consolidationId")
    private List<Events> eventsList;

    @OneToMany(fetch = FetchType.LAZY, mappedBy = "entityId")
    @Where(clause = "entity_type = 'CONSOLIDATION'")
    private List<FileRepo> fileRepoList;
}
