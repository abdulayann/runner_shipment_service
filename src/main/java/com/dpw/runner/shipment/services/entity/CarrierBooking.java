package com.dpw.runner.shipment.services.entity;

import com.dpw.runner.shipment.services.entity.enums.CarrierBookingStatus;
import com.dpw.runner.shipment.services.aspects.MultitenancyAspect.MultiTenancy;
import com.dpw.runner.shipment.services.masterdata.enums.MasterDataType;
import com.dpw.runner.shipment.services.utils.MasterData;
import com.dpw.runner.shipment.services.utils.TenantIdData;
import com.dpw.runner.shipment.services.utils.UnlocationData;
import lombok.AllArgsConstructor;
import lombok.Getter;
import lombok.NoArgsConstructor;
import lombok.Setter;
import org.hibernate.annotations.BatchSize;
import org.hibernate.annotations.Where;

import javax.persistence.*;
import javax.validation.constraints.Size;
import java.time.LocalDateTime;
import java.util.List;
import java.util.UUID;


@Entity
@Getter
@Setter
@Table(name = "carrier_booking")
@AllArgsConstructor
@NoArgsConstructor
public class CarrierBooking extends MultiTenancy {

    @Column(name = "source_tenant_id")
    @TenantIdData
    private Integer sourceTenantId;

    @Column(name = "shipment_guid")
    public UUID shipmentGuid;

//    @OneToOne(cascade = CascadeType.ALL, fetch = FetchType.LAZY, mappedBy = "booking_id")
//    private ShippingInstruction shippingInstruction; // todo: doubt here, one to one map question mapped

    @Column(name = "booking_number")
    @Size(max = 50, message = "max size is 50 for booking number")
    private String bookingNumber;

    @Column(name = "booking_id")
    @Size(max = 50, message = "max size is 50 for booking id")
    private String bookingId;

    @Enumerated(EnumType.STRING)
    @Column(name = "status")
    private CarrierBookingStatus status;

    @Column(name="consolidation_guid")
    private UUID consolidationGuid;

    @Column(name = "is_temp_controlled")
    private boolean isTempControlled;

    @Column(name = "is_split_booking")
    private boolean isSplitBooking;

    @Column(name = "parent_booking_number")
    private String parentBookingNumber;

    @Column(name = "split_booking_sequence")
    private String splitBookingSequence;

    @Column(name = "is_trans_shipment")
    private boolean isTransShipment;

    @OneToOne(targetEntity = CarrierDetails.class, cascade = CascadeType.ALL, fetch = FetchType.LAZY)
    @JoinColumn(name = "carrier_detail_id", referencedColumnName = "id")
    private CarrierDetails carrierDetails;

    @Column(name = "transport_mode")
    @Size(max=4, message = "max size is 4 for transport_mode")
    @MasterData(type = MasterDataType.TRANSPORT_MODE)
    private String transportMode;

    @Column(name = "contract_number")
    @Size(max = 255, message = "max size is 255 for contract number")
    private String contractNumber;

    @Column(name = "booking_office")
    @UnlocationData
    private String bookingOffice;

    @Column(name = "shipper_ref_number")
    @Size(max = 255, message = "max size is 255 for shipper ref number")
    private String shipperRefNumber;

    @Column(name = "forwarder_ref_number")
    @Size(max = 255, message = "max size is 255 for forwarder ref number")
    private String forwarderRefNumber;

    @Column(name = "purchase_order_number")
    @Size(max = 255, message = "max size is 255 for purchase order number")
    private String purchaseOrderNumber;

    @Column(name = "service_type")
    @Size(max=3, message = "max size is 3 for service Type")
    @MasterData(type = MasterDataType.SERVICE_MODE)
    private String serviceType;

    @Column(name = "place_of_carrier_receipt")
    @UnlocationData
    private String placeOfCarrierReceipt;

    @Column(name = "earliest_departure_date")
    private LocalDateTime earliestDepartureDate;

    @Column(name = "place_of_carrier_delivery")
    @UnlocationData
    private String placeOfCarrierDelivery; // TODO: inform abhishek and mayank that i used string here

    @Column(name = "lateset_delivery_date")
    private LocalDateTime latesetDeliveryDate;

    @OneToMany(fetch = FetchType.LAZY, mappedBy = "carrierBookingId")
    private List<ReferenceNumbers> referenceNumbersList;

    @OneToMany(fetch = FetchType.LAZY, mappedBy = "entityId")
    @Where(clause = "entity_type = 'CARRIER_BOOKING_ADDRESSES'")
    @BatchSize(size = 50)
    private List<Parties> consolidationAddresses; // TODO- entity type?

    @OneToMany(fetch = FetchType.LAZY, mappedBy = "carrierBookingId")
    private List<BookingCarriage> bookingCarriagesList;

    @OneToMany(fetch = FetchType.LAZY, mappedBy = "carrierBookingId")
    private List<BookingPayment> bookingPaymentsList;

    @OneToMany(fetch = FetchType.LAZY, mappedBy = "entityId")
    @Where(clause = "entity_type = 'CARRIER_BOOKING'")
    @BatchSize(size = 50)
    private List<Events> eventsList; // TODO- entity type?

    @Column(name = "is_override")
    private boolean isOverride;

    @Column(name = "is_linked")
    private boolean isLinked;

    @OneToMany(fetch = FetchType.LAZY, mappedBy = "carrierBookingId")
    private List<Containers> containersList;

    @OneToMany(fetch = FetchType.LAZY, mappedBy = "carrierBookingId")
    private List<Packing> packingList;

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

    @Column(name = "latest_full_equ_delivered_to_carrier")
    private LocalDateTime latestFullEquDeliveredToCarrier;

    @Column(name = "earliest_drop_off_full_equ_to_carrier")
    private LocalDateTime earliestDropOffFullEquToCarrier;

    @Column(name = "earliest_empty_equ_pick_up")
    private LocalDateTime earliestEmptyEquPickUp;

    @Column(name = "requested_transshipment")
    @UnlocationData
    private String requestedTransshipment;

    @Column(name = "prohibited_transshipment")
    @UnlocationData
    private String prohibitedTransshipment;

    @Column(name = "carrier_comments")
    private String carrierComments;

    @Column(name = "customer_comments")
    private String customerComments;

    @Column(name = "reference_no")
    private String referenceNo;

    @Column(name = "locked_by")
    private Integer lockedBy;

    @Column(name = "is_locked")
    private boolean isLocked;

//    @OneToMany(fetch = FetchType.LAZY, mappedBy = "carrierBookingId")
//    private List<OrderInfo> orderIds; // TODO: doubt here

    @Column(name = "tenant_department_id")
    private Integer tenantDepartmentId; // TODO: doubt here

    @Column(name = "order_id")
    private String orderId;

    @Column(name = "mode_of_booking")
    private String modeOfBooking;

    @Column(name = "bol")
    @Size(max = 20, message = "max size is 20 for bol")
    private String bol;

}
