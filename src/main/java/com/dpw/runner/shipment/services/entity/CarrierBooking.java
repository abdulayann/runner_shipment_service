package com.dpw.runner.shipment.services.entity;

import com.dpw.runner.shipment.services.aspects.MultitenancyAspect.MultiTenancy;
import com.dpw.runner.shipment.services.masterdata.enums.MasterDataType;
import com.dpw.runner.shipment.services.utils.MasterData;
import com.dpw.runner.shipment.services.utils.OrganizationData;
import com.dpw.runner.shipment.services.utils.UnlocationData;
import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;
import org.hibernate.annotations.BatchSize;
import org.hibernate.annotations.SQLDelete;
import org.hibernate.annotations.Where;

import javax.persistence.CascadeType;
import javax.persistence.Column;
import javax.persistence.Entity;
import javax.persistence.FetchType;
import javax.persistence.JoinColumn;
import javax.persistence.OneToMany;
import javax.persistence.OneToOne;
import javax.persistence.Table;
import java.time.LocalDateTime;
import java.util.List;

@Entity
@Table(name = "carrier_booking")
@Data
@Builder
@NoArgsConstructor
@AllArgsConstructor
@SQLDelete(sql = "UPDATE carrier_booking SET is_deleted = true WHERE id=?")
@Where(clause = "is_deleted = false")
public class CarrierBooking extends MultiTenancy {

    @Column(name = "status", length = 50)
    private String status;

    @Column(name = "booking_no", length = 100, unique = true)
    private String bookingNo;

    @Column(name = "carrier_booking_no", length = 100)
    private String carrierBookingNo;

    @Column(name = "mbl_no", length = 100)
    private String mblNo;

    @Column(name = "consolidation_no", length = 100)
    private String consolidationNo;

    @Column(name = "service_type", length = 50)
    @MasterData(type = MasterDataType.SERVICE_MODE)
    private String serviceType;

    @UnlocationData
    @Column(name = "booking_office", length = 100)
    private String bookingOffice;

    @Column(name = "booking_comment", columnDefinition = "TEXT")
    private String bookingComment;

    @Column(name = "carrier_comment", columnDefinition = "TEXT")
    private String carrierComment;

    @Column(name = "internal_emails", columnDefinition = "TEXT")
    private String internalEmails;

    @Column(name = "external_emails", columnDefinition = "TEXT")
    private String externalEmails;

    @Column(name = "pickup_from_req_empty_positioning_date")
    private LocalDateTime pickupFromReqEmptyPositioningDate;

    @Column(name = "pickup_from_req_full_pickup_date")
    private LocalDateTime pickupFromReqFullPickupDate;

    @Column(name = "pickup_from_contact_name")
    private String pickupFromContactName;

    @Column(name = "pickup_from_contact_no")
    private String pickupFromContactNo;

    @Column(name = "delivery_to_req_empty_positioning_date")
    private LocalDateTime deliveryToReqEmptyPositioningDate;

    @Column(name = "delivery_to_req_full_pickup_date")
    private LocalDateTime deliveryToReqFullPickupDate;

    @Column(name = "delivery_to_contact_name")
    private String deliveryToContactName;

    @Column(name = "delivery_to_contact_no")
    private String deliveryToContactNo;

    @OneToOne(fetch = FetchType.LAZY, targetEntity = Parties.class, cascade = CascadeType.ALL)
    @JoinColumn(name = "requester_id", referencedColumnName = "id")
    @OrganizationData
    private Parties requester;

    @OneToOne(fetch = FetchType.LAZY, targetEntity = Parties.class, cascade = CascadeType.ALL)
    @JoinColumn(name = "shipper_id", referencedColumnName = "id")
    @OrganizationData
    private Parties shipper;

    @OneToOne(fetch = FetchType.LAZY, targetEntity = Parties.class, cascade = CascadeType.ALL)
    @JoinColumn(name = "consignee_id", referencedColumnName = "id")
    @OrganizationData
    private Parties consignee;

    @OneToOne(fetch = FetchType.LAZY, targetEntity = Parties.class, cascade = CascadeType.ALL)
    @JoinColumn(name = "forwarding_agent_id", referencedColumnName = "id")
    @OrganizationData
    private Parties forwardingAgent;

    @OneToMany(fetch = FetchType.LAZY, mappedBy = "entityId")
    @Where(clause = "entity_type = 'CARRIER_BOOKING_ADDITIONAL_PARTIES'")
    @BatchSize(size = 50)
    private List<Parties> additionalParties;

    @OneToOne(fetch = FetchType.LAZY, targetEntity = Parties.class, cascade = CascadeType.ALL)
    @JoinColumn(name = "pickup_from_id", referencedColumnName = "id")
    @OrganizationData
    private Parties pickupFrom;

    @OneToOne(fetch = FetchType.LAZY, targetEntity = Parties.class, cascade = CascadeType.ALL)
    @JoinColumn(name = "deliver_to_id", referencedColumnName = "id")
    @OrganizationData
    private Parties deliveryTo;

    @OneToOne(fetch = FetchType.LAZY, cascade = CascadeType.ALL)
    @JoinColumn(name = "shipping_instruction_id", referencedColumnName = "id")
    private ShippingInstruction shippingInstruction;

    @OneToOne(fetch = FetchType.LAZY, cascade = CascadeType.ALL)
    @JoinColumn(name = "sailing_information_id", referencedColumnName = "id")
    private SailingInformation sailingInformation;

    @OneToMany(fetch = FetchType.LAZY, cascade = CascadeType.ALL, mappedBy = "carrierBookingId")
    private List<CommonContainers> containersList;

    @OneToMany(fetch = FetchType.LAZY, cascade = CascadeType.ALL, mappedBy = "carrierBookingId")
    private List<CarrierRouting> carrierRoutingList;

    @OneToMany(fetch = FetchType.LAZY, cascade = CascadeType.ALL, mappedBy = "carrierBookingId")
    private List<ReferenceNumbers> referenceNumbersList;
}
