package com.dpw.runner.shipment.services.entity;

import com.dpw.runner.shipment.services.aspects.MultitenancyAspect.MultiTenancy;
import com.dpw.runner.shipment.services.utils.OrganizationData;
import lombok.*;
import lombok.experimental.Accessors;
import org.hibernate.annotations.BatchSize;
import org.hibernate.annotations.SQLDelete;
import org.hibernate.annotations.Where;

import javax.persistence.*;
import javax.validation.constraints.Size;
import java.time.LocalDateTime;
import java.util.List;
import java.util.UUID;

@Entity
@Data
@Table(name = "evgm")
@Accessors(chain = true)
@ToString(onlyExplicitlyIncluded = true)
@NoArgsConstructor
@AllArgsConstructor
@Builder
@SQLDelete(sql = "UPDATE evgm SET is_deleted = true WHERE id=?")
@Where(clause = "is_deleted = false")
public class EVgm extends MultiTenancy {

    @Column(name = "shipment_guid")
    private UUID shipmentGuid; // TODO: confirm if need to keep it?

    @Column(name="consolidation_guid")
    private UUID consolidationGuid; // relation or not based on pavan

    @Column(name = "booking_id")
    private Long bookingId;

    @Column(name = "shipping_instruction_id")
    private Long shippingInstructionId; // TODO: confirm if need to keep it?

    @Column(name = "container_id")
    private Long containerId; // TODO: confirm if need to keep it? container ka data required in evgm or not?

    @Column(name = "status")
    private String status;

    @Column(name = "carrier")
    @Size(max = 255, message = "max size is 255 for carrier")
    private String carrier;

    @OneToOne(targetEntity = Parties.class, cascade = CascadeType.ALL, fetch = FetchType.LAZY)
    @JoinColumn(name = "responsible_party_id", referencedColumnName = "id")
    @OrganizationData
    private Parties responsibleParty;

    @OneToOne(targetEntity = Parties.class, cascade = CascadeType.ALL, fetch = FetchType.LAZY)
    @JoinColumn(name = "authorized_party_id", referencedColumnName = "id")
    @OrganizationData
    private Parties authorizedParty;

    @Column(name = "approval_signature")
    @Size(max = 255, message = "max size is 255 for approval signature")
    private String approvalSignature;

    @Column(name = "approval_date_time")
    private LocalDateTime approvalDateTime;

    @Column(name = "vgm_determination_date_time")
    private LocalDateTime vgmDeterminationDateTime;

    @Column(name = "delegate")
    private Boolean delegate;

    @Column(name = "weight_method")
    private Integer weightMethod;

    @OneToMany(fetch = FetchType.LAZY, mappedBy = "entityId")
    @Where(clause = "event_code = 'CarrierBookingEvents'")
    @BatchSize(size = 50)
    private List<Events> eventsList; // confirm with abhishek how to check the event code thing
}
