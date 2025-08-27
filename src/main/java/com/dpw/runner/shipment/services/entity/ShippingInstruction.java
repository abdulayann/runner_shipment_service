package com.dpw.runner.shipment.services.entity;

import com.dpw.runner.shipment.services.aspects.MultitenancyAspect.MultiTenancy;
import com.dpw.runner.shipment.services.utils.OrganizationData;
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
import java.util.List;

@Entity
@Table(name = "shipping_instruction")
@Data
@Builder
@NoArgsConstructor
@AllArgsConstructor
@SQLDelete(sql = "UPDATE shipping_instruction SET is_deleted = true WHERE id=?")
@Where(clause = "is_deleted = false")
public class ShippingInstruction extends MultiTenancy {

    @Column(name = "status", length = 20)
    private String status;

    @OneToOne(fetch = FetchType.LAZY, targetEntity = Parties.class, cascade = CascadeType.ALL)
    @JoinColumn(name = "contract_id", referencedColumnName = "id")
    @OrganizationData
    private Parties contract;

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
    @Where(clause = "entity_type = 'SHIPPING_INSTRUCTION_ADDITIONAL_PARTIES'")
    @BatchSize(size = 50)
    private List<Parties> additionalParties;

    @Column(name = "mbl_no", length = 100)
    private String mblNo;

    @Column(name = "carrier_booking_no", length = 100)
    private String carrierBookingNo;

    @Column(name = "entity_type", length = 50)
    private String entityType;

    @Column(name = "entity_id")
    private Long entityId;

    @Column(name = "entity_number", length = 100)
    private String entityNumber;

    @Column(name = "bl_comments", columnDefinition = "TEXT")
    private String blComments;

    @OneToMany(fetch = FetchType.LAZY, mappedBy = "shippingInstructionId")
    private List<FreightDetail> freightDetailList;

    @OneToMany(fetch = FetchType.LAZY, mappedBy = "shippingInstructionId")
    private List<CommonPackages> commonPackagesList;

    @OneToMany(fetch = FetchType.LAZY, mappedBy = "shippingInstructionId")
    private List<CommonContainers> commonContainersList;
}
