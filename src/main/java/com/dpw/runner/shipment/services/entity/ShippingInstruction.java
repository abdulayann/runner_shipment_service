package com.dpw.runner.shipment.services.entity;

import com.dpw.runner.shipment.services.aspects.MultitenancyAspect.MultiTenancy;
import com.dpw.runner.shipment.services.entity.enums.EntityType;
import com.dpw.runner.shipment.services.entity.enums.ShippingInstructionStatus;
import com.dpw.runner.shipment.services.entity.enums.ShippingInstructionType;
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

import jakarta.persistence.CascadeType;
import jakarta.persistence.Column;
import jakarta.persistence.Entity;
import jakarta.persistence.EnumType;
import jakarta.persistence.Enumerated;
import jakarta.persistence.FetchType;
import jakarta.persistence.JoinColumn;
import jakarta.persistence.OneToMany;
import jakarta.persistence.OneToOne;
import jakarta.persistence.Table;
import java.math.BigDecimal;
import java.time.LocalDateTime;
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
    @Enumerated(EnumType.STRING)
    private ShippingInstructionStatus status;

    @Column(name = "shipping_instruction_type")
    @Enumerated(EnumType.STRING)
    private ShippingInstructionType shippingInstructionType;

    @Column(name = "service_type", length = 20)
    @MasterData(type = MasterDataType.SERVICE_MODE)
    private String serviceType;

    @Column(name = "shipper_declared_value", length = 20)
    private BigDecimal shipperDeclaredValue;

    @Column(name = "shipper_declared_value_currency", length = 20)
    private String shipperDeclaredValueCurrency;

    @UnlocationData
    @Column(name = "bl_release_office", length = 100)
    private String blReleaseOffice;

    @Column(name = "carrier_bl_no", length = 100)
    private String carrierBlNo;

    @Column(name = "carrier_booking_no", length = 100)
    private String carrierBookingNo;

    @Column(name = "entity_type", length = 50)
    @Enumerated(EnumType.STRING)
    private EntityType entityType;

    @Column(name = "entity_id")
    private Long entityId;

    @Column(name = "entity_number", length = 100)
    private String entityNumber;

    @Column(name = "bl_comments", columnDefinition = "TEXT")
    private String blComments;

    @Column(name = "date_of_issue")
    private LocalDateTime dateOfIssue;

    @Column(name = "no_of_freight_copies")
    private Integer noOfFreightCopies;

    @Column(name = "no_of_un_freight_copies")
    private Integer noOfUnFreightCopies;

    @Column(name = "non_nego_freight_copies")
    private Integer nonNegoFreightCopies;

    @Column(name = "non_nego_un_freight_copies")
    private Integer nonNegoUnFreightCopies;

    @Column(name = "internal_emails", columnDefinition = "TEXT")
    private String internalEmails;

    @Column(name = "external_emails", columnDefinition = "TEXT")
    private String externalEmails;

    @Column(name = "other_internal_emails", columnDefinition = "TEXT")
    private String otherInternalEmails;

    @Column(name = "other_external_emails", columnDefinition = "TEXT")
    private String otherExternalEmails;

    @Column(name = "created_by_user_email")
    private String createByUserEmail;

    @Column(name = "submit_by_user_email")
    private String submitByUserEmail;

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

    @OneToOne(fetch = FetchType.LAZY, targetEntity = Parties.class, cascade = CascadeType.ALL)
    @JoinColumn(name = "requestor_id", referencedColumnName = "id")
    @OrganizationData
    private Parties requestor;

    @OneToOne(fetch = FetchType.LAZY, targetEntity = Parties.class, cascade = CascadeType.ALL)
    @JoinColumn(name = "notify_party_id", referencedColumnName = "id")
    @OrganizationData
    private Parties notifyParty;

    @OneToMany(fetch = FetchType.LAZY, mappedBy = "entityId")
    @Where(clause = "entity_type = 'SHIPPING_INSTRUCTION_ADDITIONAL_PARTIES'")
    @BatchSize(size = 50)
    private List<Parties> additionalParties;

    @OneToMany(fetch = FetchType.LAZY, cascade = CascadeType.ALL)
    @JoinColumn(name = "shipping_instruction_id")
    private List<FreightDetail> freightDetailList;

    @OneToMany(fetch = FetchType.LAZY, cascade = CascadeType.ALL)
    @JoinColumn(name = "shipping_instruction_id")
    private List<CommonPackages> commonPackagesList;

    @OneToMany(fetch = FetchType.LAZY, cascade = CascadeType.ALL)
    @JoinColumn(name = "shipping_instruction_id")
    private List<CommonContainers> commonContainersList;

    @OneToMany(fetch = FetchType.LAZY, cascade = CascadeType.ALL)
    @JoinColumn(name = "shipping_instruction_id")
    private List<ReferenceNumbers> referenceNumbers;

    @OneToOne(fetch = FetchType.LAZY, cascade = CascadeType.ALL)
    @JoinColumn(name = "sailing_information_id", referencedColumnName = "id")
    private SailingInformation sailingInformation;

    @Column(name = "si_payload")
    private String payloadJson;

    @Column(name = "si_comments")
    private String comments;
}
